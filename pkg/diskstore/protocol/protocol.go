package protocol

import (
	"context"
	"errors"
	"fmt"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/server"

	"zenhack.net/go/ocap-md/pkg/diskstore"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

var (
	ErrNotRef = errors.New("Capability is not a (server hosted) ref")
)

type storageServer struct {
	store *diskstore.DiskStore
}

type refServer struct {
	store *diskstore.DiskStore
	ref   diskstore.Ref
}

type rootApiServer struct {
	storage protocol.Storage
	root    protocol.RootPtr
}

type rootPtrServer struct {
	store   *diskstore.DiskStore
	version uint64
}

func getRefServer(ctx context.Context, ref protocol.Ref) (*refServer, error) {
	if err := ref.Client.Resolve(ctx); err != nil {
		return nil, err
	}
	brand := ref.Client.State().Brand
	if err, ok := brand.Value.(error); ok {
		return nil, fmt.Errorf("getRefServer: ref is an error: %w\n", err)
	}

	srv, ok := server.IsServer(brand)
	if !ok {
		return nil, ErrNotRef
	}
	refSrv, ok := srv.(*refServer)
	if !ok {
		return nil, ErrNotRef
	}
	return refSrv, nil
}

func (s storageServer) Put(ctx context.Context, p protocol.Storage_put) error {
	p.Ack()

	args := p.Args()

	stored, err := protocol.NewStored(args.Struct.Segment())
	if err != nil {
		return err
	}

	v, err := args.Value()
	if err != nil {
		return err
	}

	stored.SetData(v)

	caps := args.Message().CapTable

	refs, err := stored.NewRefs(int32(len(caps)))
	if err != nil {
		return err
	}

	for i, c := range caps {
		ref, err := getRefServer(ctx, protocol.Ref{c})
		if err != nil {
			return err
		}
		ref.ref.Hash().ToContentId(refs.At(i))
	}

	data, err := capnp.Canonicalize(stored.Struct)
	if err != nil {
		return err
	}

	diskRef, err := s.store.Put(data)
	if err != nil {
		return err
	}
	refClient := protocol.Ref_ServerToClient(&refServer{
		store: s.store,
		ref:   diskRef,
	})

	res, err := p.AllocResults()
	if err != nil {
		return err
	}
	res.SetRef(refClient)
	return nil
}

func (r *refServer) Get(ctx context.Context, p protocol.Getter_get) error {
	p.Ack()
	stored, err := r.getStored()
	if err != nil {
		return err
	}
	value, err := stored.Data()
	if err != nil {
		return err
	}
	res, err := p.AllocResults()
	if err != nil {
		return err
	}
	res.SetValue(value)
	return nil
}

func (r *refServer) GetStored(ctx context.Context, p protocol.Ref_getStored) error {
	p.Ack()
	stored, err := r.getStored()
	if err != nil {
		return err
	}
	res, err := p.AllocResults()
	if err != nil {
		return err
	}
	res.SetStoredValue(stored)
	return nil
}

func (r *refServer) getStored() (protocol.Stored, error) {
	data, err := r.ref.Get()
	if err != nil {
		return protocol.Stored{}, err
	}

	stored, err := protocol.ReadRootStored(&capnp.Message{Arena: capnp.SingleSegment(data)})
	if err != nil {
		return protocol.Stored{}, err
	}
	msg := stored.Struct.Message()
	refs, err := stored.Refs()
	if err != nil {
		return protocol.Stored{}, err
	}

	// Attach capabilities for outgoing poitners:
	caps := msg.CapTable
	for i := 0; i < refs.Len(); i++ {
		hash, err := diskstore.ContentIdHash(refs.At(i))
		if err != nil {
			continue
		}
		ref, err := r.store.ResolveHash(hash)
		if err != nil {
			continue
		}
		refClient := protocol.Ref_ServerToClient(&refServer{
			store: r.store,
			ref:   ref,
		})
		caps = append(caps, refClient.Client)
	}
	msg.CapTable = caps

	return stored, nil
}

func (s rootApiServer) Storage(ctx context.Context, p protocol.RootApi_storage) error {
	res, err := p.AllocResults()
	if err != nil {
		return err
	}
	res.SetStorage(s.storage.AddRef())
	return nil
}

func (s rootApiServer) Root(ctx context.Context, p protocol.RootApi_root) error {
	res, err := p.AllocResults()
	if err != nil {
		return err
	}
	res.SetRoot(s.root.AddRef())
	return nil
}

func (s rootApiServer) BlobMap(ctx context.Context, p protocol.RootApi_blobMap) error {
	panic("TODO")
}

func (s *rootPtrServer) Get(ctx context.Context, p protocol.Getter_get) error {
	ref, err := s.store.GetRoot()
	if err != nil {
		return err
	}
	res, err := p.AllocResults()
	if err != nil {
		return err
	}
	refClient := protocol.Ref_ServerToClient(&refServer{
		store: s.store,
		ref:   ref,
	})
	res.SetValue(capnp.NewInterface(
		res.Struct.Segment(),
		res.Struct.Message().AddCap(refClient.Client),
	).ToPtr())
	return nil
}

func (s *rootPtrServer) Set(ctx context.Context, p protocol.Setter_set) error {
	v, err := p.Args().Value()
	if err != nil {
		return err
	}
	refSrv, err := getRefServer(ctx, protocol.Ref{v.Interface().Client()})
	if err != nil {
		return err
	}
	err = s.store.SetRoot(refSrv.ref)
	if err != nil {
		return err
	}
	s.version++
	return nil
}

func (s *rootPtrServer) TxGet(ctx context.Context, p protocol.TxCell_txGet) error {
	panic("TODO")
}

func newRootApi(s *diskstore.DiskStore) rootApiServer {
	return rootApiServer{
		storage: protocol.Storage_ServerToClient(storageServer{store: s}),
		root:    protocol.RootPtr_ServerToClient(&rootPtrServer{store: s, version: 0}),
	}
}

func NewRootApi(s *diskstore.DiskStore) protocol.RootApi {
	return protocol.RootApi_ServerToClient(newRootApi(s))
}
