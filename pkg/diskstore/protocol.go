package diskstore

import (
	"context"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/server"

	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

type storageServer struct {
	store *DiskStore
}

type refServer struct {
	store *DiskStore
	hash  Hash
}

type rootApiServer struct {
	storage protocol.Storage
	root    protocol.RootPtr
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
		if err = c.Resolve(ctx); err != nil {
			return err
		}
		srv, ok := server.IsServer(c.State().Brand)
		if !ok {
			continue
		}
		ref, ok := srv.(*refServer)
		if !ok {
			continue
		}
		ref.hash.ToContentId(refs.At(i))
	}

	data, err := capnp.Canonicalize(stored.Struct)
	if err != nil {
		return err
	}

	hash, _, err := s.store.Put(data)
	if err != nil {
		return err
	}
	refClient := protocol.Ref_ServerToClient(&refServer{
		store: s.store,
		hash:  hash,
	}, nil)

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
	data, err := r.store.Get(&r.hash)
	if err != nil {
		return protocol.Stored{}, err
	}
	// TODO: verify the hash.
	stored, err := protocol.ReadRootStored(&capnp.Message{Arena: capnp.SingleSegment(data)})
	if err != nil {
		return protocol.Stored{}, err
	}
	msg := stored.Struct.Message()
	refs, err := stored.Refs()
	if err != nil {
		return protocol.Stored{}, err
	}
	caps := msg.CapTable
	for i := 0; i < refs.Len(); i++ {
		// FIXME: attach caps to message.
	}
	msg.CapTable = caps
	return stored, nil
}

func (s rootApiServer) Storage(ctx context.Context, p protocol.RootApi_storage) error {
	res, err := p.AllocResults()
	if err != nil {
		return err
	}
	res.SetStorage(s.storage)
	return nil
}

func (s rootApiServer) Root(ctx context.Context, p protocol.RootApi_root) error {
	res, err := p.AllocResults()
	if err != nil {
		return err
	}
	res.SetRoot(s.root)
	return nil
}

func (s rootApiServer) BlobMap(ctx context.Context, p protocol.RootApi_blobMap) error {
	panic("TODO")
}
