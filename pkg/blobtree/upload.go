package blobtree

import (
	"context"
	"io"
	"log"

	"zenhack.net/go/ocap-md/pkg/schema/files"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"

	"capnproto.org/go/capnp/v3"

	"github.com/bobg/hashsplit"
)

type blobNode struct {
	size   uint64
	isLeaf bool
	ref    protocol.Ref
}

func writeBranches(ctx context.Context, s protocol.Storage, nodes []blobNode) (blobNode, error) {
	var result blobNode
	res, rel := s.Put(ctx, func(p protocol.Storage_put_Params) error {
		branches, err := files.NewBlobTree_List(p.Struct.Segment(), int32(len(nodes)))
		if err != nil {
			return err
		}
		totalSize := uint64(0)
		for i, node := range nodes {
			totalSize += node.size
			branch := branches.At(i)
			if node.isLeaf {
				branch.SetLeaf(node.ref)
			} else {
				branch.SetBranch(node.ref)
			}
			branch.SetSize(node.size)
		}
		result = blobNode{
			size:   totalSize,
			isLeaf: false,
			// We fill in ref below.
		}
		return nil
	})
	defer rel()
	result.ref = res.Ref().AddRef()
	return result, nil
}

// WriteStream constructs a Ref(BlobTree) inthe storage, from the contents of r.
func WriteStream(ctx context.Context, s protocol.Storage, r io.Reader) (protocol.Ref, error) {
	var nodes []blobNode
	defer func() {
		for _, node := range nodes {
			node.ref.Release()
		}
	}()

	sp := hashsplit.NewSplitter(func(b []byte, level uint) error {
		if ctx.Err() != nil {
			return ctx.Err()
		}

		// TODO: do something with level other than print it.
		log.Print("Level: ", level)

		res, rel := s.Put(ctx, func(p protocol.Storage_put_Params) error {
			list, err := capnp.NewData(p.Struct.Segment(), b)
			if err != nil {
				return err
			}
			p.SetValue(list.ToPtr())
			return nil
		})
		defer rel()
		nodes = append(nodes, blobNode{
			size:   uint64(len(b)),
			isLeaf: true,
			ref:    res.Ref().AddRef(),
		})
		return nil
	})
	sp.MinSize = 32 * 1024
	_, err := io.Copy(sp, r)
	if err != nil {
		return protocol.Ref{}, err
	}
	rootNode, err := writeBranches(ctx, s, nodes)
	if err != nil {
		return protocol.Ref{}, err
	}
	return rootNode.ref, nil
}
