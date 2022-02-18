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

func (node blobNode) encode(bt files.BlobTree) {
	if node.isLeaf {
		bt.SetLeaf(node.ref)
	} else {
		bt.SetBranch(node.ref)
	}
	bt.SetSize(node.size)
}

func writeBranches(ctx context.Context, s protocol.Storage, nodes []blobNode) (blobNode, error) {
	var result blobNode
	res, rel := s.Put(ctx, func(p protocol.Storage_put_Params) error {
		branches, err := files.NewBlobTree_List(p.Struct.Segment(), int32(len(nodes)))
		if err != nil {
			return err
		}
		for i, node := range nodes {
			result.size += node.size
			node.encode(branches.At(i))
		}
		p.SetValue(branches.ToPtr())
		return nil
	})
	defer func() {
		go rel()
	}()
	result.ref = res.Ref().AddRef()
	return result, nil
}

// WriteStream constructs a BlobTree in the storage from the contents of r,
// and encodes its root node in bt.
func WriteStream(ctx context.Context, s protocol.Storage, r io.Reader, bt files.BlobTree) error {
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
		defer func() {
			go rel()
		}()
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
		return err
	}
	err = sp.Close()
	if err != nil {
		return err
	}
	rootNode, err := writeBranches(ctx, s, nodes)
	if err != nil {
		return err
	}
	rootNode.encode(bt)
	return nil
}
