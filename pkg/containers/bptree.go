package containers

import (
	"context"
	"io"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/schema/containers"
)

func IterBPlusTree(bt containers.BPlusTree) (*BPlusTreeIterator, error) {
	root, err := bt.Root()
	if err != nil {
		return nil, err
	}
	branches, err := root.Branches()
	if err != nil {
		return nil, err
	}
	return &BPlusTreeIterator{
		parents: nil,
		current: bpTreeLevelIterator{
			i:        0,
			branches: branches,
			rel:      nil,
		},
	}, nil
}

type BPlusTreeIterator struct {
	parents []bpTreeLevelIterator
	current bpTreeLevelIterator
}

type bpTreeLevelIterator struct {
	i        int
	branches containers.BPlusTree_Branch_List
	rel      capnp.ReleaseFunc
}

func (it *BPlusTreeIterator) releaseCurrent() {
	if it.current.rel != nil {
		it.current.rel()
		it.current.rel = nil
	}
}

func (it *BPlusTreeIterator) Next(ctx context.Context) (capnp.Ptr, capnp.Ptr, error) {
	for it.current.i >= it.current.branches.Len() {
		it.releaseCurrent()
		if len(it.parents) == 0 {
			return capnp.Ptr{}, capnp.Ptr{}, io.EOF
		}
		it.current = it.parents[len(it.parents)-1]
		it.parents = it.parents[:len(it.parents)-1]
	}
	for {
		branch := it.current.branches.At(it.current.i)
		switch branch.Which() {
		case containers.BPlusTree_Branch_Which_leaf:
			v, err := branch.Leaf()
			if err != nil {
				return capnp.Ptr{}, capnp.Ptr{}, io.EOF
			}
			k, err := branch.Key()
			if err != nil {
				return capnp.Ptr{}, capnp.Ptr{}, io.EOF
			}
			it.current.i++
			return k, v, nil
		case containers.BPlusTree_Branch_Which_node:
			ref := branch.Node()
			res, rel := ref.Get(ctx, nil)
			s, err := res.Value().Struct()
			if err != nil {
				rel()
				return capnp.Ptr{}, capnp.Ptr{}, err
			}
			node := containers.BPlusTree_Node{s}
			branches, err := node.Branches()
			if err != nil {
				rel()
				return capnp.Ptr{}, capnp.Ptr{}, err
			}
			it.parents = append(it.parents, it.current)
			it.current = bpTreeLevelIterator{
				i:        0,
				branches: branches,
				rel:      rel,
			}
		}
	}
}
