package bptree

import (
	"context"
	"strings"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/errs"
	"zenhack.net/go/ocap-md/pkg/schema/containers"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

type CmpFn = func(x, y capnp.Ptr) int

func CmpText(x, y capnp.Ptr) int {
	return strings.Compare(x.Text(), y.Text())
}

type BPlusTree struct {
	bt  containers.BPlusTree
	cmp CmpFn
}

func Open(bt containers.BPlusTree, cmp CmpFn) BPlusTree {
	return BPlusTree{
		bt:  bt,
		cmp: cmp,
	}
}

func (t BPlusTree) Iter(ctx context.Context, ch chan<- containers.KV) error {
	defer close(ch)
	root, err := t.bt.Root()
	if err != nil {
		return err
	}
	return t.iterNode(ctx, ch, root)
}

func (t BPlusTree) iterNode(ctx context.Context, ch chan<- containers.KV, node containers.BPlusTree_Node) error {
	if ctx.Err() != nil {
		return ctx.Err()
	}
	switch node.Which() {
	case containers.BPlusTree_Node_Which_leaf:
		leaf, err := node.Leaf()
		if err != nil {
			return err
		}
		return t.iterLeaf(ctx, ch, leaf)
	case containers.BPlusTree_Node_Which_interior:
		return t.iterInterior(ctx, ch, node.Interior())
	default:
		return errs.UnknownVariant("BPlusTree.Node", uint16(node.Which()))
	}
}

func (t BPlusTree) iterLeaf(ctx context.Context, ch chan<- containers.KV, leaf containers.KV_List) error {
	for i := 0; i < leaf.Len(); i++ {
		select {
		case ch <- leaf.At(i):
		case <-ctx.Done():
			return ctx.Err()
		}
	}
	return nil
}

func (t BPlusTree) iterInterior(ctx context.Context, ch chan<- containers.KV, node containers.BPlusTree_Node_interior) error {
	if err := t.iterNodeRef(ctx, ch, node.Left()); err != nil {
		return err
	}
	branches, err := node.Branches()
	if err != nil {
		return err
	}
	for i := 0; i < branches.Len(); i++ {
		v, err := branches.At(i).Val()
		if err != nil {
			return err
		}
		if err = t.iterNodeRef(ctx, ch, protocol.Ref{v.Interface().Client()}); err != nil {
			return err
		}
	}
	return nil
}

func (t BPlusTree) iterNodeRef(ctx context.Context, ch chan<- containers.KV, ref protocol.Ref) error {
	// TODO: deal with release func somehow.
	res, _ := ref.Get(ctx, nil)
	s, err := res.Value().Struct()
	if err != nil {
		return err
	}
	return t.iterNode(ctx, ch, containers.BPlusTree_Node{s})
}
