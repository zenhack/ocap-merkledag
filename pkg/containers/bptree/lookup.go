package bptree

import (
	"context"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/errs"
	"zenhack.net/go/ocap-md/pkg/schema/containers"
)

func (t BPlusTree) Lookup(ctx context.Context, key capnp.Ptr) (_ capnp.Ptr, ok bool, err error) {
	root, err := t.bt.Root()
	if err != nil {
		return capnp.Ptr{}, false, err
	}
	return t.lookupNode(ctx, root, key)
}

func (t BPlusTree) lookupNode(ctx context.Context, node containers.BPlusTree_Node, key capnp.Ptr) (_ capnp.Ptr, ok bool, err error) {
	// TODO: turn this into a loop, to avoid stack overflow if we're handed
	// a tree that isn't actually balanced (illegal, but representable).
	if err = ctx.Err(); err != nil {
		return capnp.Ptr{}, false, err
	}

	switch node.Which() {
	case containers.BPlusTree_Node_Which_leaf:
		leaf, err := node.Leaf()
		if err != nil {
			return capnp.Ptr{}, false, err
		}
		return t.lookupLeaf(ctx, leaf, key)
	case containers.BPlusTree_Node_Which_interior:
		return t.lookupInterior(ctx, node.Interior(), key)
	default:
		return capnp.Ptr{}, false, errs.UnknownVariant("BPlusTree.Node", uint16(node.Which()))
	}
}

func (t BPlusTree) lookupLeaf(ctx context.Context, leaf containers.KV_List, key capnp.Ptr) (_ capnp.Ptr, ok bool, err error) {
	min := 0
	max := leaf.Len()
	for max != min {
		if ctx.Err() != nil {
			return capnp.Ptr{}, false, ctx.Err()
		}

		mid := min + (max-min)/2

		kv := leaf.At(mid)
		k, err := kv.Key()
		if err != nil {
			return capnp.Ptr{}, false, err
		}
		ord := t.cmp(key, k)
		switch {
		case ord < 0:
			max = mid
		case ord > 0:
			min = mid + 1
		default:
			v, err := kv.Val()
			if err != nil {
				return capnp.Ptr{}, false, err
			}
			return v, true, nil
		}
	}
	return capnp.Ptr{}, false, nil
}

func (t BPlusTree) lookupInterior(ctx context.Context, node containers.BPlusTree_Node_interior, key capnp.Ptr) (_ capnp.Ptr, ok bool, err error) {
	panic("TODO: search interior BPlusTree nodes.")
}
