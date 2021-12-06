// package triemap implements an on-disk trie, where keys are byte slices which
// must all have the same length, and values are addresses.
package triemap

import (
	"bytes"
	"errors"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/diskstore/types"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

var (
	ErrNotFound  = errors.New("Not Found")
	ErrShortKey  = errors.New("Key too short")
	ErrMalformed = errors.New("Malformed triemap node")
)

// Abstract interface for the backing store used by the map.
type Storage interface {
	// Fetch the block of data at the given address.
	Fetch(types.Addr) (data []byte, err error)

	// Store the data, returning its address.
	Store(data []byte) (types.Addr, error)

	// Marks the address range for deletion. Note: the current implementation
	// does not use this; proper cleanup is still TODO
	Clear(types.Addr) error
}

// Look up a key in the map.
func Lookup(s Storage, key []byte, m diskstore.TrieMap) (types.Addr, error) {
	addr, err := lookup(s, key, m)
	zero := types.Addr{}
	if err != nil {
		return zero, err
	}
	res := types.DecodeAddr(addr)
	if res == zero {
		return zero, ErrNotFound
	}
	return res, nil
}

func lookup(s Storage, key []byte, m diskstore.TrieMap) (res diskstore.Addr, err error) {
	switch m.Which() {
	case diskstore.TrieMap_Which_leaf:
		leaf := m.Leaf()
		prefix, err := leaf.Prefix()
		if err != nil {
			return res, err
		}
		if bytes.Compare(key, prefix) != 0 {
			return res, ErrNotFound
		}
		return leaf.Addr()
	case diskstore.TrieMap_Which_branches:
		if len(key) == 0 {
			return res, ErrShortKey
		}
		branches, err := m.Branches()
		if err != nil {
			return res, err
		}
		i := int(key[0])
		if branches.Len() <= i {
			return res, ErrMalformed
		}
		addr := branches.At(i)
		if len(key) == 1 {
			return addr, nil
		}

		if addr.Length() == 0 {
			return res, ErrNotFound
		}

		m, err := fetchNode(s, types.DecodeAddr(addr))
		if err != nil {
			return res, err
		}
		return lookup(s, key[1:], m)
	default:
		return res, ErrMalformed
	}
}

// Result returned by Insert
type InsertResult struct {
	// The address of the new root of the tree.
	ResAddr types.Addr

	// The new root of the tree.
	ResNode diskstore.TrieMap
}

// Insert the given key, value pair into the map. Replaces the old value if
// the key is already present.
func Insert(s Storage, key []byte, value diskstore.Addr, m diskstore.TrieMap) (res InsertResult, err error) {
	if !m.Struct.IsValid() {
		return saveLeaf(s, key, value)
	}
	switch m.Which() {
	case diskstore.TrieMap_Which_leaf:
		leaf := m.Leaf()
		prefix, err := leaf.Prefix()
		if err != nil {
			return res, err
		}
		oldAddr, err := leaf.Addr()
		if err != nil {
			return res, err
		}
		if bytes.Compare(key, prefix) == 0 {
			res, err = saveLeaf(s, key, value)
			if err != nil {
				return InsertResult{}, err
			}
			err = s.Clear(types.DecodeAddr(oldAddr))
			return res, err
		} else if len(key) == 0 {
			return res, ErrShortKey
		} else if len(prefix) == 0 {
			return res, ErrMalformed
		} else {
			return savePair(s, key, prefix, value, oldAddr)
		}
	case diskstore.TrieMap_Which_branches:
		branches, err := m.Branches()
		if err != nil {
			return res, err
		}
		if branches.Len() != 256 {
			return res, ErrMalformed
		}
		branchAddr := branches.At(int(key[0]))
		addr := types.DecodeAddr(branchAddr)

		// Swap the result into branchAddr, updating res to point to m.
		replaceBranch := func(res *InsertResult) error {
			res.ResAddr.EncodeInto(branchAddr)
			addr, err := s.Store(m.Struct.Segment().Data())
			if err != nil {
				return err
			}
			res.ResAddr = addr
			res.ResNode = m
			return err
		}

		if addr == (types.Addr{}) {
			// Empty branch; create a new leaf and attach it.
			res, err := saveLeaf(s, key[1:], value)
			if err != nil {
				return InsertResult{}, err
			}
			err = replaceBranch(&res)
			return res, err
		}
		mchild, err := fetchNode(s, addr)
		if err != nil {
			return res, err
		}
		res, err = Insert(s, key[1:], value, mchild)
		if err != nil {
			return res, err
		}
		err = replaceBranch(&res)
		return res, err
	default:
		return res, ErrMalformed
	}
}

// Result returned by Delete
type DeleteResult struct {
	// The new root of the map.
	ResNode diskstore.TrieMap

	// The address of the new root of the map. If this is not filled
	// in, i.e. ResAddr == types.Addr{}, it means the root has not changed,
	// which can happen if the key to be deleted was already not in the
	// map.
	ResAddr types.Addr
}

// Delete the key from the map.
func Delete(s Storage, key []byte, m diskstore.TrieMap) (DeleteResult, error) {
	switch m.Which() {
	case diskstore.TrieMap_Which_leaf:
		leaf := m.Leaf()
		prefix, err := leaf.Prefix()
		if err != nil {
			return DeleteResult{}, err
		}
		if bytes.Compare(key, prefix) != 0 {
			// Doesn't match; return unchanged.
			return DeleteResult{ResNode: m}, nil
		}
		return DeleteResult{}, nil
	case diskstore.TrieMap_Which_branches:
		if len(key) == 0 {
			return DeleteResult{}, ErrShortKey
		}
		branches, err := m.Branches()
		if err != nil {
			return DeleteResult{}, err
		}

		if branches.Len() != 256 {
			return DeleteResult{}, ErrMalformed
		}
		branchAddr := branches.At(int(key[0]))
		addr := types.DecodeAddr(branchAddr)

		if addr == (types.Addr{}) {
			return DeleteResult{ResNode: m}, nil
		}

		if len(key) == 1 {
			// Address points directly to the data; just clear it.
			(types.Addr{}).EncodeInto(branchAddr)
			addr, err = s.Store(m.Struct.Segment().Data())
			if err != nil {
				return DeleteResult{}, err
			}
			return DeleteResult{
				ResNode: m,
				ResAddr: addr,
			}, nil
		}

		mchild, err := fetchNode(s, addr)
		if err != nil {
			return DeleteResult{}, err
		}
		res, err := Delete(s, key[1:], mchild)
		if err != nil {
			return DeleteResult{}, err
		}
		res.ResAddr.EncodeInto(branchAddr)
		newAddr, err := s.Store(m.Struct.Segment().Data())
		if err != nil {
			return DeleteResult{}, err
		}

		if newAddr != addr {
			err = s.Clear(addr)
			if err != nil {
				return DeleteResult{}, err
			}
		}

		res.ResNode = m
		res.ResAddr = newAddr
		return res, nil
	default:
		return DeleteResult{}, ErrMalformed
	}
}

// Allocate a new capnp message with a map as its root.
func newNode() (*capnp.Message, *capnp.Segment, diskstore.TrieMap) {
	msg, seg := capnp.NewSingleSegmentMessage(nil)
	m, err := diskstore.NewRootTrieMap(seg)
	if err != nil {
		panic(err)
	}
	return msg, seg, m
}

// Save the prefix, value pair as a leaf node.
func saveLeaf(s Storage, prefix []byte, value diskstore.Addr) (res InsertResult, err error) {
	_, seg, root := newNode()
	root.SetLeaf()
	newLeaf := root.Leaf()
	newLeaf.SetPrefix(prefix)
	newLeaf.SetAddr(value)
	addr, err := s.Store(seg.Data())
	return InsertResult{
		ResAddr: addr,
		ResNode: root,
	}, err
}

// Save a subtree storing exactly the two (key, value) pairs.
func savePair(s Storage, k1, k2 []byte, v1, v2 diskstore.Addr) (res InsertResult, err error) {
	_, seg, m := newNode()
	branches, err := m.NewBranches(256)
	if err != nil {
		panic(err)
	}

	if len(k1) == 0 || len(k2) == 0 {
		return res, ErrShortKey
	}

	if k1[0] == k2[0] {
		res, err = savePair(s, k1[1:], k2[1:], v1, v2)
		setBranch(branches, k1[0], res.ResAddr)
	} else if len(k1) == 1 {
		setBranch(branches, k1[0], types.DecodeAddr(v1))
		setBranch(branches, k2[0], types.DecodeAddr(v2))
	} else {
		res1, err := saveLeaf(s, k1[1:], v1)
		if err != nil {
			return res, err
		}
		res2, err := saveLeaf(s, k2[1:], v2)
		if err != nil {
			return res, err
		}
		setBranch(branches, k1[0], res1.ResAddr)
		setBranch(branches, k2[0], res2.ResAddr)
	}

	addr, err := s.Store(seg.Data())
	return InsertResult{
		ResAddr: addr,
		ResNode: m,
	}, err
}

// Update the given branch of the list to point to the specifed address.
func setBranch(list diskstore.Addr_List, index uint8, addr types.Addr) {
	addr.EncodeInto(list.At(int(index)))
}

// Fetch a node from the specified address.
func fetchNode(s Storage, addr types.Addr) (diskstore.TrieMap, error) {
	data, err := s.Fetch(addr)
	if err != nil {
		return diskstore.TrieMap{}, err
	}
	msg := &capnp.Message{Arena: capnp.SingleSegment(data)}
	return diskstore.ReadRootTrieMap(msg)
}
