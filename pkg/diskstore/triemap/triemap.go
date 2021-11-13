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

type Storage interface {
	Fetch(types.Addr) (data []byte, err error)
	Store(data []byte) (types.Addr, error)
	Clear(types.Addr) error
}

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

		data, err := s.Fetch(types.DecodeAddr(addr))
		if err != nil {
			return res, err
		}
		msg := &capnp.Message{Arena: capnp.SingleSegment(data)}
		m, err := diskstore.ReadRootTrieMap(msg)
		if err != nil {
			return res, err
		}
		return lookup(s, key[1:], m)
	default:
		return res, ErrMalformed
	}
}

type InsertResult struct {
	ResAddr types.Addr
	ResNode diskstore.TrieMap
	OldAddr types.Addr
	WasOld  bool
}

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
			res.WasOld = true
			res.OldAddr = types.DecodeAddr(oldAddr)
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
		data, err := s.Fetch(types.DecodeAddr(branchAddr))
		if err != nil {
			return res, err
		}
		msg := &capnp.Message{Arena: capnp.SingleSegment(data)}
		mchild, err := diskstore.ReadRootTrieMap(msg)
		if err != nil {
			return res, err
		}
		res, err = Insert(s, key[1:], value, mchild)
		if err != nil {
			return res, err
		}
		res.ResAddr.EncodeInto(branchAddr)
		addr, err := s.Store(m.Struct.Segment().Data())
		res.ResNode = m
		res.ResAddr = addr
		return res, err
	default:
		return res, ErrMalformed
	}
}

func newNode() (*capnp.Message, *capnp.Segment, diskstore.TrieMap) {
	msg, _, _ := capnp.NewMessage(capnp.SingleSegment(nil))

	seg, err := msg.Segment(0)
	if err != nil {
		panic(err)
	}
	m, err := diskstore.NewRootTrieMap(seg)
	if err != nil {
		panic(err)
	}
	return msg, seg, m
}

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

func setBranch(list diskstore.Addr_List, index uint8, addr types.Addr) {
	addr.EncodeInto(list.At(int(index)))
}
