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
	ErrMalformed = errors.New("Malformed triemap node")
)

type Storage interface {
	Fetch(types.Addr) (data []byte, err error)
	Store(data []byte) (types.Addr, error)
	Clear(types.Addr) error
}

func Lookup(s Storage, key []byte, m diskstore.TrieMap) (res diskstore.Addr, err error) {
	which := m.Which()
	switch which {
	case diskstore.TrieMap_Which_empty:
		return res, ErrNotFound
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
			return res, ErrNotFound
		}
		branchesAddr, err := m.Branches()
		if err != nil {
			return res, err
		}
		data, err := s.Fetch(types.DecodeAddr(branchesAddr))
		if err != nil {
			return res, err
		}
		msg := &capnp.Message{Arena: capnp.SingleSegment(data)}
		branches, err := diskstore.ReadRootTrieMap_Branches(msg)
		if err != nil {
			return res, err
		}
		bs, err := branches.Branches()
		if err != nil {
			return res, err
		}
		i := int(key[0])
		if bs.Len() <= i {
			return res, ErrNotFound
		}
		return Lookup(s, key[1:], bs.At(i))
	default:
		return res, ErrMalformed
	}
}
