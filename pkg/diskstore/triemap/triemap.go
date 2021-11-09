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
