package triemap

import (
	"bytes"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/diskstore/types"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type TrieMap struct {
	storage Storage
	root    nodePtr
}

func New(s Storage, addr types.Addr) *TrieMap {
	return &TrieMap{
		storage: s,
		root:    nodePtr{addr: addr},
	}
}

func (m *TrieMap) Lookup(key []byte) (types.Addr, error) {
	return (&m.root).lookup(key, m.storage)
}

func (m *TrieMap) Insert(key []byte, val types.Addr) error {
	return (&m.root).insert(key, val, m.storage)
}

func (m *TrieMap) Remove(key []byte) error {
	return (&m.root).remove(key, m.storage)
}

func (m *TrieMap) Flush() error {
	_, err := (&m.root).flushAndClear(m.storage)
	return err
}

func (m *TrieMap) RootAddr() types.Addr {
	return m.root.addr
}

type node interface {
	remove(key []byte, s Storage) (node, error)
	insert(key []byte, val types.Addr, s Storage) (node, error)
	lookup(key []byte, s Storage) (types.Addr, error)
	flush(s Storage) (types.Addr, error)
}

type nodePtr struct {
	node  node
	dirty bool
	addr  types.Addr
}

type branch struct {
	kids [256]nodePtr
}

type leaf struct {
	key  []byte
	addr types.Addr
}

func (p *nodePtr) empty() bool {
	return p.node == nil && p.addr == types.Addr{}
}

func (p *nodePtr) insert(key []byte, val types.Addr, s Storage) error {
	err := p.load(s)
	if err != nil {
		return err
	}
	if p.empty() {
		p.node = &leaf{
			key:  key,
			addr: val,
		}
		p.dirty = true
		return nil
	}

	node, err := p.node.insert(key, val, s)
	if err == nil {
		p.node = node
		p.dirty = true
	}
	return err
}

func (l *leaf) insert(key []byte, val types.Addr, s Storage) (node, error) {
	if bytes.Compare(key, l.key) == 0 {
		l.addr = val
		return l, nil
	}

	b := &branch{}
	node, err := b.insert(l.key, l.addr, s)
	if err != nil {
		return nil, err
	}
	return node.insert(key, val, s)
}

func (b *branch) insert(key []byte, val types.Addr, s Storage) (node, error) {
	if len(key) == 0 {
		return nil, ErrShortKey
	}

	err := (&b.kids[key[0]]).insert(key[1:], val, s)
	return b, err
}

func (p *nodePtr) remove(key []byte, s Storage) error {
	if p.empty() {
		return ErrNotFound
	}
	err := p.load(s)
	if err != nil {
		return err
	}
	node, err := p.node.remove(key, s)
	if err == nil {
		p.node = node
		p.dirty = true
	}
	return err
}

func (l *leaf) remove(key []byte, s Storage) (node, error) {
	if bytes.Compare(l.key, key) == 0 {
		return nil, nil
	}

	return l, ErrNotFound
}

func (b *branch) remove(key []byte, s Storage) (node, error) {
	if len(key) == 0 {
		return b, ErrShortKey
	}
	err := (&b.kids[key[0]]).remove(key[1:], s)
	return b, err
}

func (p *nodePtr) flushAndClear(s Storage) (types.Addr, error) {
	addr, err := p.flush(s)
	if err == nil {
		p.node = nil
	}
	return addr, err
}

func (p *nodePtr) flush(s Storage) (addr types.Addr, err error) {
	if !p.dirty {
		return p.addr, nil
	}
	if p.node != nil {
		addr, err = p.node.flush(s)
	}
	if err != nil {
		return types.Addr{}, err
	}
	p.addr = addr
	p.dirty = false
	return addr, nil
}

func (l *leaf) flush(s Storage) (types.Addr, error) {
	_, seg, _ := capnp.NewMessage(capnp.SingleSegment(nil))
	m, err := diskstore.NewRootTrieMap(seg)
	if err != nil {
		return types.Addr{}, err
	}
	m.SetLeaf()
	leaf := m.Leaf()
	leaf.SetPrefix(l.key)
	addr, err := leaf.NewAddr()
	if err != nil {
		return types.Addr{}, err
	}
	l.addr.EncodeInto(addr)
	return s.Store(seg.Data())
}

func (b *branch) flush(s Storage) (types.Addr, error) {
	_, seg, _ := capnp.NewMessage(capnp.SingleSegment(nil))
	m, err := diskstore.NewRootTrieMap(seg)
	if err != nil {
		return types.Addr{}, err
	}
	branches, err := m.NewBranches(256)
	if err != nil {
		return types.Addr{}, err
	}

	for i := range b.kids {
		addr, err := (&b.kids[i]).flush(s)
		if err != nil {
			return types.Addr{}, err
		}
		addr.EncodeInto(branches.At(i))
	}

	return s.Store(seg.Data())
}

func (p *nodePtr) lookup(key []byte, s Storage) (types.Addr, error) {
	err := p.load(s)
	if err != nil {
		return types.Addr{}, err
	}
	if p.node == nil {
		return types.Addr{}, ErrNotFound
	}
	return p.node.lookup(key, s)
}

func (l *leaf) lookup(key []byte, s Storage) (types.Addr, error) {
	if bytes.Compare(key, l.key) != 0 {
		return types.Addr{}, ErrNotFound
	}
	return l.addr, nil
}

func (b *branch) lookup(key []byte, s Storage) (types.Addr, error) {
	if len(key) == 0 {
		return types.Addr{}, ErrShortKey
	}
	kid := &b.kids[key[0]]
	return kid.lookup(key[1:], s)
}

func (p *nodePtr) load(s Storage) error {
	if p.node != nil || p.empty() {
		// already loaded.
		return nil
	}
	m, err := fetchNode(s, p.addr)
	if err != nil {
		return err
	}
	return p.parse(m)
}

func (p *nodePtr) parse(m diskstore.TrieMap) (err error) {
	switch m.Which() {
	case diskstore.TrieMap_Which_leaf:
		var leafNode leaf
		leaf := m.Leaf()
		leafNode.key, err = leaf.Prefix()
		if err != nil {
			return err
		}
		addr, err := leaf.Addr()
		if err != nil {
			return err
		}
		leafNode.addr = types.DecodeAddr(addr)
		p.node = &leafNode
		return nil
	case diskstore.TrieMap_Which_branches:
		var branchNode branch
		branches, err := m.Branches()
		if err != nil {
			return err
		}
		if branches.Len() != 256 {
			return ErrMalformed
		}
		for i := 0; i < 256; i++ {
			branchNode.kids[i].addr = types.DecodeAddr(branches.At(i))
		}
		p.node = &branchNode
		return nil
	default:
		return ErrMalformed
	}
}
