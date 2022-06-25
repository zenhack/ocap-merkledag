// Code generated by capnpc-go. DO NOT EDIT.

package containers

import (
	capnp "capnproto.org/go/capnp/v3"
	text "capnproto.org/go/capnp/v3/encoding/text"
	schemas "capnproto.org/go/capnp/v3/schemas"
	strconv "strconv"
	protocol "zenhack.net/go/ocap-md/pkg/schema/protocol"
)

type KV struct{ capnp.Struct }

// KV_TypeID is the unique identifier for the type KV.
const KV_TypeID = 0xcfcc3326923195bc

func NewKV(s *capnp.Segment) (KV, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 0, PointerCount: 2})
	return KV{st}, err
}

func NewRootKV(s *capnp.Segment) (KV, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 0, PointerCount: 2})
	return KV{st}, err
}

func ReadRootKV(msg *capnp.Message) (KV, error) {
	root, err := msg.Root()
	return KV{root.Struct()}, err
}

func (s KV) String() string {
	str, _ := text.Marshal(0xcfcc3326923195bc, s.Struct)
	return str
}

func (s KV) Key() (capnp.Ptr, error) {
	return s.Struct.Ptr(0)
}

func (s KV) HasKey() bool {
	return s.Struct.HasPtr(0)
}

func (s KV) SetKey(v capnp.Ptr) error {
	return s.Struct.SetPtr(0, v)
}

func (s KV) Val() (capnp.Ptr, error) {
	return s.Struct.Ptr(1)
}

func (s KV) HasVal() bool {
	return s.Struct.HasPtr(1)
}

func (s KV) SetVal(v capnp.Ptr) error {
	return s.Struct.SetPtr(1, v)
}

// KV_List is a list of KV.
type KV_List = capnp.StructList[KV]

// NewKV creates a new list of KV.
func NewKV_List(s *capnp.Segment, sz int32) (KV_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 0, PointerCount: 2}, sz)
	return capnp.StructList[KV]{List: l}, err
}

// KV_Future is a wrapper for a KV promised by a client call.
type KV_Future struct{ *capnp.Future }

func (p KV_Future) Struct() (KV, error) {
	s, err := p.Future.Struct()
	return KV{s}, err
}

func (p KV_Future) Key() *capnp.Future {
	return p.Future.Field(0, nil)
}

func (p KV_Future) Val() *capnp.Future {
	return p.Future.Field(1, nil)
}

type BPlusTree struct{ capnp.Struct }

// BPlusTree_TypeID is the unique identifier for the type BPlusTree.
const BPlusTree_TypeID = 0x96262c98f85cc890

func NewBPlusTree(s *capnp.Segment) (BPlusTree, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 1})
	return BPlusTree{st}, err
}

func NewRootBPlusTree(s *capnp.Segment) (BPlusTree, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 1})
	return BPlusTree{st}, err
}

func ReadRootBPlusTree(msg *capnp.Message) (BPlusTree, error) {
	root, err := msg.Root()
	return BPlusTree{root.Struct()}, err
}

func (s BPlusTree) String() string {
	str, _ := text.Marshal(0x96262c98f85cc890, s.Struct)
	return str
}

func (s BPlusTree) Root() (BPlusTree_Node, error) {
	p, err := s.Struct.Ptr(0)
	return BPlusTree_Node{Struct: p.Struct()}, err
}

func (s BPlusTree) HasRoot() bool {
	return s.Struct.HasPtr(0)
}

func (s BPlusTree) SetRoot(v BPlusTree_Node) error {
	return s.Struct.SetPtr(0, v.Struct.ToPtr())
}

// NewRoot sets the root field to a newly
// allocated BPlusTree_Node struct, preferring placement in s's segment.
func (s BPlusTree) NewRoot() (BPlusTree_Node, error) {
	ss, err := NewBPlusTree_Node(s.Struct.Segment())
	if err != nil {
		return BPlusTree_Node{}, err
	}
	err = s.Struct.SetPtr(0, ss.Struct.ToPtr())
	return ss, err
}

func (s BPlusTree) MinBranches() uint32 {
	return s.Struct.Uint32(0)
}

func (s BPlusTree) SetMinBranches(v uint32) {
	s.Struct.SetUint32(0, v)
}

func (s BPlusTree) MaxBranches() uint32 {
	return s.Struct.Uint32(4)
}

func (s BPlusTree) SetMaxBranches(v uint32) {
	s.Struct.SetUint32(4, v)
}

// BPlusTree_List is a list of BPlusTree.
type BPlusTree_List = capnp.StructList[BPlusTree]

// NewBPlusTree creates a new list of BPlusTree.
func NewBPlusTree_List(s *capnp.Segment, sz int32) (BPlusTree_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 8, PointerCount: 1}, sz)
	return capnp.StructList[BPlusTree]{List: l}, err
}

// BPlusTree_Future is a wrapper for a BPlusTree promised by a client call.
type BPlusTree_Future struct{ *capnp.Future }

func (p BPlusTree_Future) Struct() (BPlusTree, error) {
	s, err := p.Future.Struct()
	return BPlusTree{s}, err
}

func (p BPlusTree_Future) Root() BPlusTree_Node_Future {
	return BPlusTree_Node_Future{Future: p.Future.Field(0, nil)}
}

type BPlusTree_Node struct{ capnp.Struct }
type BPlusTree_Node_interior BPlusTree_Node
type BPlusTree_Node_Which uint16

const (
	BPlusTree_Node_Which_leaf     BPlusTree_Node_Which = 0
	BPlusTree_Node_Which_interior BPlusTree_Node_Which = 1
)

func (w BPlusTree_Node_Which) String() string {
	const s = "leafinterior"
	switch w {
	case BPlusTree_Node_Which_leaf:
		return s[0:4]
	case BPlusTree_Node_Which_interior:
		return s[4:12]

	}
	return "BPlusTree_Node_Which(" + strconv.FormatUint(uint64(w), 10) + ")"
}

// BPlusTree_Node_TypeID is the unique identifier for the type BPlusTree_Node.
const BPlusTree_Node_TypeID = 0x892485c6916ad641

func NewBPlusTree_Node(s *capnp.Segment) (BPlusTree_Node, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2})
	return BPlusTree_Node{st}, err
}

func NewRootBPlusTree_Node(s *capnp.Segment) (BPlusTree_Node, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2})
	return BPlusTree_Node{st}, err
}

func ReadRootBPlusTree_Node(msg *capnp.Message) (BPlusTree_Node, error) {
	root, err := msg.Root()
	return BPlusTree_Node{root.Struct()}, err
}

func (s BPlusTree_Node) String() string {
	str, _ := text.Marshal(0x892485c6916ad641, s.Struct)
	return str
}

func (s BPlusTree_Node) Which() BPlusTree_Node_Which {
	return BPlusTree_Node_Which(s.Struct.Uint16(0))
}
func (s BPlusTree_Node) Leaf() (KV_List, error) {
	if s.Struct.Uint16(0) != 0 {
		panic("Which() != leaf")
	}
	p, err := s.Struct.Ptr(0)
	return KV_List{List: p.List()}, err
}

func (s BPlusTree_Node) HasLeaf() bool {
	if s.Struct.Uint16(0) != 0 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s BPlusTree_Node) SetLeaf(v KV_List) error {
	s.Struct.SetUint16(0, 0)
	return s.Struct.SetPtr(0, v.List.ToPtr())
}

// NewLeaf sets the leaf field to a newly
// allocated KV_List, preferring placement in s's segment.
func (s BPlusTree_Node) NewLeaf(n int32) (KV_List, error) {
	s.Struct.SetUint16(0, 0)
	l, err := NewKV_List(s.Struct.Segment(), n)
	if err != nil {
		return KV_List{}, err
	}
	err = s.Struct.SetPtr(0, l.List.ToPtr())
	return l, err
}

func (s BPlusTree_Node) Interior() BPlusTree_Node_interior { return BPlusTree_Node_interior(s) }

func (s BPlusTree_Node) SetInterior() {
	s.Struct.SetUint16(0, 1)
}

func (s BPlusTree_Node_interior) Left() protocol.Ref {
	p, _ := s.Struct.Ptr(0)
	return protocol.Ref{Client: p.Interface().Client()}
}

func (s BPlusTree_Node_interior) HasLeft() bool {
	return s.Struct.HasPtr(0)
}

func (s BPlusTree_Node_interior) SetLeft(v protocol.Ref) error {
	if !v.Client.IsValid() {
		return s.Struct.SetPtr(0, capnp.Ptr{})
	}
	seg := s.Segment()
	in := capnp.NewInterface(seg, seg.Message().AddCap(v.Client))
	return s.Struct.SetPtr(0, in.ToPtr())
}

func (s BPlusTree_Node_interior) Branches() (KV_List, error) {
	p, err := s.Struct.Ptr(1)
	return KV_List{List: p.List()}, err
}

func (s BPlusTree_Node_interior) HasBranches() bool {
	return s.Struct.HasPtr(1)
}

func (s BPlusTree_Node_interior) SetBranches(v KV_List) error {
	return s.Struct.SetPtr(1, v.List.ToPtr())
}

// NewBranches sets the branches field to a newly
// allocated KV_List, preferring placement in s's segment.
func (s BPlusTree_Node_interior) NewBranches(n int32) (KV_List, error) {
	l, err := NewKV_List(s.Struct.Segment(), n)
	if err != nil {
		return KV_List{}, err
	}
	err = s.Struct.SetPtr(1, l.List.ToPtr())
	return l, err
}

// BPlusTree_Node_List is a list of BPlusTree_Node.
type BPlusTree_Node_List = capnp.StructList[BPlusTree_Node]

// NewBPlusTree_Node creates a new list of BPlusTree_Node.
func NewBPlusTree_Node_List(s *capnp.Segment, sz int32) (BPlusTree_Node_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2}, sz)
	return capnp.StructList[BPlusTree_Node]{List: l}, err
}

// BPlusTree_Node_Future is a wrapper for a BPlusTree_Node promised by a client call.
type BPlusTree_Node_Future struct{ *capnp.Future }

func (p BPlusTree_Node_Future) Struct() (BPlusTree_Node, error) {
	s, err := p.Future.Struct()
	return BPlusTree_Node{s}, err
}

func (p BPlusTree_Node_Future) Interior() BPlusTree_Node_interior_Future {
	return BPlusTree_Node_interior_Future{p.Future}
}

// BPlusTree_Node_interior_Future is a wrapper for a BPlusTree_Node_interior promised by a client call.
type BPlusTree_Node_interior_Future struct{ *capnp.Future }

func (p BPlusTree_Node_interior_Future) Struct() (BPlusTree_Node_interior, error) {
	s, err := p.Future.Struct()
	return BPlusTree_Node_interior{s}, err
}

func (p BPlusTree_Node_interior_Future) Left() protocol.Ref {
	return protocol.Ref{Client: p.Future.Field(0, nil).Client()}
}

const schema_eb26e91f28682410 = "x\xda\xb4\x92\xc1k\x13A\x18\xc5\xbf7\xb3\xdb\x8d\xd0" +
	"6;\xd9J\x04\x0f\x15)\xb5-\x1a\x1a\"\x08=\xa5" +
	"\x85\x80\xa4\xa0\x19\x95\x9cZp\x9bNI4\xdd\xc4M" +
	"\xb4\xf6\xe0\xc1C@\x0f\x82\x0aJ\xf5\xa6\xe0Y<\x0a" +
	"=KAE<\xe8\xc1\xff@\xbc\x8b :2\xe96" +
	"\xd9F\x04\x0fz\xc9\xe1e\xf9\xe6\xfd\xde{\xb3;\xc8" +
	"[\xd9\x91SC\xc4\xe4i{H\xcf\x7f\xbct\xefU" +
	"g\xe26\xc9\xc3\x80\xbe\xbb\xb3\xf4m\xeb\xf8\xe4C*" +
	"0\x87\xb9\xc8\x813x#\xdc!\xf2\x0e\xf0\x0d\x8a}" +
	" \x05\xa0\xdd\x89\xea\xd4\xf8\xe7\xc9/d\xc3q\xe1=" +
	"\xe3\x9f\xbc\x17<M\xe4\xbd\xe4\xcf\x89\xbc\x8e\x95\xd67" +
	"\xbe\x7f=\xf7\xba\xfcx\x8b\xe4\x11\xa0\xff\xdaA\xe6\xc0" +
	"E\xee\xa6U\x04\xc1\xbbc\x99\xdb\xdb\x0f\xb2\xf7's" +
	"o\xde\x91\x10\xf1\xd3\xcc\x9c~k?\xf5>\xd8\xc6\xc7" +
	"{{\x83\xc8+\x0c\xa5\xe9\x91\xae4\x82\xb6_\x0b\x14" +
	"\x0f[\x99\x8a\xdf\x0c\x9as\x0b\xa5\xfa\xd5\xd6\x85P\xa9" +
	"\xcc\x99\xc6\xaa\xa2\x12 \x13\xdc\x1a\xd6\xda\x02\x91\x98\x9e" +
	"!\x92\x13\x1c\xf2\x16\xc3\x08~j\xa0\xefOt\x8a\xc4" +
	"\x92u\xe5\xafa\x94P\xe2\x80\xdbwD\x94\x87\xc0\xb8" +
	"\xb4\x18\xe2\xa2\xc01\x99\x00\xba\x8f\x98_\x97\x03\xa9x" +
	"\x8cDH\xd9\x03\xc2(A\xd7\x82\xb6\x0ak\x8d\x90\x88" +
	"z\x10l\x10\x82+%\xadxh\x023I\x83%\x87" +
	"\xb9E\xd4%*\x18\xa2<\x87\xbc\xc8\x00\x8c\xc1h\xcb" +
	"+Dr\x89CV\x19\x04\xc3\x18\x18\x91PF\\\xe5" +
	"\x90M\x86d\xd8h\xb4\xe1\xf6\x0f\xc7\xf1zV\xcd-" +
	"\xb8\x04\xbd^\x0b\x16B?\xa8\x90SU-$\x88!" +
	"aT\xff\xfa\xef\xea|\x02\xc2N\x09;\x85E\x94{" +
	"d\xd6\x1f\xea\xc9\xf4c0-\x11\xed\xfa\xef\xb5\xd4f" +
	"\x10\x88\xfc_)\x12\xc9&\xc7\xf9C`H\xd6\xd5Z" +
	"\x1bBg\xb7\x977\xd7\x7f\x9c}\xb2\x0f &\x0a\xa4" +
	"M\x82\x88\xea\xfc\x0b^A\xd0+\x06\xab\xaaZQY" +
	"\xffn\x0a\xff\xc3\xb0Y\xd3^\xce\xd8\xcb\xd9\x99[," +
	"G\xd3\x8fv2}4\xcat6\xca\xd4\x88'\x8c8" +
	"\xc5!O28\x97\xd5f\xd7p\x8c\x09)\x82s\xcd" +
	"\xaf\xefnx\xe0\x8f}U\xff\x0a\x00\x00\xff\xff\xee\x98" +
	"\x17\xb8"

func init() {
	schemas.Register(schema_eb26e91f28682410,
		0x892485c6916ad641,
		0x96262c98f85cc890,
		0x989a56cb52f6fb7d,
		0xcfcc3326923195bc)
}
