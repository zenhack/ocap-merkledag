// Code generated by capnpc-go. DO NOT EDIT.

package diskstore

import (
	capnp "capnproto.org/go/capnp/v3"
	text "capnproto.org/go/capnp/v3/encoding/text"
	schemas "capnproto.org/go/capnp/v3/schemas"
	strconv "strconv"
	protocol "zenhack.net/go/ocap-md/pkg/schema/protocol"
)

// Constants defined in diskstore.capnp.
const (
	StorageFormatVersion = uint32(1)
)

type Manifest struct{ capnp.Struct }
type Manifest_lastLog Manifest

// Manifest_TypeID is the unique identifier for the type Manifest.
const Manifest_TypeID = 0xe09672577fe40a61

func NewManifest(s *capnp.Segment) (Manifest, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 24, PointerCount: 2})
	return Manifest{st}, err
}

func NewRootManifest(s *capnp.Segment) (Manifest, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 24, PointerCount: 2})
	return Manifest{st}, err
}

func ReadRootManifest(msg *capnp.Message) (Manifest, error) {
	root, err := msg.Root()
	return Manifest{root.Struct()}, err
}

func (s Manifest) String() string {
	str, _ := text.Marshal(0xe09672577fe40a61, s.Struct)
	return str
}

func (s Manifest) FormatVersion() uint32 {
	return s.Struct.Uint32(16)
}

func (s Manifest) SetFormatVersion(v uint32) {
	s.Struct.SetUint32(16, v)
}

func (s Manifest) BlobMap() (Addr, error) {
	p, err := s.Struct.Ptr(0)
	return Addr{Struct: p.Struct()}, err
}

func (s Manifest) HasBlobMap() bool {
	return s.Struct.HasPtr(0)
}

func (s Manifest) SetBlobMap(v Addr) error {
	return s.Struct.SetPtr(0, v.Struct.ToPtr())
}

// NewBlobMap sets the blobMap field to a newly
// allocated Addr struct, preferring placement in s's segment.
func (s Manifest) NewBlobMap() (Addr, error) {
	ss, err := NewAddr(s.Struct.Segment())
	if err != nil {
		return Addr{}, err
	}
	err = s.Struct.SetPtr(0, ss.Struct.ToPtr())
	return ss, err
}

func (s Manifest) Root() (protocol.ContentId, error) {
	p, err := s.Struct.Ptr(1)
	return protocol.ContentId{Struct: p.Struct()}, err
}

func (s Manifest) HasRoot() bool {
	return s.Struct.HasPtr(1)
}

func (s Manifest) SetRoot(v protocol.ContentId) error {
	return s.Struct.SetPtr(1, v.Struct.ToPtr())
}

// NewRoot sets the root field to a newly
// allocated protocol.ContentId struct, preferring placement in s's segment.
func (s Manifest) NewRoot() (protocol.ContentId, error) {
	ss, err := protocol.NewContentId(s.Struct.Segment())
	if err != nil {
		return protocol.ContentId{}, err
	}
	err = s.Struct.SetPtr(1, ss.Struct.ToPtr())
	return ss, err
}

func (s Manifest) LastLog() Manifest_lastLog { return Manifest_lastLog(s) }

func (s Manifest_lastLog) Size() uint64 {
	return s.Struct.Uint64(0)
}

func (s Manifest_lastLog) SetSize(v uint64) {
	s.Struct.SetUint64(0, v)
}

func (s Manifest_lastLog) Number() uint64 {
	return s.Struct.Uint64(8)
}

func (s Manifest_lastLog) SetNumber(v uint64) {
	s.Struct.SetUint64(8, v)
}

// Manifest_List is a list of Manifest.
type Manifest_List = capnp.StructList[Manifest]

// NewManifest creates a new list of Manifest.
func NewManifest_List(s *capnp.Segment, sz int32) (Manifest_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 24, PointerCount: 2}, sz)
	return capnp.StructList[Manifest]{l}, err
}

// Manifest_Future is a wrapper for a Manifest promised by a client call.
type Manifest_Future struct{ *capnp.Future }

func (p Manifest_Future) Struct() (Manifest, error) {
	s, err := p.Future.Struct()
	return Manifest{s}, err
}

func (p Manifest_Future) BlobMap() Addr_Future {
	return Addr_Future{Future: p.Future.Field(0, nil)}
}

func (p Manifest_Future) Root() protocol.ContentId_Future {
	return protocol.ContentId_Future{Future: p.Future.Field(1, nil)}
}

func (p Manifest_Future) LastLog() Manifest_lastLog_Future { return Manifest_lastLog_Future{p.Future} }

// Manifest_lastLog_Future is a wrapper for a Manifest_lastLog promised by a client call.
type Manifest_lastLog_Future struct{ *capnp.Future }

func (p Manifest_lastLog_Future) Struct() (Manifest_lastLog, error) {
	s, err := p.Future.Struct()
	return Manifest_lastLog{s}, err
}

type Addr struct{ capnp.Struct }

// Addr_TypeID is the unique identifier for the type Addr.
const Addr_TypeID = 0xe45f1d3ad96f0c55

func NewAddr(s *capnp.Segment) (Addr, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 24, PointerCount: 0})
	return Addr{st}, err
}

func NewRootAddr(s *capnp.Segment) (Addr, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 24, PointerCount: 0})
	return Addr{st}, err
}

func ReadRootAddr(msg *capnp.Message) (Addr, error) {
	root, err := msg.Root()
	return Addr{root.Struct()}, err
}

func (s Addr) String() string {
	str, _ := text.Marshal(0xe45f1d3ad96f0c55, s.Struct)
	return str
}

func (s Addr) LogNumber() uint64 {
	return s.Struct.Uint64(16)
}

func (s Addr) SetLogNumber(v uint64) {
	s.Struct.SetUint64(16, v)
}

func (s Addr) Offset() uint64 {
	return s.Struct.Uint64(0)
}

func (s Addr) SetOffset(v uint64) {
	s.Struct.SetUint64(0, v)
}

func (s Addr) Length() uint32 {
	return s.Struct.Uint32(8)
}

func (s Addr) SetLength(v uint32) {
	s.Struct.SetUint32(8, v)
}

// Addr_List is a list of Addr.
type Addr_List = capnp.StructList[Addr]

// NewAddr creates a new list of Addr.
func NewAddr_List(s *capnp.Segment, sz int32) (Addr_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 24, PointerCount: 0}, sz)
	return capnp.StructList[Addr]{l}, err
}

// Addr_Future is a wrapper for a Addr promised by a client call.
type Addr_Future struct{ *capnp.Future }

func (p Addr_Future) Struct() (Addr, error) {
	s, err := p.Future.Struct()
	return Addr{s}, err
}

type LogEntry struct{ capnp.Struct }
type LogEntry_Which uint16

const (
	LogEntry_Which_blob      LogEntry_Which = 0
	LogEntry_Which_indexNode LogEntry_Which = 1
)

func (w LogEntry_Which) String() string {
	const s = "blobindexNode"
	switch w {
	case LogEntry_Which_blob:
		return s[0:4]
	case LogEntry_Which_indexNode:
		return s[4:13]

	}
	return "LogEntry_Which(" + strconv.FormatUint(uint64(w), 10) + ")"
}

// LogEntry_TypeID is the unique identifier for the type LogEntry.
const LogEntry_TypeID = 0xd848462725353ce8

func NewLogEntry(s *capnp.Segment) (LogEntry, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 1})
	return LogEntry{st}, err
}

func NewRootLogEntry(s *capnp.Segment) (LogEntry, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 1})
	return LogEntry{st}, err
}

func ReadRootLogEntry(msg *capnp.Message) (LogEntry, error) {
	root, err := msg.Root()
	return LogEntry{root.Struct()}, err
}

func (s LogEntry) String() string {
	str, _ := text.Marshal(0xd848462725353ce8, s.Struct)
	return str
}

func (s LogEntry) Which() LogEntry_Which {
	return LogEntry_Which(s.Struct.Uint16(0))
}
func (s LogEntry) Blob() ([]byte, error) {
	if s.Struct.Uint16(0) != 0 {
		panic("Which() != blob")
	}
	p, err := s.Struct.Ptr(0)
	return []byte(p.Data()), err
}

func (s LogEntry) HasBlob() bool {
	if s.Struct.Uint16(0) != 0 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s LogEntry) SetBlob(v []byte) error {
	s.Struct.SetUint16(0, 0)
	return s.Struct.SetData(0, v)
}

func (s LogEntry) IndexNode() (TrieMap, error) {
	if s.Struct.Uint16(0) != 1 {
		panic("Which() != indexNode")
	}
	p, err := s.Struct.Ptr(0)
	return TrieMap{Struct: p.Struct()}, err
}

func (s LogEntry) HasIndexNode() bool {
	if s.Struct.Uint16(0) != 1 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s LogEntry) SetIndexNode(v TrieMap) error {
	s.Struct.SetUint16(0, 1)
	return s.Struct.SetPtr(0, v.Struct.ToPtr())
}

// NewIndexNode sets the indexNode field to a newly
// allocated TrieMap struct, preferring placement in s's segment.
func (s LogEntry) NewIndexNode() (TrieMap, error) {
	s.Struct.SetUint16(0, 1)
	ss, err := NewTrieMap(s.Struct.Segment())
	if err != nil {
		return TrieMap{}, err
	}
	err = s.Struct.SetPtr(0, ss.Struct.ToPtr())
	return ss, err
}

// LogEntry_List is a list of LogEntry.
type LogEntry_List = capnp.StructList[LogEntry]

// NewLogEntry creates a new list of LogEntry.
func NewLogEntry_List(s *capnp.Segment, sz int32) (LogEntry_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 8, PointerCount: 1}, sz)
	return capnp.StructList[LogEntry]{l}, err
}

// LogEntry_Future is a wrapper for a LogEntry promised by a client call.
type LogEntry_Future struct{ *capnp.Future }

func (p LogEntry_Future) Struct() (LogEntry, error) {
	s, err := p.Future.Struct()
	return LogEntry{s}, err
}

func (p LogEntry_Future) IndexNode() TrieMap_Future {
	return TrieMap_Future{Future: p.Future.Field(0, nil)}
}

type TrieMap struct{ capnp.Struct }
type TrieMap_leaf TrieMap
type TrieMap_Which uint16

const (
	TrieMap_Which_leaf     TrieMap_Which = 0
	TrieMap_Which_branches TrieMap_Which = 1
)

func (w TrieMap_Which) String() string {
	const s = "leafbranches"
	switch w {
	case TrieMap_Which_leaf:
		return s[0:4]
	case TrieMap_Which_branches:
		return s[4:12]

	}
	return "TrieMap_Which(" + strconv.FormatUint(uint64(w), 10) + ")"
}

// TrieMap_TypeID is the unique identifier for the type TrieMap.
const TrieMap_TypeID = 0xcc6f8e43c0d837f7

func NewTrieMap(s *capnp.Segment) (TrieMap, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 3})
	return TrieMap{st}, err
}

func NewRootTrieMap(s *capnp.Segment) (TrieMap, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 3})
	return TrieMap{st}, err
}

func ReadRootTrieMap(msg *capnp.Message) (TrieMap, error) {
	root, err := msg.Root()
	return TrieMap{root.Struct()}, err
}

func (s TrieMap) String() string {
	str, _ := text.Marshal(0xcc6f8e43c0d837f7, s.Struct)
	return str
}

func (s TrieMap) Which() TrieMap_Which {
	return TrieMap_Which(s.Struct.Uint16(0))
}
func (s TrieMap) Leaf() TrieMap_leaf { return TrieMap_leaf(s) }

func (s TrieMap) SetLeaf() {
	s.Struct.SetUint16(0, 0)
}

func (s TrieMap_leaf) Prefix() ([]byte, error) {
	p, err := s.Struct.Ptr(0)
	return []byte(p.Data()), err
}

func (s TrieMap_leaf) HasPrefix() bool {
	return s.Struct.HasPtr(0)
}

func (s TrieMap_leaf) SetPrefix(v []byte) error {
	return s.Struct.SetData(0, v)
}

func (s TrieMap_leaf) Addr() (Addr, error) {
	p, err := s.Struct.Ptr(1)
	return Addr{Struct: p.Struct()}, err
}

func (s TrieMap_leaf) HasAddr() bool {
	return s.Struct.HasPtr(1)
}

func (s TrieMap_leaf) SetAddr(v Addr) error {
	return s.Struct.SetPtr(1, v.Struct.ToPtr())
}

// NewAddr sets the addr field to a newly
// allocated Addr struct, preferring placement in s's segment.
func (s TrieMap_leaf) NewAddr() (Addr, error) {
	ss, err := NewAddr(s.Struct.Segment())
	if err != nil {
		return Addr{}, err
	}
	err = s.Struct.SetPtr(1, ss.Struct.ToPtr())
	return ss, err
}

func (s TrieMap) Branches() (Addr_List, error) {
	if s.Struct.Uint16(0) != 1 {
		panic("Which() != branches")
	}
	p, err := s.Struct.Ptr(0)
	return Addr_List{List: p.List()}, err
}

func (s TrieMap) HasBranches() bool {
	if s.Struct.Uint16(0) != 1 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s TrieMap) SetBranches(v Addr_List) error {
	s.Struct.SetUint16(0, 1)
	return s.Struct.SetPtr(0, v.List.ToPtr())
}

// NewBranches sets the branches field to a newly
// allocated Addr_List, preferring placement in s's segment.
func (s TrieMap) NewBranches(n int32) (Addr_List, error) {
	s.Struct.SetUint16(0, 1)
	l, err := NewAddr_List(s.Struct.Segment(), n)
	if err != nil {
		return Addr_List{}, err
	}
	err = s.Struct.SetPtr(0, l.List.ToPtr())
	return l, err
}

func (s TrieMap) BreadCrumbs() ([]byte, error) {
	p, err := s.Struct.Ptr(2)
	return []byte(p.Data()), err
}

func (s TrieMap) HasBreadCrumbs() bool {
	return s.Struct.HasPtr(2)
}

func (s TrieMap) SetBreadCrumbs(v []byte) error {
	return s.Struct.SetData(2, v)
}

// TrieMap_List is a list of TrieMap.
type TrieMap_List = capnp.StructList[TrieMap]

// NewTrieMap creates a new list of TrieMap.
func NewTrieMap_List(s *capnp.Segment, sz int32) (TrieMap_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 8, PointerCount: 3}, sz)
	return capnp.StructList[TrieMap]{l}, err
}

// TrieMap_Future is a wrapper for a TrieMap promised by a client call.
type TrieMap_Future struct{ *capnp.Future }

func (p TrieMap_Future) Struct() (TrieMap, error) {
	s, err := p.Future.Struct()
	return TrieMap{s}, err
}

func (p TrieMap_Future) Leaf() TrieMap_leaf_Future { return TrieMap_leaf_Future{p.Future} }

// TrieMap_leaf_Future is a wrapper for a TrieMap_leaf promised by a client call.
type TrieMap_leaf_Future struct{ *capnp.Future }

func (p TrieMap_leaf_Future) Struct() (TrieMap_leaf, error) {
	s, err := p.Future.Struct()
	return TrieMap_leaf{s}, err
}

func (p TrieMap_leaf_Future) Addr() Addr_Future {
	return Addr_Future{Future: p.Future.Field(1, nil)}
}

const schema_dc976fcd6fee6223 = "x\xdatSM\x88\x1cE\x18}\xaf\xaa\xe3L\xc2n" +
	"\xa6\x87\xde\x80\x1e\x16\x11]\x92,f\x89D1\x0c\xc2" +
	"\xc6\x8d\x13\xa2\xec.S\xc6\x9f\x08+\xda\x93\xae\x99i" +
	"\x9c\xed\x1e\xab;\x18\x03\xb2\x07\x11<\x09^\xd4\x8bW" +
	"\x0f^\xf6\xe4!\x87\x88+\xe8!\xc4\x9b`\x04A\x03" +
	"\x11\x7f\x0e^\x05\x0f)\xa9\x9e\xde\x9eaBn]\xc5" +
	"\xab\xef\xbd\xef\xbd\xd7'\xaf\xf1\x8c\xf7\xc4\xfcO\x1e\x84" +
	":}\xe0\x01\xbbv\xf1\xd7c\xdf\xef}\xfe1\x9a\xbe" +
	"g\x1f\xed\xfe\x93\xfe\x90~\xfa\x0b\xc0SJ,3\x08" +
	"E\x0d\xb8\xb0%$/\x0c\x84 `?\xbc\xfd~\xf0" +
	"\xde\xdd\xfegP\x0f\x92\xf6\xdf\xa7o}s\xf6\xa3\xf4" +
	"&\x8e\xc8\x1a\x81\xe05\xf17\x18\xbc.\xde\x01\xedM" +
	"\xff\xe8\xdb_\x7f\xb0\xb0\x0b\xf5\x10\xa5\x0d\x0f\xdd\xd9y" +
	"\xd5|\xf2\x1b\x8e\x08\x87<uM\x1c\"\x18\xec\x15\xd0" +
	"j\x8c\xf2\xc9\x89\x8a\xb6\xac\x09 X\x94\xdf\x06K\xb2" +
	"\x06\x04\x8f\xc8]\xd0\xfe\xf9\xccSKG\xcf\x9d\xbf5" +
	"\x8bf\x81\xfeJ\xde\x08\xf6\x0a\xf4u\xe9fW\xc4\xca" +
	"\xa7\x9c\xa0\x0f\xb8\xcd\x82E\xefFp\xdcs_K\xde" +
	"\x1f\xa0}y.\xfd\xb9\xb5\xf8\xc6\x9d\x19p\x81\xb8\xed" +
	"}\x19\xfcU|\xfd\xee\xedb\xd3Fq\xf6V\x96\xa7" +
	"F\xea\x95K\xe1(\x19\xb5\xdc!\xec\xebs\xa9\xd9\x0e" +
	"\xf3W\xb4idq\x9atH\xd6!\x9au\x82\xd5\x13" +
	"\xb1\xff\xe4%\x13\xeb\x8dp\xb4\xd2\x18\xea\xb0\xa7\xea\xd2" +
	"\x03<\x02\xcd\xe3-@=&\xa9N\x0a6\xc9\x05\xba" +
	"\xcb\x13\xcb\x80:&\xa9\x9e\x14\\\x1d\x19\xdd\x8b\xafp" +
	"\x1e\x82\xf3`#\x8c\"C\x7f\xa2\x1f\xa4?EXi" +
	"\xdc\x08\x93\xb8\xa7\xb3|e\x18f\xf9z\xda\x07\xc6\xac" +
	"\x0b\x14\x8evy\x86V:\xda\xd6\x84\xb6\x91\xc5W5" +
	"\x0fB\xf0 \xb8\x9a\\\xde\xeej\xb3\x7f\xbc\xefv\xe8" +
	"\x90jNzs\xd6\x92\x93\x025\xdb\xcb\x10\xf3\xbck" +
	"\xc7\xdc'^\x00\xd4\xe3\x92\xea\xbc`S\x881w\xbb" +
	"\x0b\xa8\xe7$UG\xb0\xf0\xc8vM\x98\\\x1a\xe8\x0c" +
	"\x00\x0f\x83\x1d\xc9\x99\xb5\x0f\x83\xb6kt\x18\x9d5\x97" +
	"Q\xdb\xeef\xfb\x1e\xdd\xabo=\xed\xb7\x93\xdc\xbc\x8b" +
	"Ba\xbdP8\xb6\x7f\xca\x87\xb1\xc2\xc2\xff\x17K\x85" +
	"\xa7\x05\x1b\xdda\xda\xad&\xc7I\xa4\xafl\xa6\x11\xa8" +
	"\xe9O\xda<\x13\x82\x98\x0da\xcc\xebK\x8f,h\xc3" +
	"5@mI\xaa\x81\xb3\xa0L];-oJ\xaa\xa1" +
	" %\xa7\xfe\xabf\xbc\x06\xe1{\x0b\xf4\x80\xa62\x80" +
	"\xeaH\xaa-\xc1\x1d'n#\x1c\xdd[\x88\x86I\xd3" +
	"\x9c\xbe\xfd\xc2\xfc7\xf8\xf1\xea\xf5\xef\xca\xeb\x9d\xb2\x0d" +
	"\xb6Wv\x17\x0f\x1bW^\xd7\\\xd6\xa7V\xe0\xfe\x0a" +
	"\x8dg\xa3\xc8\x94\xb9\x96\xea\xdb\xae'g$\xd5z\x19" +
	"\xa0\xbb|\xbe5\x09\xd0\x17\xe3\xa47\x9c\x8f\xeb\x92\xea" +
	"\xa2\xe0j\xda\xebe:\xaf*5\xd4I?\x1fT\xbc" +
	"\xc3\xb4\xbf\xe9J\x06V5\xfb?\x00\x00\xff\xff@\x03" +
	"4\x95"

func init() {
	schemas.Register(schema_dc976fcd6fee6223,
		0x919dc1c628df5842,
		0x9867fe7d1383e188,
		0xaf1485be712710cc,
		0xcc6f8e43c0d837f7,
		0xd848462725353ce8,
		0xe09672577fe40a61,
		0xe45f1d3ad96f0c55)
}
