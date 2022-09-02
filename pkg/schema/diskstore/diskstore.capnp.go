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

type Manifest capnp.Struct
type Manifest_lastLog Manifest

// Manifest_TypeID is the unique identifier for the type Manifest.
const Manifest_TypeID = 0xe09672577fe40a61

func NewManifest(s *capnp.Segment) (Manifest, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 24, PointerCount: 2})
	return Manifest(st), err
}

func NewRootManifest(s *capnp.Segment) (Manifest, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 24, PointerCount: 2})
	return Manifest(st), err
}

func ReadRootManifest(msg *capnp.Message) (Manifest, error) {
	root, err := msg.Root()
	return Manifest(root.Struct()), err
}

func (s Manifest) String() string {
	str, _ := text.Marshal(0xe09672577fe40a61, capnp.Struct(s))
	return str
}

func (s Manifest) EncodeAsPtr(seg *capnp.Segment) capnp.Ptr {
	return capnp.Struct(s).EncodeAsPtr(seg)
}

func (Manifest) DecodeFromPtr(p capnp.Ptr) Manifest {
	return Manifest(capnp.Struct{}.DecodeFromPtr(p))
}

func (s Manifest) ToPtr() capnp.Ptr {
	return capnp.Struct(s).ToPtr()
}
func (s Manifest) IsValid() bool {
	return capnp.Struct(s).IsValid()
}

func (s Manifest) Message() *capnp.Message {
	return capnp.Struct(s).Message()
}

func (s Manifest) Segment() *capnp.Segment {
	return capnp.Struct(s).Segment()
}
func (s Manifest) FormatVersion() uint32 {
	return capnp.Struct(s).Uint32(16)
}

func (s Manifest) SetFormatVersion(v uint32) {
	capnp.Struct(s).SetUint32(16, v)
}

func (s Manifest) BlobMap() (Addr, error) {
	p, err := capnp.Struct(s).Ptr(0)
	return Addr(p.Struct()), err
}

func (s Manifest) HasBlobMap() bool {
	return capnp.Struct(s).HasPtr(0)
}

func (s Manifest) SetBlobMap(v Addr) error {
	return capnp.Struct(s).SetPtr(0, capnp.Struct(v).ToPtr())
}

// NewBlobMap sets the blobMap field to a newly
// allocated Addr struct, preferring placement in s's segment.
func (s Manifest) NewBlobMap() (Addr, error) {
	ss, err := NewAddr(capnp.Struct(s).Segment())
	if err != nil {
		return Addr{}, err
	}
	err = capnp.Struct(s).SetPtr(0, capnp.Struct(ss).ToPtr())
	return ss, err
}

func (s Manifest) Root() (protocol.ContentId, error) {
	p, err := capnp.Struct(s).Ptr(1)
	return protocol.ContentId(p.Struct()), err
}

func (s Manifest) HasRoot() bool {
	return capnp.Struct(s).HasPtr(1)
}

func (s Manifest) SetRoot(v protocol.ContentId) error {
	return capnp.Struct(s).SetPtr(1, capnp.Struct(v).ToPtr())
}

// NewRoot sets the root field to a newly
// allocated protocol.ContentId struct, preferring placement in s's segment.
func (s Manifest) NewRoot() (protocol.ContentId, error) {
	ss, err := protocol.NewContentId(capnp.Struct(s).Segment())
	if err != nil {
		return protocol.ContentId{}, err
	}
	err = capnp.Struct(s).SetPtr(1, capnp.Struct(ss).ToPtr())
	return ss, err
}

func (s Manifest) LastLog() Manifest_lastLog { return Manifest_lastLog(s) }

func (s Manifest_lastLog) IsValid() bool {
	return capnp.Struct(s).IsValid()
}

func (s Manifest_lastLog) Message() *capnp.Message {
	return capnp.Struct(s).Message()
}

func (s Manifest_lastLog) Segment() *capnp.Segment {
	return capnp.Struct(s).Segment()
}
func (s Manifest_lastLog) Size() uint64 {
	return capnp.Struct(s).Uint64(0)
}

func (s Manifest_lastLog) SetSize(v uint64) {
	capnp.Struct(s).SetUint64(0, v)
}

func (s Manifest_lastLog) Number() uint64 {
	return capnp.Struct(s).Uint64(8)
}

func (s Manifest_lastLog) SetNumber(v uint64) {
	capnp.Struct(s).SetUint64(8, v)
}

// Manifest_List is a list of Manifest.
type Manifest_List = capnp.StructList[Manifest]

// NewManifest creates a new list of Manifest.
func NewManifest_List(s *capnp.Segment, sz int32) (Manifest_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 24, PointerCount: 2}, sz)
	return capnp.StructList[Manifest](l), err
}

// Manifest_Future is a wrapper for a Manifest promised by a client call.
type Manifest_Future struct{ *capnp.Future }

func (p Manifest_Future) Struct() (Manifest, error) {
	s, err := p.Future.Struct()
	return Manifest(s), err
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
	return Manifest_lastLog(s), err
}

type Addr capnp.Struct

// Addr_TypeID is the unique identifier for the type Addr.
const Addr_TypeID = 0xe45f1d3ad96f0c55

func NewAddr(s *capnp.Segment) (Addr, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 24, PointerCount: 0})
	return Addr(st), err
}

func NewRootAddr(s *capnp.Segment) (Addr, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 24, PointerCount: 0})
	return Addr(st), err
}

func ReadRootAddr(msg *capnp.Message) (Addr, error) {
	root, err := msg.Root()
	return Addr(root.Struct()), err
}

func (s Addr) String() string {
	str, _ := text.Marshal(0xe45f1d3ad96f0c55, capnp.Struct(s))
	return str
}

func (s Addr) EncodeAsPtr(seg *capnp.Segment) capnp.Ptr {
	return capnp.Struct(s).EncodeAsPtr(seg)
}

func (Addr) DecodeFromPtr(p capnp.Ptr) Addr {
	return Addr(capnp.Struct{}.DecodeFromPtr(p))
}

func (s Addr) ToPtr() capnp.Ptr {
	return capnp.Struct(s).ToPtr()
}
func (s Addr) IsValid() bool {
	return capnp.Struct(s).IsValid()
}

func (s Addr) Message() *capnp.Message {
	return capnp.Struct(s).Message()
}

func (s Addr) Segment() *capnp.Segment {
	return capnp.Struct(s).Segment()
}
func (s Addr) LogNumber() uint64 {
	return capnp.Struct(s).Uint64(16)
}

func (s Addr) SetLogNumber(v uint64) {
	capnp.Struct(s).SetUint64(16, v)
}

func (s Addr) Offset() uint64 {
	return capnp.Struct(s).Uint64(0)
}

func (s Addr) SetOffset(v uint64) {
	capnp.Struct(s).SetUint64(0, v)
}

func (s Addr) Length() uint32 {
	return capnp.Struct(s).Uint32(8)
}

func (s Addr) SetLength(v uint32) {
	capnp.Struct(s).SetUint32(8, v)
}

// Addr_List is a list of Addr.
type Addr_List = capnp.StructList[Addr]

// NewAddr creates a new list of Addr.
func NewAddr_List(s *capnp.Segment, sz int32) (Addr_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 24, PointerCount: 0}, sz)
	return capnp.StructList[Addr](l), err
}

// Addr_Future is a wrapper for a Addr promised by a client call.
type Addr_Future struct{ *capnp.Future }

func (p Addr_Future) Struct() (Addr, error) {
	s, err := p.Future.Struct()
	return Addr(s), err
}

type LogEntry capnp.Struct
type LogEntry_blob LogEntry
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
	return LogEntry(st), err
}

func NewRootLogEntry(s *capnp.Segment) (LogEntry, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 1})
	return LogEntry(st), err
}

func ReadRootLogEntry(msg *capnp.Message) (LogEntry, error) {
	root, err := msg.Root()
	return LogEntry(root.Struct()), err
}

func (s LogEntry) String() string {
	str, _ := text.Marshal(0xd848462725353ce8, capnp.Struct(s))
	return str
}

func (s LogEntry) EncodeAsPtr(seg *capnp.Segment) capnp.Ptr {
	return capnp.Struct(s).EncodeAsPtr(seg)
}

func (LogEntry) DecodeFromPtr(p capnp.Ptr) LogEntry {
	return LogEntry(capnp.Struct{}.DecodeFromPtr(p))
}

func (s LogEntry) ToPtr() capnp.Ptr {
	return capnp.Struct(s).ToPtr()
}

func (s LogEntry) Which() LogEntry_Which {
	return LogEntry_Which(capnp.Struct(s).Uint16(0))
}
func (s LogEntry) IsValid() bool {
	return capnp.Struct(s).IsValid()
}

func (s LogEntry) Message() *capnp.Message {
	return capnp.Struct(s).Message()
}

func (s LogEntry) Segment() *capnp.Segment {
	return capnp.Struct(s).Segment()
}
func (s LogEntry) Blob() LogEntry_blob { return LogEntry_blob(s) }

func (s LogEntry) SetBlob() {
	capnp.Struct(s).SetUint16(0, 0)
}

func (s LogEntry_blob) IsValid() bool {
	return capnp.Struct(s).IsValid()
}

func (s LogEntry_blob) Message() *capnp.Message {
	return capnp.Struct(s).Message()
}

func (s LogEntry_blob) Segment() *capnp.Segment {
	return capnp.Struct(s).Segment()
}
func (s LogEntry_blob) Segment_() ([]byte, error) {
	p, err := capnp.Struct(s).Ptr(0)
	return []byte(p.Data()), err
}

func (s LogEntry_blob) HasSegment_() bool {
	return capnp.Struct(s).HasPtr(0)
}

func (s LogEntry_blob) SetSegment_(v []byte) error {
	return capnp.Struct(s).SetData(0, v)
}

func (s LogEntry_blob) Compression() CompressionScheme {
	return CompressionScheme(capnp.Struct(s).Uint16(2))
}

func (s LogEntry_blob) SetCompression(v CompressionScheme) {
	capnp.Struct(s).SetUint16(2, uint16(v))
}

func (s LogEntry_blob) Packed() bool {
	return capnp.Struct(s).Bit(32)
}

func (s LogEntry_blob) SetPacked(v bool) {
	capnp.Struct(s).SetBit(32, v)
}

func (s LogEntry) IndexNode() (TrieMap, error) {
	if capnp.Struct(s).Uint16(0) != 1 {
		panic("Which() != indexNode")
	}
	p, err := capnp.Struct(s).Ptr(0)
	return TrieMap(p.Struct()), err
}

func (s LogEntry) HasIndexNode() bool {
	if capnp.Struct(s).Uint16(0) != 1 {
		return false
	}
	return capnp.Struct(s).HasPtr(0)
}

func (s LogEntry) SetIndexNode(v TrieMap) error {
	capnp.Struct(s).SetUint16(0, 1)
	return capnp.Struct(s).SetPtr(0, capnp.Struct(v).ToPtr())
}

// NewIndexNode sets the indexNode field to a newly
// allocated TrieMap struct, preferring placement in s's segment.
func (s LogEntry) NewIndexNode() (TrieMap, error) {
	capnp.Struct(s).SetUint16(0, 1)
	ss, err := NewTrieMap(capnp.Struct(s).Segment())
	if err != nil {
		return TrieMap{}, err
	}
	err = capnp.Struct(s).SetPtr(0, capnp.Struct(ss).ToPtr())
	return ss, err
}

// LogEntry_List is a list of LogEntry.
type LogEntry_List = capnp.StructList[LogEntry]

// NewLogEntry creates a new list of LogEntry.
func NewLogEntry_List(s *capnp.Segment, sz int32) (LogEntry_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 8, PointerCount: 1}, sz)
	return capnp.StructList[LogEntry](l), err
}

// LogEntry_Future is a wrapper for a LogEntry promised by a client call.
type LogEntry_Future struct{ *capnp.Future }

func (p LogEntry_Future) Struct() (LogEntry, error) {
	s, err := p.Future.Struct()
	return LogEntry(s), err
}

func (p LogEntry_Future) Blob() LogEntry_blob_Future { return LogEntry_blob_Future{p.Future} }

// LogEntry_blob_Future is a wrapper for a LogEntry_blob promised by a client call.
type LogEntry_blob_Future struct{ *capnp.Future }

func (p LogEntry_blob_Future) Struct() (LogEntry_blob, error) {
	s, err := p.Future.Struct()
	return LogEntry_blob(s), err
}

func (p LogEntry_Future) IndexNode() TrieMap_Future {
	return TrieMap_Future{Future: p.Future.Field(0, nil)}
}

type CompressionScheme uint16

// CompressionScheme_TypeID is the unique identifier for the type CompressionScheme.
const CompressionScheme_TypeID = 0xa9e5a7728baf3d2e

// Values of CompressionScheme.
const (
	CompressionScheme_none CompressionScheme = 0
	CompressionScheme_xz   CompressionScheme = 1
)

// String returns the enum's constant name.
func (c CompressionScheme) String() string {
	switch c {
	case CompressionScheme_none:
		return "none"
	case CompressionScheme_xz:
		return "xz"

	default:
		return ""
	}
}

// CompressionSchemeFromString returns the enum value with a name,
// or the zero value if there's no such value.
func CompressionSchemeFromString(c string) CompressionScheme {
	switch c {
	case "none":
		return CompressionScheme_none
	case "xz":
		return CompressionScheme_xz

	default:
		return 0
	}
}

type CompressionScheme_List = capnp.EnumList[CompressionScheme]

func NewCompressionScheme_List(s *capnp.Segment, sz int32) (CompressionScheme_List, error) {
	return capnp.NewEnumList[CompressionScheme](s, sz)
}

type TrieMap capnp.Struct
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
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2})
	return TrieMap(st), err
}

func NewRootTrieMap(s *capnp.Segment) (TrieMap, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2})
	return TrieMap(st), err
}

func ReadRootTrieMap(msg *capnp.Message) (TrieMap, error) {
	root, err := msg.Root()
	return TrieMap(root.Struct()), err
}

func (s TrieMap) String() string {
	str, _ := text.Marshal(0xcc6f8e43c0d837f7, capnp.Struct(s))
	return str
}

func (s TrieMap) EncodeAsPtr(seg *capnp.Segment) capnp.Ptr {
	return capnp.Struct(s).EncodeAsPtr(seg)
}

func (TrieMap) DecodeFromPtr(p capnp.Ptr) TrieMap {
	return TrieMap(capnp.Struct{}.DecodeFromPtr(p))
}

func (s TrieMap) ToPtr() capnp.Ptr {
	return capnp.Struct(s).ToPtr()
}

func (s TrieMap) Which() TrieMap_Which {
	return TrieMap_Which(capnp.Struct(s).Uint16(0))
}
func (s TrieMap) IsValid() bool {
	return capnp.Struct(s).IsValid()
}

func (s TrieMap) Message() *capnp.Message {
	return capnp.Struct(s).Message()
}

func (s TrieMap) Segment() *capnp.Segment {
	return capnp.Struct(s).Segment()
}
func (s TrieMap) Leaf() TrieMap_leaf { return TrieMap_leaf(s) }

func (s TrieMap) SetLeaf() {
	capnp.Struct(s).SetUint16(0, 0)
}

func (s TrieMap_leaf) IsValid() bool {
	return capnp.Struct(s).IsValid()
}

func (s TrieMap_leaf) Message() *capnp.Message {
	return capnp.Struct(s).Message()
}

func (s TrieMap_leaf) Segment() *capnp.Segment {
	return capnp.Struct(s).Segment()
}
func (s TrieMap_leaf) Prefix() ([]byte, error) {
	p, err := capnp.Struct(s).Ptr(0)
	return []byte(p.Data()), err
}

func (s TrieMap_leaf) HasPrefix() bool {
	return capnp.Struct(s).HasPtr(0)
}

func (s TrieMap_leaf) SetPrefix(v []byte) error {
	return capnp.Struct(s).SetData(0, v)
}

func (s TrieMap_leaf) Addr() (Addr, error) {
	p, err := capnp.Struct(s).Ptr(1)
	return Addr(p.Struct()), err
}

func (s TrieMap_leaf) HasAddr() bool {
	return capnp.Struct(s).HasPtr(1)
}

func (s TrieMap_leaf) SetAddr(v Addr) error {
	return capnp.Struct(s).SetPtr(1, capnp.Struct(v).ToPtr())
}

// NewAddr sets the addr field to a newly
// allocated Addr struct, preferring placement in s's segment.
func (s TrieMap_leaf) NewAddr() (Addr, error) {
	ss, err := NewAddr(capnp.Struct(s).Segment())
	if err != nil {
		return Addr{}, err
	}
	err = capnp.Struct(s).SetPtr(1, capnp.Struct(ss).ToPtr())
	return ss, err
}

func (s TrieMap) Branches() (Addr_List, error) {
	if capnp.Struct(s).Uint16(0) != 1 {
		panic("Which() != branches")
	}
	p, err := capnp.Struct(s).Ptr(0)
	return Addr_List(p.List()), err
}

func (s TrieMap) HasBranches() bool {
	if capnp.Struct(s).Uint16(0) != 1 {
		return false
	}
	return capnp.Struct(s).HasPtr(0)
}

func (s TrieMap) SetBranches(v Addr_List) error {
	capnp.Struct(s).SetUint16(0, 1)
	return capnp.Struct(s).SetPtr(0, v.ToPtr())
}

// NewBranches sets the branches field to a newly
// allocated Addr_List, preferring placement in s's segment.
func (s TrieMap) NewBranches(n int32) (Addr_List, error) {
	capnp.Struct(s).SetUint16(0, 1)
	l, err := NewAddr_List(capnp.Struct(s).Segment(), n)
	if err != nil {
		return Addr_List{}, err
	}
	err = capnp.Struct(s).SetPtr(0, l.ToPtr())
	return l, err
}

// TrieMap_List is a list of TrieMap.
type TrieMap_List = capnp.StructList[TrieMap]

// NewTrieMap creates a new list of TrieMap.
func NewTrieMap_List(s *capnp.Segment, sz int32) (TrieMap_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2}, sz)
	return capnp.StructList[TrieMap](l), err
}

// TrieMap_Future is a wrapper for a TrieMap promised by a client call.
type TrieMap_Future struct{ *capnp.Future }

func (p TrieMap_Future) Struct() (TrieMap, error) {
	s, err := p.Future.Struct()
	return TrieMap(s), err
}

func (p TrieMap_Future) Leaf() TrieMap_leaf_Future { return TrieMap_leaf_Future{p.Future} }

// TrieMap_leaf_Future is a wrapper for a TrieMap_leaf promised by a client call.
type TrieMap_leaf_Future struct{ *capnp.Future }

func (p TrieMap_leaf_Future) Struct() (TrieMap_leaf, error) {
	s, err := p.Future.Struct()
	return TrieMap_leaf(s), err
}

func (p TrieMap_leaf_Future) Addr() Addr_Future {
	return Addr_Future{Future: p.Future.Field(1, nil)}
}

const schema_dc976fcd6fee6223 = "x\xda|T]h\x1cU\x18\xfd\xce\xbd\xb3\xee\xa6l" +
	":3\x9d\x0d\xfe`\x88\xa0\xa5M\xb0\xdbJ\x95B\xb0" +
	"\xa4\xd9v\x8b\x96\xa4\xec5\xfeT(\xb4\xb3\xbbww" +
	"\x87\xee\xcelgFL\x82%\x8a\x14DQ\xf0E}" +
	"\xf1IP\xecK\x9e}P\xb4\xa0\x0f\xa5>\x08\x0a-" +
	"X\xda@\xe3\x1f\"\xe8\x8b\xe0C\xaf\xdc\xd9\xc9l\x9a" +
	"Z\xdf\xe6^\xce\xfd\xce\xf9\xbes\xbe\xd9g\xb2C\xc6" +
	"c\xa3\xd7s\xc4\xc4\x91\xdc=\xaar\xe2\xfa\xeeo\xbe" +
	"\xfa\xf0]\xb2-C=\\\xff#\xf86x\xffG\"" +
	"\xec\xff\x81M\xc1Ygy\xa2\x85\x1b\x8cc\xe1w\xc6" +
	"@\xa4\xdeX{\xdd9w\xab\xfd\x01\x89\xfb\x00\xf5\xf7" +
	"\x81+_\x1e~'\xb8Lc,\x0f\"\xe7\x1a\xfb\x8d" +
	"\xe0\xac\xb1\x97\x09\xea\xfcOoV\xf6._\xf8\x88\xc4" +
	"\xfd\x80\xfa\xe5\xc9'v\xee:\xfa\xd4\x15\x1aC\x82<" +
	"\xc8\xff\"8\xb3|\x95\xa0\xca\x07W\xdf\x0a?Y\xff" +
	"\x94l\x8b\xdd\xa6!g\xec\x803f\xe4\x89\x1c\xdb8" +
	"@P\x97\xad]g\xbf8_Z\xd55\xb9r\xb7\xdd" +
	"\\y!|\xefF\xca\xbe\x7f\xcc\xd8\x06\x823nh" +
	"\xfaL\x9a\xb0\x80a\xd5*\xcb3\"g\xc9\xb8\xe8\xbc" +
	"\x96\x14>\x97\xa03y[\xd0H\xd0\xdf\x19\x97\x9ck" +
	"\x09\xfaj\x82\xce\x88\x85\x05>D\xe7\xf4\xb4\x9c\xf1\xdc" +
	"%g2\xa7\xbfv\xe6~&\xa8\xe7\x8a\xc1\xd5\xe9\xf1" +
	"S7\xb7\x80\x93rk\xb9\x0b\xce\xaf\x09v=\xb7J" +
	"/\xaa\xa6\x17\x9d\x89\xe2 \xe4\xb2\xdcp\xfb~\x7fZ" +
	"\x1f\xdc\xb6<\x1a\x84=7~^\x86f\xe4\x05~\x0d" +
	"@\x81\x98]\x00!{\xc26\x9e<\x1bzr\xde\xed" +
	"\x97\xcd\xaet[\xa2\xc0\x0d\"\x03D\xf6\xe44\x91x" +
	"\x84C\xecc\xb0\x81\x12\xf4\xe5\x9e)\"\xb1\x9bC<" +
	"\xce0\xd3\x0fe\xcb[\xc4(1\x8c\x12L\xb7\xd9\x0c" +
	"a\x0d\xf5\x13`\xfd\x17\xe1\\\xd0\xae\xfaq\xb84Q" +
	"\xaew\x83\xba(f\x8c\xd5\x8a]\x9d\x10\x1d\x0e\x11\xa7" +
	"\x94\x8c\xc8>['\x12}\x0e\xf1\x0a\x83\xcd\x1e*\x81" +
	"\x13\xd9KZ\\\xcc!^eX\x89d\xbb'\xfdX" +
	"\x18`\xea\xcf\xb7\xf7\xde\xbb\xe3\xf4g\x17I\x18\x0c\xb3" +
	"%\xa0Hd\xe3\x98J1\xa7\xb4\xaaT\xb0j\x04\xbd" +
	"~(\xa3\x88\xf2^\xe0\xc3\x1c\xe6\x8a\x00\x930\xd3w" +
	"\x1bgd\x13 \x86\xcd\x93\xcb\x86}8}\xef\x05\xfe" +
	"B\xa3#{\x905@\x14\x12\xd5\xf6\x94\xaeb\x8f<" +
	"@d\xfa\x81/\xf9\xe2\xf2\x9d\x05\xe6]\xdfk\xc9(" +
	".w\xdd(\x9e\x0b\xdaD\x83\xf9\x0f\x1a\x9f\x9c\xdab" +
	"\x80n|\xcf\xf4\xd0\x003\xf2\x96%F\x88a\x840" +
	"\xe3\xbf\xd4\xab\xcbp\xe3xW\x9f)\xd1\xc8\x8d\xa2R" +
	"\xc0p=5\x1b\x1b\xc5-5\xe0\xb6\x8f\x11\x09\x8bC" +
	"<\xca\x90\x04C\xd5C\xd7otdDD\xd8N\xa8" +
	"ql\xf1z\xfb\xffyM\xb7\xd3f\xbb\xbe\x89V'" +
	"\xc0~&\xa5}\x90\xc1\xd4\xe9P\x9e\xdf\x94\x8b\xc7\x83" +
	"&A\xc2\x1a.\xe9\xdd\xb2\xb51\xd1\x01\x9f\xc5\x0d " +
	"\x89\x96[!\x12'9DG\x87(\x0d\xb3\xd4\x03>" +
	"\xcd!\xba\x0c\xe0\xd8\xf4\xbb\xb0\xbd\x0a1\xcb(\xc1 " +
	"\xb2EH$j\x1c\xe2$\xc3\x8a\x165\xef\xf6\xef\xcc" +
	"\xb9\x19\x06A\x0cK}\x1c\xfe\xd3\xf9~\xf9\xf3\xaf\xd3" +
	"\xeb\x95\xd4Z\xd5JW\x92&B\x1d\x18\xbd\x90(l" +
	"j\x01\x1b-\x98\xb3\xcdf\xa8\xd5\x173\xf5Um\xfa" +
	"!\x0e1\xa7\xd5\xb3\x81\xfa\xa7\xf5\xe5\x11\x0eQc\xb0" +
	"\xd8\xc0\xb6y=\xbf9\x0eq\x82a&h\xb5\"\x19" +
	"g\xf9\xe8J\xbf\x1dw2\xden\xd0>\xae\x13C\xc8" +
	"2\xf3o\x00\x00\x00\xff\xff-\x1e\x8dP"

func init() {
	schemas.Register(schema_dc976fcd6fee6223,
		0x919dc1c628df5842,
		0x9867fe7d1383e188,
		0xa3aa7a2f428ae685,
		0xa9e5a7728baf3d2e,
		0xaf1485be712710cc,
		0xcc6f8e43c0d837f7,
		0xd848462725353ce8,
		0xe09672577fe40a61,
		0xe45f1d3ad96f0c55)
}
