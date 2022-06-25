// Code generated by capnpc-go. DO NOT EDIT.

package files

import (
	capnp "capnproto.org/go/capnp/v3"
	text "capnproto.org/go/capnp/v3/encoding/text"
	schemas "capnproto.org/go/capnp/v3/schemas"
	strconv "strconv"
	protocol "zenhack.net/go/ocap-md/pkg/schema/protocol"
)

type File struct{ capnp.Struct }
type File_metadata File
type File_Which uint16

const (
	File_Which_file    File_Which = 0
	File_Which_dir     File_Which = 2
	File_Which_symlink File_Which = 1
)

func (w File_Which) String() string {
	const s = "filedirsymlink"
	switch w {
	case File_Which_file:
		return s[0:4]
	case File_Which_dir:
		return s[4:7]
	case File_Which_symlink:
		return s[7:14]

	}
	return "File_Which(" + strconv.FormatUint(uint64(w), 10) + ")"
}

type File_metadata_Which uint16

const (
	File_metadata_Which_noMetadata   File_metadata_Which = 0
	File_metadata_Which_unixMetadata File_metadata_Which = 1
)

func (w File_metadata_Which) String() string {
	const s = "noMetadataunixMetadata"
	switch w {
	case File_metadata_Which_noMetadata:
		return s[0:10]
	case File_metadata_Which_unixMetadata:
		return s[10:22]

	}
	return "File_metadata_Which(" + strconv.FormatUint(uint64(w), 10) + ")"
}

// File_TypeID is the unique identifier for the type File.
const File_TypeID = 0xc4abcc30dd3a9483

func NewFile(s *capnp.Segment) (File, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2})
	return File{st}, err
}

func NewRootFile(s *capnp.Segment) (File, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2})
	return File{st}, err
}

func ReadRootFile(msg *capnp.Message) (File, error) {
	root, err := msg.Root()
	return File{root.Struct()}, err
}

func (s File) String() string {
	str, _ := text.Marshal(0xc4abcc30dd3a9483, s.Struct)
	return str
}

func (s File) Which() File_Which {
	return File_Which(s.Struct.Uint16(0))
}
func (s File) Metadata() File_metadata { return File_metadata(s) }

func (s File_metadata) Which() File_metadata_Which {
	return File_metadata_Which(s.Struct.Uint16(2))
}
func (s File_metadata) SetNoMetadata() {
	s.Struct.SetUint16(2, 0)

}

func (s File_metadata) UnixMetadata() (UnixMetadata, error) {
	if s.Struct.Uint16(2) != 1 {
		panic("Which() != unixMetadata")
	}
	p, err := s.Struct.Ptr(1)
	return UnixMetadata{Struct: p.Struct()}, err
}

func (s File_metadata) HasUnixMetadata() bool {
	if s.Struct.Uint16(2) != 1 {
		return false
	}
	return s.Struct.HasPtr(1)
}

func (s File_metadata) SetUnixMetadata(v UnixMetadata) error {
	s.Struct.SetUint16(2, 1)
	return s.Struct.SetPtr(1, v.Struct.ToPtr())
}

// NewUnixMetadata sets the unixMetadata field to a newly
// allocated UnixMetadata struct, preferring placement in s's segment.
func (s File_metadata) NewUnixMetadata() (UnixMetadata, error) {
	s.Struct.SetUint16(2, 1)
	ss, err := NewUnixMetadata(s.Struct.Segment())
	if err != nil {
		return UnixMetadata{}, err
	}
	err = s.Struct.SetPtr(1, ss.Struct.ToPtr())
	return ss, err
}

func (s File) File() (BlobTree, error) {
	if s.Struct.Uint16(0) != 0 {
		panic("Which() != file")
	}
	p, err := s.Struct.Ptr(0)
	return BlobTree{Struct: p.Struct()}, err
}

func (s File) HasFile() bool {
	if s.Struct.Uint16(0) != 0 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s File) SetFile(v BlobTree) error {
	s.Struct.SetUint16(0, 0)
	return s.Struct.SetPtr(0, v.Struct.ToPtr())
}

// NewFile sets the file field to a newly
// allocated BlobTree struct, preferring placement in s's segment.
func (s File) NewFile() (BlobTree, error) {
	s.Struct.SetUint16(0, 0)
	ss, err := NewBlobTree(s.Struct.Segment())
	if err != nil {
		return BlobTree{}, err
	}
	err = s.Struct.SetPtr(0, ss.Struct.ToPtr())
	return ss, err
}

func (s File) Dir() protocol.Ref {
	if s.Struct.Uint16(0) != 2 {
		panic("Which() != dir")
	}
	p, _ := s.Struct.Ptr(0)
	return protocol.Ref{Client: p.Interface().Client()}
}

func (s File) HasDir() bool {
	if s.Struct.Uint16(0) != 2 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s File) SetDir(v protocol.Ref) error {
	s.Struct.SetUint16(0, 2)
	if !v.Client.IsValid() {
		return s.Struct.SetPtr(0, capnp.Ptr{})
	}
	seg := s.Segment()
	in := capnp.NewInterface(seg, seg.Message().AddCap(v.Client))
	return s.Struct.SetPtr(0, in.ToPtr())
}

func (s File) Symlink() (string, error) {
	if s.Struct.Uint16(0) != 1 {
		panic("Which() != symlink")
	}
	p, err := s.Struct.Ptr(0)
	return p.Text(), err
}

func (s File) HasSymlink() bool {
	if s.Struct.Uint16(0) != 1 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s File) SymlinkBytes() ([]byte, error) {
	p, err := s.Struct.Ptr(0)
	return p.TextBytes(), err
}

func (s File) SetSymlink(v string) error {
	s.Struct.SetUint16(0, 1)
	return s.Struct.SetText(0, v)
}

// File_List is a list of File.
type File_List = capnp.StructList[File]

// NewFile creates a new list of File.
func NewFile_List(s *capnp.Segment, sz int32) (File_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2}, sz)
	return capnp.StructList[File]{List: l}, err
}

// File_Future is a wrapper for a File promised by a client call.
type File_Future struct{ *capnp.Future }

func (p File_Future) Struct() (File, error) {
	s, err := p.Future.Struct()
	return File{s}, err
}

func (p File_Future) Metadata() File_metadata_Future { return File_metadata_Future{p.Future} }

// File_metadata_Future is a wrapper for a File_metadata promised by a client call.
type File_metadata_Future struct{ *capnp.Future }

func (p File_metadata_Future) Struct() (File_metadata, error) {
	s, err := p.Future.Struct()
	return File_metadata{s}, err
}

func (p File_metadata_Future) UnixMetadata() UnixMetadata_Future {
	return UnixMetadata_Future{Future: p.Future.Field(1, nil)}
}

func (p File_Future) File() BlobTree_Future {
	return BlobTree_Future{Future: p.Future.Field(0, nil)}
}

func (p File_Future) Dir() protocol.Ref {
	return protocol.Ref{Client: p.Future.Field(0, nil).Client()}
}

type UnixMetadata struct{ capnp.Struct }

// UnixMetadata_TypeID is the unique identifier for the type UnixMetadata.
const UnixMetadata_TypeID = 0xeab612050895537a

func NewUnixMetadata(s *capnp.Segment) (UnixMetadata, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 16, PointerCount: 0})
	return UnixMetadata{st}, err
}

func NewRootUnixMetadata(s *capnp.Segment) (UnixMetadata, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 16, PointerCount: 0})
	return UnixMetadata{st}, err
}

func ReadRootUnixMetadata(msg *capnp.Message) (UnixMetadata, error) {
	root, err := msg.Root()
	return UnixMetadata{root.Struct()}, err
}

func (s UnixMetadata) String() string {
	str, _ := text.Marshal(0xeab612050895537a, s.Struct)
	return str
}

func (s UnixMetadata) Permissions() uint32 {
	return s.Struct.Uint32(0)
}

func (s UnixMetadata) SetPermissions(v uint32) {
	s.Struct.SetUint32(0, v)
}

func (s UnixMetadata) ModTime() int64 {
	return int64(s.Struct.Uint64(8))
}

func (s UnixMetadata) SetModTime(v int64) {
	s.Struct.SetUint64(8, uint64(v))
}

// UnixMetadata_List is a list of UnixMetadata.
type UnixMetadata_List = capnp.StructList[UnixMetadata]

// NewUnixMetadata creates a new list of UnixMetadata.
func NewUnixMetadata_List(s *capnp.Segment, sz int32) (UnixMetadata_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 16, PointerCount: 0}, sz)
	return capnp.StructList[UnixMetadata]{List: l}, err
}

// UnixMetadata_Future is a wrapper for a UnixMetadata promised by a client call.
type UnixMetadata_Future struct{ *capnp.Future }

func (p UnixMetadata_Future) Struct() (UnixMetadata, error) {
	s, err := p.Future.Struct()
	return UnixMetadata{s}, err
}

type BlobTree struct{ capnp.Struct }
type BlobTree_Which uint16

const (
	BlobTree_Which_leaf   BlobTree_Which = 0
	BlobTree_Which_branch BlobTree_Which = 1
)

func (w BlobTree_Which) String() string {
	const s = "leafbranch"
	switch w {
	case BlobTree_Which_leaf:
		return s[0:4]
	case BlobTree_Which_branch:
		return s[4:10]

	}
	return "BlobTree_Which(" + strconv.FormatUint(uint64(w), 10) + ")"
}

// BlobTree_TypeID is the unique identifier for the type BlobTree.
const BlobTree_TypeID = 0xfde2c325eeeb9f0a

func NewBlobTree(s *capnp.Segment) (BlobTree, error) {
	st, err := capnp.NewStruct(s, capnp.ObjectSize{DataSize: 16, PointerCount: 1})
	return BlobTree{st}, err
}

func NewRootBlobTree(s *capnp.Segment) (BlobTree, error) {
	st, err := capnp.NewRootStruct(s, capnp.ObjectSize{DataSize: 16, PointerCount: 1})
	return BlobTree{st}, err
}

func ReadRootBlobTree(msg *capnp.Message) (BlobTree, error) {
	root, err := msg.Root()
	return BlobTree{root.Struct()}, err
}

func (s BlobTree) String() string {
	str, _ := text.Marshal(0xfde2c325eeeb9f0a, s.Struct)
	return str
}

func (s BlobTree) Which() BlobTree_Which {
	return BlobTree_Which(s.Struct.Uint16(0))
}
func (s BlobTree) Leaf() protocol.Ref {
	if s.Struct.Uint16(0) != 0 {
		panic("Which() != leaf")
	}
	p, _ := s.Struct.Ptr(0)
	return protocol.Ref{Client: p.Interface().Client()}
}

func (s BlobTree) HasLeaf() bool {
	if s.Struct.Uint16(0) != 0 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s BlobTree) SetLeaf(v protocol.Ref) error {
	s.Struct.SetUint16(0, 0)
	if !v.Client.IsValid() {
		return s.Struct.SetPtr(0, capnp.Ptr{})
	}
	seg := s.Segment()
	in := capnp.NewInterface(seg, seg.Message().AddCap(v.Client))
	return s.Struct.SetPtr(0, in.ToPtr())
}

func (s BlobTree) Branch() protocol.Ref {
	if s.Struct.Uint16(0) != 1 {
		panic("Which() != branch")
	}
	p, _ := s.Struct.Ptr(0)
	return protocol.Ref{Client: p.Interface().Client()}
}

func (s BlobTree) HasBranch() bool {
	if s.Struct.Uint16(0) != 1 {
		return false
	}
	return s.Struct.HasPtr(0)
}

func (s BlobTree) SetBranch(v protocol.Ref) error {
	s.Struct.SetUint16(0, 1)
	if !v.Client.IsValid() {
		return s.Struct.SetPtr(0, capnp.Ptr{})
	}
	seg := s.Segment()
	in := capnp.NewInterface(seg, seg.Message().AddCap(v.Client))
	return s.Struct.SetPtr(0, in.ToPtr())
}

func (s BlobTree) Size() uint64 {
	return s.Struct.Uint64(8)
}

func (s BlobTree) SetSize(v uint64) {
	s.Struct.SetUint64(8, v)
}

// BlobTree_List is a list of BlobTree.
type BlobTree_List = capnp.StructList[BlobTree]

// NewBlobTree creates a new list of BlobTree.
func NewBlobTree_List(s *capnp.Segment, sz int32) (BlobTree_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 16, PointerCount: 1}, sz)
	return capnp.StructList[BlobTree]{List: l}, err
}

// BlobTree_Future is a wrapper for a BlobTree promised by a client call.
type BlobTree_Future struct{ *capnp.Future }

func (p BlobTree_Future) Struct() (BlobTree, error) {
	s, err := p.Future.Struct()
	return BlobTree{s}, err
}

func (p BlobTree_Future) Leaf() protocol.Ref {
	return protocol.Ref{Client: p.Future.Field(0, nil).Client()}
}

func (p BlobTree_Future) Branch() protocol.Ref {
	return protocol.Ref{Client: p.Future.Field(0, nil).Client()}
}

const schema_ae1c58020a8c26ad = "x\xda\x94\x92?hSQ\x14\xc6\xbf\xef\xde\xf7\xfa\xd2" +
	"\x92\x98\\_\xc0E\x11\x04\xab\x95R\x1ap\x90\"X" +
	"\x0a\x11\x15\xab\xb9\xd8B\x91:\xbc6\xaf\xf4i\xfe\x94" +
	"$\xa2-J\x07qP\x1c\x1c\xac\xb8\x89\xab\xa2\xe2\"" +
	"\x0e\x0a\x82\x1dD\x9c\x8ak\x07\x15\x11\x15uqpi" +
	"\xae\xdc\xd7hb\xb7n\x8f\xf3\xbes\xbfs\xbe\xdf\x19" +
	"\xfc\xc5a'\x97\xfa\xe9A\xe8I\xb7\xcb\x9cP\x1f\xf5" +
	"\x91\xe6\x93%hE\x9a+\xb7\x86V\x07\xdf>X\xc6" +
	"\xb8\xf0(H\x7fU\xae\x80\xfe{y\x01\x1d?u\x92" +
	"4\x0f{o\xf4\x88\x89\xed\x8f\x90\x17\x9e\x04\xfc\xbcs" +
	"\xdd\x1fu<\xc0?\xea|\x06\xcd\xc2\xa9\xa5\x84\xbb\xf5" +
	"\xe9W\xab\x16mu\xac\xb8\xec\xbe\xf1\xaf\xb9\xf6\xeb\xaa" +
	"k_\xee\xb9\xfb\xed\xc7\xeeW\x1f\xd66h\xf3\xf4\x04" +
	"\xe0\x7fr\xef\xfb\xdfc\xf5\x17\xf71V\xccLT\x0a" +
	"\xeb\x03\xd3\x81\x98\xab\xcc\x0d\x1d\x8eJ\xe1@9l\x04" +
	"\xc5\xa0\xc1@'\xa4\x934&K\x02\xaa\xef4\xa0\xf7" +
	"J\xea\xfd\x82;\xd84\xccR\x02*w\x16\xd0\x83\x92" +
	"\xfa\xa0\xa0\xa9TG\xe3^\xc8F\x80.s\xbe\x12]" +
	"\xb4\x05\xa4\x8bA#`\xa6\xbd\x05\xc8\x0c\xf8\xcf\x9b\x7f" +
	"\xbd\x81\x02\xa93\xd2I\xd1\x18\xc7\xda\x06\xfb\x00=)" +
	"\xa9g\x05m[;b\x15\x1e\x83H\xc9\xa6\xc9R\x00" +
	"J\x8f\x00\xfa\xb8\xa4\x9e\x10L\x895\x93\xa5\x03\xa8\xf1" +
	"]\x80.H\xeaw\x82ik\xc7L;\x9f\xd6\x14\xad" +
	"}\x03\x00\x8b\xf5\xf9r)\xaa\x9cc\x12\x82I\xd0+" +
	"F5*\x93{~f\xbe\xbcv\xf2\x1e\x80a*\xee" +
	"\xd4\x8e`gQq\x9bvH\xb2 \xc9\x8c\xb9\xf9z" +
	"\xf2\xf7\x9d\xfe\xde\xdb\xff\xc9;\x8a\x8a{t\xc2\xcau" +
	"B2\xde\x97\xb1a\xa6}\x14 UG@1\x9c\xf1" +
	"V\x9c\xeb\xb3\x16\xe2v\x07\x88c\xea\x9bj\xd3Q\xe4" +
	":\xb2\x9c\x8d\xa4_R\x1f\x104sa\xad\x1c\xd5\xeb" +
	"\x11\xbcj\xa5\xce\x04\x04\x13\xe0b\xb9Z\x1c\x8b\xca!" +
	"]\x08\xba\x1b\x89\x8c\x94\xaaS\xe9\xb1Z\x18Z\xafd" +
	"|\x0b\xb1Y\xde2\x19\x96\xd4\x97\x04Sl\xb6\x0ed" +
	"~\x08\xd0\x0dI\xfdRP\x09\xaeSya\xa5\xcf$" +
	"\xf5\xb2`\xba\x14\x063\x9bM3\x05a\x9384U" +
	"\x0b*\xd3\xb3\x9b\xed\xde\x82\x16\x92N\xe6\x0aL\xd7\xa3" +
	"\x85\x90\xdd\x10\xec\x06\xff\x04\x00\x00\xff\xff[\x0d\xf6\xf6"

func init() {
	schemas.Register(schema_ae1c58020a8c26ad,
		0x95b1fe4851e3114e,
		0xc4abcc30dd3a9483,
		0xeab612050895537a,
		0xfde2c325eeeb9f0a)
}
