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
type File_List struct{ capnp.List }

// NewFile creates a new list of File.
func NewFile_List(s *capnp.Segment, sz int32) (File_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 8, PointerCount: 2}, sz)
	return File_List{l}, err
}

func (s File_List) At(i int) File { return File{s.List.Struct(i)} }

func (s File_List) Set(i int, v File) error { return s.List.SetStruct(i, v.Struct) }

func (s File_List) String() string {
	str, _ := text.MarshalList(0xc4abcc30dd3a9483, s.List)
	return str
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
type UnixMetadata_List struct{ capnp.List }

// NewUnixMetadata creates a new list of UnixMetadata.
func NewUnixMetadata_List(s *capnp.Segment, sz int32) (UnixMetadata_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 16, PointerCount: 0}, sz)
	return UnixMetadata_List{l}, err
}

func (s UnixMetadata_List) At(i int) UnixMetadata { return UnixMetadata{s.List.Struct(i)} }

func (s UnixMetadata_List) Set(i int, v UnixMetadata) error { return s.List.SetStruct(i, v.Struct) }

func (s UnixMetadata_List) String() string {
	str, _ := text.MarshalList(0xeab612050895537a, s.List)
	return str
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
type BlobTree_List struct{ capnp.List }

// NewBlobTree creates a new list of BlobTree.
func NewBlobTree_List(s *capnp.Segment, sz int32) (BlobTree_List, error) {
	l, err := capnp.NewCompositeList(s, capnp.ObjectSize{DataSize: 16, PointerCount: 1}, sz)
	return BlobTree_List{l}, err
}

func (s BlobTree_List) At(i int) BlobTree { return BlobTree{s.List.Struct(i)} }

func (s BlobTree_List) Set(i int, v BlobTree) error { return s.List.SetStruct(i, v.Struct) }

func (s BlobTree_List) String() string {
	str, _ := text.MarshalList(0xfde2c325eeeb9f0a, s.List)
	return str
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

const schema_ae1c58020a8c26ad = "x\xda\x94\x92Mh\x13_\x14\xc5\xcfyo&\x99\x96" +
	"\xe4\x9f\xbc\xffDDP\x04\xc1j\xa5\x94\x06\\H\x11" +
	",\x85\xaa\x88\x1fy\xd8B\x91\x0aN\x9b)\x1d\xcdG" +
	"MR4E\xe8\xc2\x9d\xb8pa\xc5\x9d\xb8TQ\xe9" +
	"Bq\xa1 \xd8\x85\x88+q\xdb\x8d.\x04\x05\xd7n" +
	"\x92'o\x1aM\xec\xae\xbb\xe1\xce\xb9\xef\xdc{~w" +
	"\xa4\xc51\x91wOx\x80\xbe\xe4&\xccY\xf5U\x9f" +
	"l\xaf\xadB+\xd2\xdc\xbc;\xba1\xf2\xf1\xc9:\xa6" +
	"D\x92\x82\xf4\xfb\x9cO\xa0\x9fv\xae\xa1\xe7\xa7N\x91" +
	"\xe6\xe9\xc0\xed~1\xbd\xfb\x19&DR\x02\xfeU\xe7" +
	"\x96\xdft\x92\x80\xbf\xe4|\x03\xcd\xf2\xf9U\xcf\xfd\xff" +
	"\xe5w\xab\x16]u\xacx\xe4~\xf0_\xb8\xf6k\xcd" +
	"\xb5/\xf7?\xf8\xf1s\xff\xbb/\xad-\xda\x09&\x05" +
	"\xe0\xab\xc4c\x7fW\xc2\xaaw$\x9ec\xc3\xccG\xa5" +
	"\xb0><\x17\x88\xc5\xca\xe2\xe8\xf1\xa8\x14\x0e\x97\xc3F" +
	"P\x0c\x1a\x0c\xb4'\x9d\x9419\x12P\x83\x17\x00}" +
	"PR\x1f\x16\xdc\xc3\xb6a\x8e\x12P\xf9\xcb\x80\x1e\x91" +
	"\xd4G\x05M\xa5z&\xee\x85l\x04H\x98\xa5Jt" +
	"\xdd\x16\x90)\x06\x8d\x80\xd9\xee\x16 \xb3\xe0_o\xfe" +
	"\xf1\x06\x0a\xa4\xceJ'Mc\x1ck\x1b\x1c\x02\xf4\x8c" +
	"\xa4^\x10\xb4m\xdd\x88Ux\x0a\"-\xdb&G\x01" +
	"(=\x0e\xe8\xd3\x92zZ0-Z&G\x07PS" +
	"\xfb\x00]\x90\xd4\x9f\x053\xd6\x8e\xd9n>\x9d):" +
	"\xfb\x06\x00V\xea\xcdr)\xaa\\a\x0a\x82)0Y" +
	"\x8cjT&\xff\xfab\xb3\xdc:\xf7\x10\xc0\x18\x15\xf7" +
	"jG\xb0\xb7\xa8\xb8S;$Y\x90d\xd6\xdcy?" +
	"\xf3\xeb\xfe\xd0\xc0\xbd\x7f\xe4=E\xc5\x03\xda\xb3r\xed" +
	"I\xc6\xfb26\xccv\x8f\x02\xa4\xea\x09(\x863\xd5" +
	"\x89ss\xd6B\xdc\xee\x00qL\x83\xb3]:\x8a\xdc" +
	"D\x96\xb7\x91\x0cI\xea#\x82f1\xac\x95\xa3z=" +
	"B\xb2Z\xa9\xd3\x83\xa0\x07\xae\x94\xab\xc5\xc9\xa8\x1c\xd2" +
	"\x85\xa0\xbb\x95\xc8x\xa9:\x9b\x99\xac\x85\xa1\xf5J\xc5" +
	"\xb7\x10\x9bMX&c\x92\xfa\x86`\x9a\xed\xce\x814" +
	"G\x01\xdd\x90\xd4o\x05\x95\xe0&\x957V\xfaJR" +
	"\xaf\x0bfJa0\xbf\xdd4\xd3\x106\x89c\xb3\xb5" +
	"\xa02\xb7\xb0\xdd\xee\xff\xd0A\xd2\xcb\\\x81\x99z\xb4" +
	"\x1c\xb2\x0f\x82}\xe0\xef\x00\x00\x00\xff\xff\x09\xfb\xf3D"

func init() {
	schemas.Register(schema_ae1c58020a8c26ad,
		0x95b1fe4851e3114e,
		0xc4abcc30dd3a9483,
		0xeab612050895537a,
		0xfde2c325eeeb9f0a)
}
