package fuse

import (
	"context"
	"io"
	"io/fs"
	"math"
	"syscall"
	"time"

	"bazil.org/fuse"
	//"bazil.org/fuse/fs"

	"zenhack.net/go/ocap-md/pkg/containers"
	containerscp "zenhack.net/go/ocap-md/pkg/schema/containers"
	"zenhack.net/go/ocap-md/pkg/schema/files"
	//"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

type sysErr syscall.Errno

func (e sysErr) Error() string {
	return syscall.Errno(e).Error()
}

func (e sysErr) Errno() fuse.Errno {
	return fuse.Errno(e)
}

type Node struct {
	f files.File
}

func (n *Node) Attr(ctx context.Context, attr *fuse.Attr) error {
	// Immutable; cache as long as you want:
	attr.Valid = time.Duration(math.MaxInt64)

	m := n.f.Metadata()
	switch m.Which() {
	case files.File_metadata_Which_unixMetadata:
		um, err := m.UnixMetadata()
		if err != nil {
			return err
		}
		attr.Mode = fs.FileMode(um.Permissions() & 0777)
		attr.Mtime = time.Unix(um.ModTime(), 0)
	default:
		if n.f.Which() == files.File_Which_dir {
			attr.Mode = 0755
		} else {
			attr.Mode = 0644
		}
	}

	switch n.f.Which() {
	case files.File_Which_file:
		blobTree, err := n.f.File()
		if err != nil {
			return err
		}
		attr.Size = blobTree.Size()
	case files.File_Which_dir:
		attr.Mode |= fs.ModeDir
	case files.File_Which_symlink:
		attr.Mode |= fs.ModeSymlink
	}

	attr.Ctime = attr.Mtime
	attr.Blocks = attr.Size / 512
	return nil
}

func (n *Node) Read(ctx context.Context, req *fuse.ReadRequest, resp *fuse.ReadResponse) error {
	switch n.f.Which() {
	case files.File_Which_file:
		if req.Dir {
			return sysErr(syscall.ENOTDIR)
		}
		bt, err := n.f.File()
		if err != nil {
			return err
		}
		return readBlobTree(ctx, req, resp, bt)
	case files.File_Which_dir:
		if !req.Dir {
			return sysErr(syscall.EISDIR)
		}
	}
	return sysErr(syscall.EIO)
}

func (n *Node) ReadDirAll(ctx context.Context) ([]fuse.Dirent, error) {
	if n.f.Which() != files.File_Which_dir {
		return nil, sysErr(syscall.ENOTDIR)
	}
	res, rel := n.f.Dir().Get(ctx, nil)
	defer rel()
	s, err := res.Struct()
	if err != nil {
		return nil, err
	}
	v, err := s.Value()
	if err != nil {
		return nil, err
	}
	it, err := containers.IterBPlusTree(containerscp.BPlusTree{v.Struct()})
	ents := []fuse.Dirent{}
	k, v, err := it.Next(ctx)
	for err == nil {
		f := files.File{v.Struct()}
		var typ fuse.DirentType
		switch f.Which() {
		case files.File_Which_file:
			typ = fuse.DT_File
		case files.File_Which_dir:
			typ = fuse.DT_Dir
		case files.File_Which_symlink:
			typ = fuse.DT_Link
		default:
			typ = fuse.DT_Unknown
		}
		ents = append(ents, fuse.Dirent{
			Name:  k.Text(),
			Type:  typ,
			Inode: 0, // TODO: make these unique.
		})
		k, v, err = it.Next(ctx)
	}
	if err == io.EOF {
		return ents, nil
	}
	return nil, err
}

func readBlobTree(ctx context.Context, req *fuse.ReadRequest, resp *fuse.ReadResponse, bt files.BlobTree) error {
	switch bt.Which() {
	case files.BlobTree_Which_leaf:
		res, rel := bt.Leaf().Get(ctx, nil)
		defer rel()
		s, err := res.Struct()
		if err != nil {
			return err
		}
		v, err := s.Value()
		if err != nil {
			return err
		}
		data := v.Data()
		start := int(req.Offset)
		if start >= len(data) {
			// Read zero bytes
			return nil
		}
		end := start + req.Size
		if end >= len(data) {
			end = len(data) - 1
		}
		resp.Data = data[start:end]
		return nil
	case files.BlobTree_Which_branch:
		res, rel := bt.Branch().Get(ctx, nil)
		defer rel()
		s, err := res.Struct()
		if err != nil {
			return err
		}
		v, err := s.Value()
		if err != nil {
			return err
		}
		branches := files.BlobTree_List{v.List()}
		for i := 0; i < branches.Len(); i++ {
			branch := branches.At(i)
			size := int64(branch.Size())
			if size < req.Offset {
				// If the read spans a chunk boundary, we'll just do
				// a short read; applications should be able to handle
				// this.
				return readBlobTree(ctx, req, resp, branch)
			}
			req.Offset -= size
		}
	}
	return sysErr(syscall.EIO)
}
