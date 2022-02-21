package fuse

import (
	"context"
	"io/fs"
	"math"
	"time"

	"bazil.org/fuse"
	//"bazil.org/fuse/fs"

	"zenhack.net/go/ocap-md/pkg/schema/files"
	//"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

type Node struct {
	f files.File
}

func (n *Node) Attr(ctx context.Context, attr *fuse.Attr) error {
	// Imutable; cache as long as you want:
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
