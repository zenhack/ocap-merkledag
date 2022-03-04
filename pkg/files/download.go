package files

import (
	"context"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"

	"zenhack.net/go/ocap-md/pkg/schema/containers"
	"zenhack.net/go/ocap-md/pkg/schema/files"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

func Download(ctx context.Context, path string, ref protocol.Ref) error {
	res, rel := ref.Get(ctx, nil)
	defer rel()
	s, err := res.Struct()
	if err != nil {
		return err
	}
	v, err := s.Value()
	if err != nil {
		return err
	}
	f := files.File{v.Struct()}
	return saveFile(ctx, path, f)
}

// os.PathSepartor, but as a string instead of a rune.
var pathSepString = fmt.Sprintf("%c", os.PathSeparator)

func isValidPathSegment(s string) bool {
	return s != "" &&
		s != "." &&
		s != ".." &&
		!strings.Contains(s, "/") &&
		!strings.Contains(s, pathSepString)
}

func saveFile(ctx context.Context, path string, f files.File) error {
	// Check if the file already exists. This protects us against
	// downloading malformed directories which contain multiple
	// files by the same name; if one is a symlink it could be used
	// for path traversal attacks. This is racy, but works to avoid
	// interfering with ourselves.
	//
	// TODO: we could avoid this by instead incrementally checking
	// when walking the directory entries that they are sorted,
	// and therefore catch duplicates.
	_, err := os.Lstat(path)
	if err == nil {
		return fmt.Errorf("lstat: %q already exists.", path)
	}

	switch f.Which() {
	case files.File_Which_file:
		localFile, err := os.Create(path)
		if err != nil {
			return err
		}
		defer localFile.Close()
		bt, err := f.File()
		if err != nil {
			return err
		}
		err = writeBlobTree(ctx, localFile, bt)
		if err != nil {
			return err
		}
		return syncMetadata(path, f.Metadata())
	case files.File_Which_dir:
		err = saveDir(ctx, path, f)
		if err != nil {
			return err
		}
		return syncMetadata(path, f.Metadata())
	case files.File_Which_symlink:
		target, err := f.Symlink()
		if err != nil {
			return err
		}
		return os.Symlink(path, target)
	default:
		return fmt.Errorf("%q: Unknown file type: %v", path, f.Which())
	}
}

func saveDir(ctx context.Context, path string, f files.File) error {
	err := os.Mkdir(path, 0700)
	if err != nil {
		return err
	}
	res, rel := f.Dir().Get(ctx, nil)
	defer rel()
	s, err := res.Value().Struct()
	if err != nil {
		return err
	}
	root, err := containers.BPlusTree{s}.Root()
	if err != nil {
		return err
	}
	return saveDirEnts(ctx, path, root)
}

func saveDirEnts(ctx context.Context, path string, node containers.BPlusTree_Node) error {
	ents, err := node.Branches()
	if err != nil {
		return err
	}

	for i := 0; i < ents.Len(); i++ {
		if err = ctx.Err(); err != nil {
			return err
		}
		ent := ents.At(i)
		switch ent.Which() {
		case containers.BPlusTree_Branch_Which_leaf:
			k, err := ent.Key()
			if err != nil {
				return err
			}
			name := k.Text()
			if !isValidPathSegment(name) {
				return fmt.Errorf("Invalid path segment: %q", name)
			}
			leaf, err := ent.Leaf()
			if err != nil {
				return err
			}
			f := files.File{leaf.Struct()}
			if err = saveFile(ctx, filepath.Join(path, name), f); err != nil {
				return err
			}
		case containers.BPlusTree_Branch_Which_node:
			// N.B. this is curerntly untested. The upload logic doesn't
			// actually construct nested trees (yet).
			res, rel := ent.Node().Get(ctx, nil)
			s, err := res.Value().Struct()
			if err != nil {
				rel()
				return err
			}
			err = saveDirEnts(ctx, path, containers.BPlusTree_Node{s})
			rel()
			if err != nil {
				return err
			}
		default:
			return fmt.Errorf("Uknown Branch tag: %d", ent.Which())
		}

	}
	return nil
}

func writeBlobTree(ctx context.Context, w io.Writer, bt files.BlobTree) error {
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
		_, err = w.Write(v.Data())
		return err
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
		kids := files.BlobTree_List{v.List()}
		for i := 0; i < kids.Len(); i++ {
			kid := kids.At(i)
			if err = writeBlobTree(ctx, w, kid); err != nil {
				return err
			}
		}
		return nil
	default:
		return fmt.Errorf("Unknown BlobTree variant: %v", bt.Which())
	}
}

func syncMetadata(path string, m files.File_metadata) error {
	if m.Which() != files.File_metadata_Which_unixMetadata {
		return nil
	}

	um, err := m.UnixMetadata()
	if err != nil {
		return err
	}

	perm := fs.FileMode(um.Permissions() & 0777)
	modTime := time.Unix(um.ModTime(), 0)

	if err := os.Chmod(path, perm); err != nil {
		return err
	}
	return os.Chtimes(path, modTime, modTime)
}
