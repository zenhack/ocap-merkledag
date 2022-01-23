package files

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"zenhack.net/go/ocap-md/pkg/schema/files"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

// os.PathSepartor, but as a string instead of a rune.
var pathSepString = fmt.Sprintf("%c", os.PathSeparator)

func isValidPathSegment(s string) bool {
	return s != "" &&
		s != "." &&
		s != ".." &&
		!strings.Contains(s, "/") &&
		!strings.Contains(s, pathSepString)
}

func saveFile(ctx context.Context, dir string, f files.File) error {
	name, err := f.Name()
	if err != nil {
		return err
	}
	if !isValidPathSegment(name) {
		return fmt.Errorf("Invalid path segment: %q", name)
	}
	path := filepath.Join(dir, name)

	// Check if the file already exists. This protects us against
	// downloading malformed directories which contain multiple
	// files by the same name; if one is a symlink it could be used
	// for path traversal attacks. This is racy, but works to avoid
	// interfering with ourselves.
	_, err = os.Lstat(path)
	if err == nil {
		return fmt.Errorf("lstat: %q already exists.", path)
	}

	switch f.Which() {
	case files.File_Which_file:
		localFile, err := os.Create(path)
		if err != nil {
			return err
		}
		err = writeBlobTree(ctx, localFile, f.File())
		localFile.Close()
		if err != nil {
			return err
		}
		return syncMetadata(path, f)
	case files.File_Which_dir:
		res, rel := f.Dir().Get(ctx, nil)
		ret, err := res.Struct()
		rel()
		if err != nil {
			return err
		}
		v, err := ret.Value()
		if err != nil {
			return err
		}
		err = saveDir(ctx, path, files.File_List{v.List()})
		if err != nil {
			return err
		}
		return syncMetadata(path, f)
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

func saveDir(ctx context.Context, path string, ents files.File_List) error {
	err := os.Mkdir(path, 0700)
	if err != nil {
		return err
	}
	for i := 0; i < ents.Len(); i++ {
		if err = ctx.Err(); err != nil {
			return err
		}
		if err = saveFile(ctx, path, ents.At(i)); err != nil {
			return err
		}
	}
	return nil
}

func writeBlobTree(ctx context.Context, sink io.Writer, ref protocol.Ref) error {
	panic("TODO")
}

func syncMetadata(path string, f files.File) error {
	panic("TODO")
}
