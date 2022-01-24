package files

import (
	"context"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"zenhack.net/go/ocap-md/pkg/blobtree"
	"zenhack.net/go/ocap-md/pkg/schema/files"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

type ErrUnsupportedFileType fs.FileMode

func (e ErrUnsupportedFileType) Error() string {
	return fmt.Sprintf("Unsupported file type: %v", fs.FileMode(e).Type())
}

func PutFile(ctx context.Context, s protocol.Storage, path string) (protocol.Ref, error) {
	fi, err := os.Lstat(path)
	if err != nil {
		return protocol.Ref{}, err
	}
	res, _ := s.Put(ctx, func(p protocol.Storage_put_Params) error {
		f, err := files.NewFile(p.Segment())
		if err != nil {
			return err
		}
		storeFile(ctx, s, fi, path, f)
		p.SetValue(f.ToPtr())
		return nil
	})
	return res.Ref().AddRef(), nil
}

func storeFile(ctx context.Context, s protocol.Storage, fi os.FileInfo, path string, f files.File) error {
	err := storeFileInfo(fi, f)
	if err != nil {
		return err
	}

	mode := fi.Mode()
	if mode.IsDir() {
		return storeDirectory(ctx, s, fi, path, f)
	}
	if mode.IsRegular() {
		return storeRegularFile(ctx, s, path, f)
	}
	if mode.Type() == fs.ModeSymlink {
		return storeSymlink(path, f)
	}
	return ErrUnsupportedFileType(mode)
}

func storeFileInfo(fi fs.FileInfo, f files.File) error {
	f.SetName(fi.Name())
	meta, err := files.NewUnixMetadata(f.Segment())
	if err != nil {
		return err
	}
	meta.SetPermissions(uint32(fi.Mode() & fs.ModePerm))
	meta.SetModTime(fi.ModTime().Unix())
	f.Metadata().SetUnixMetadata(meta)
	return nil
}

func storeRegularFile(ctx context.Context, s protocol.Storage, path string, f files.File) error {
	input, err := os.Open(path)
	if err != nil {
		return fmt.Errorf("os.Open(): %w", err)
	}
	defer input.Close()

	bt, err := f.NewFile()
	if err != nil {
		return err
	}
	err = blobtree.WriteStream(ctx, s, input, bt)
	if err != nil {
		return fmt.Errorf("blobtree.WriteStream(): %w", err)
	}
	return nil
}

func storeDirectory(ctx context.Context, s protocol.Storage, fi fs.FileInfo, path string, f files.File) error {
	// TODO: handle very large directories.
	ents, err := os.ReadDir(path)
	if err != nil {
		return fmt.Errorf("readdir(): %w", err)
	}

	// TODO: release?
	res, _ := s.Put(ctx, func(p protocol.Storage_put_Params) error {
		var contents files.File_List
		contents, err = files.NewFile_List(p.Segment(), int32(len(ents)))
		if err != nil {
			return err
		}
		for i, ent := range ents {
			fi, err := ent.Info()
			if err != nil {
				return err
			}
			err = storeFile(ctx, s, fi, filepath.Join(path, fi.Name()), contents.At(i))
			if err != nil {
				fmt.Errorf("storeFile at index %d: %w", i, err)
			}
		}
		p.SetValue(contents.ToPtr())
		return nil
	})
	if err != nil {
		return err
	}
	f.SetDir(res.Ref().AddRef())
	return nil
}

func storeSymlink(path string, f files.File) error {
	target, err := os.Readlink(path)
	if err != nil {
		return fmt.Errorf("readlink(): %w", err)
	}
	f.SetSymlink(target)
	return nil
}
