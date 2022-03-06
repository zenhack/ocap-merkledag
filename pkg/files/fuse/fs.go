package fuse

import (
	"zenhack.net/go/ocap-md/pkg/schema/files"

	"bazil.org/fuse/fs"
)

type FS func() (files.File, error)

func (fs FS) Root() (fs.Node, error) {
	f, err := fs()
	if err != nil {
		return nil, err
	}
	return &Node{f: f}, nil
}
