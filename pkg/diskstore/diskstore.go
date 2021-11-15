package diskstore

import (
	"io/ioutil"
	"os"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/diskstore/filearena"
	"zenhack.net/go/ocap-md/pkg/diskstore/triemap"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type DiskStore struct {
	path                   string
	manifest               diskstore.Manifest
	indexArena, blobsArena *filearena.FileArena
	indexFile, blobsFile   *os.File
	indexStorage           triemap.Storage
}

func (s *DiskStore) Close() error {
	maybeClose(s.indexFile)
	maybeClose(s.blobsFile)
	return nil
}

func maybeClose(f *os.File) {
	if f != nil {
		f.Close()
	}
}

func Open(path string) (*DiskStore, error) {
	manifestBytes, err := ioutil.ReadFile(path + "/manifest")
	if err != nil {
		return nil, err
	}
	msg := &capnp.Message{Arena: capnp.SingleSegment(manifestBytes)}
	root, err := diskstore.ReadRootManifest(msg)
	if err != nil {
		return nil, err
	}
	ret := &DiskStore{
		path:     path,
		manifest: root,
	}
	defer func() {
		if err != nil {
			ret.Close()
		}
	}()
	if ret.indexFile, err = os.OpenFile(path+"/index", os.O_RDWR, 0600); err != nil {
		return nil, err
	}
	if ret.blobsFile, err = os.OpenFile(path+"/blobs", os.O_RDWR, 0600); err != nil {
		return nil, err
	}
	if err = initArenas(ret); err != nil {
		return nil, err
	}
	return ret, nil
}

func initArenas(s *DiskStore) error {
	blobMapAddr, err := s.manifest.BlobMap()
	if err != nil {
		return err
	}

	// Cut the index arena off right after the root node, as that will have been
	// the last allocation:
	indexBound := int64(blobMapAddr.Offset()) + int64(blobMapAddr.Length())
	if s.indexArena, err = filearena.New(s.indexFile, indexBound); err != nil {
		return err
	}

	if s.blobsArena, err = filearena.New(s.blobsFile, int64(s.manifest.BlobArenaSize())); err != nil {
		return err
	}
	s.indexStorage = &triemap.FileArenaStorage{
		FileArena: s.indexArena,
		// TODO: Set ClearFn to something useful.
	}
	return nil
}

func erase(s *DiskStore) {
	os.Remove(s.path + "/manifest")
	os.Remove(s.path + "/index")
	os.Remove(s.path + "/blobs")
}

func Create(path string) (*DiskStore, error) {
	ret := &DiskStore{path: path}
	_, seg, _ := capnp.NewMessage(capnp.SingleSegment(nil))
	manifest, err := diskstore.NewRootManifest(seg)
	if err != nil {
		return nil, err
	}
	ret.manifest = manifest

	defer func() {
		if err != nil {
			ret.Close()
			erase(ret)
		}
	}()
	if ret.indexFile, err = os.Create(path + "/index"); err != nil {
		return nil, err
	}
	if ret.blobsFile, err = os.Create(path + "/blobs"); err != nil {
		return nil, err
	}
	if err = initArenas(ret); err != nil {
		return nil, err
	}
	return ret, nil
}
