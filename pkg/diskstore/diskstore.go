package diskstore

import (
	"io/ioutil"
	"os"
	"sync"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/diskstore/filearena"
	"zenhack.net/go/ocap-md/pkg/diskstore/triemap"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type DiskStore struct {
	path         string
	manifest     diskstore.Manifest
	index, blobs *filearena.FileArena
	indexStorage triemap.Storage

	// Lock which must be held in write mode to make a checkpoint.
	// goroutines making modifications to the state must hold this
	// in read mode whenever a checkpoint would be inconsistent.
	checkpointLock *sync.RWMutex
}

type syncArenaResult struct {
	off int64
	err error
}

// TODO: move this to some utility module:
func firstErr(errs ...error) error {
	for _, err := range errs {
		if err != nil {
			return err
		}
	}
	return nil
}

func (s *DiskStore) Checkpoint() error {
	tmpManifestPath := s.path + "/manifest.new"

	s.checkpointLock.Lock()
	defer s.checkpointLock.Unlock()

	syncArena := func(a *filearena.FileArena) chan syncArenaResult {
		ch := make(chan syncArenaResult, 1)
		go func() {
			off, err := a.Sync()
			ch <- syncArenaResult{
				off: off,
				err: err,
			}
		}()
		return ch
	}

	indexCh := syncArena(s.index)
	blobsCh := syncArena(s.blobs)

	indexRes := <-indexCh
	blobsRes := <-blobsCh

	err := firstErr(indexRes.err, blobsRes.err)
	if err != nil {
		return err
	}
	s.manifest.SetBlobArenaSize(uint64(blobsRes.off))

	//TODO/FIXME: set address of root?

	err = func() (err error) {
		f, err := os.Create(tmpManifestPath)
		if err != nil {
			return err
		}
		defer func() {
			f.Close()
			err = firstErr(err, f.Sync())
		}()
		err = capnp.NewEncoder(f).Encode(s.manifest.Struct.Message())
		return err
	}()

	// TODO: add some context.
	if err != nil {
		// Best effort cleanup:
		os.Remove(tmpManifestPath)

		return err
	}

	// TODO/FIXME: do we need to sync the parent dir?
	return os.Rename(tmpManifestPath, s.path+"/manifest")
}

func (s *DiskStore) Close() error {
	if s.index != nil {
		s.index.Close()
	}
	if s.blobs != nil {
		s.blobs.Close()
	}
	return nil
}

func openArena(path string, offset int64, create bool) (*filearena.FileArena, error) {
	f, err := openArenaFile(path, create)
	if err != nil {
		return nil, err
	}
	fa, err := filearena.New(f, offset)
	if err != nil {
		f.Close()
		return nil, err
	}
	return fa, nil
}

func openArenaFile(path string, create bool) (*os.File, error) {
	if create {
		return os.Create(path)
	} else {
		return os.OpenFile(path, os.O_RDWR, 0600)
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
	if err = initArenas(ret, false); err != nil {
		ret.Close()
		return nil, err
	}
	return ret, nil
}

func initArenas(s *DiskStore, create bool) error {
	blobMapAddr, err := s.manifest.BlobMap()
	if err != nil {
		return err
	}

	// Cut the index arena off right after the root node, as that will have been
	// the last allocation:
	indexBound := int64(blobMapAddr.Offset()) + int64(blobMapAddr.Length())
	if s.index, err = openArena(s.path+"/index", indexBound, create); err != nil {
		return err
	}

	if s.blobs, err = openArena(s.path+"/blobs", int64(s.manifest.BlobArenaSize()), create); err != nil {
		return err
	}
	s.indexStorage = &triemap.FileArenaStorage{
		FileArena: s.index,
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

	if err = initArenas(ret, true); err != nil {
		ret.Close()
		erase(ret)
		return nil, err
	}
	return ret, nil
}
