package diskstore

import (
	"crypto/sha256"
	"io/ioutil"
	"os"
	"sync"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/diskstore/filearena"
	"zenhack.net/go/ocap-md/pkg/diskstore/triemap"
	"zenhack.net/go/ocap-md/pkg/diskstore/types"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

type DiskStore struct {
	path         string
	manifest     diskstore.Manifest
	index, blobs *filearena.FileArena
	indexStorage triemap.Storage

	indexRoot diskstore.TrieMap

	// Lock which must be held in write mode to make a checkpoint.
	// goroutines making modifications to the state must hold this
	// in read mode whenever a checkpoint would be inconsistent.
	checkpointLock *sync.RWMutex

	// TODO: the way much of the code is written tries to anticipate
	// fine-grained locking, but at a certain point I decided I wanted
	// to get something up and running faster, so this is currently
	// a big giant lock that everything must hold. Down the line,
	// remove this and do finer grained locking.
	mu *sync.Mutex
}

type Hash [sha256.Size]byte

func (h *Hash) ToContentId(cid protocol.ContentId) {
	cid.SetDigest(h[:])
	cid.SetFormat(protocol.ContentId_Format_segment)
	cid.SetAlgo(protocol.ContentId_Algo_sha256)
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
	s.mu.Lock()
	defer s.mu.Unlock()

	// TODO: this all needs to be carefully audited and vetted.
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

	// TODO: can we get away with finer-grained concurrency here? I think
	// we should be able to release the locks on the arenas, now that we know
	// their bounds; we just need to make sure nobody else tries to
	// Checkpoint().
	//
	// At the very least, we should be able to release locks before the
	// next fsync, which is the bigger deal.

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
	msg, err := capnp.Unmarshal(manifestBytes)
	if err != nil {
		return nil, err
	}
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
	err := os.MkdirAll(path, 0700)
	if err != nil {
		return nil, err
	}
	ret := &DiskStore{path: path}
	_, seg, _ := capnp.NewMessage(capnp.SingleSegment(nil))
	manifest, err := diskstore.NewRootManifest(seg)
	if err != nil {
		return nil, err
	}
	ret.manifest = manifest

	f, err := os.Create(path + "/manifest")
	if err != nil {
		return nil, err
	}
	defer f.Close()
	err = capnp.NewEncoder(f).Encode(manifest.Struct.Message())
	if err != nil {
		erase(ret)
		return nil, err
	}

	if err = initArenas(ret, true); err != nil {
		ret.Close()
		erase(ret)
		return nil, err
	}
	return ret, nil
}

func (s *DiskStore) lookup(hash *Hash) (types.Addr, error) {
	return triemap.Lookup(s.indexStorage, hash[:], s.indexRoot)
}

func (s *DiskStore) Get(hash *Hash) ([]byte, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	addr, err := s.lookup(hash)
	if err != nil {
		return nil, err
	}
	return s.blobs.Get(addr)
}

func (s *DiskStore) Put(data []byte) (Hash, types.Addr, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	hash := Hash(sha256.Sum256(data))
	addr, err := s.lookup(&hash)
	if err == nil {
		return hash, addr, nil
	} else if err == triemap.ErrNotFound {
		addr, err := s.blobs.Put(data)
		if err != nil {
			return hash, types.Addr{}, err
		}

		return hash, addr, s.insert(&hash, addr)
	}
	return hash, types.Addr{}, err
}

func (s *DiskStore) insert(hash *Hash, addr types.Addr) error {
	res, err := triemap.Insert(s.indexStorage, hash[:], addr.Encode(), s.indexRoot)
	if err != nil {
		return err
	}
	bm, err := s.manifest.BlobMap()
	if err != nil {
		return err
	}
	res.ResAddr.EncodeInto(bm)
	s.indexRoot = res.ResNode
	return nil
}
