package diskstore

import (
	"crypto/sha256"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"os"
	"path/filepath"
	"sync"
	"time"

	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/diskstore/logwriter"
	"zenhack.net/go/ocap-md/pkg/diskstore/triemap"
	"zenhack.net/go/ocap-md/pkg/diskstore/types"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

var (
	ErrUnknownAlgo     = errors.New("Unknown hash algorithm")
	ErrBadDigestLength = errors.New("Incorrect digest length")
	ErrCorruptedBlob   = errors.New("Loaded blob did not match expected digest")
	ErrNoRoot          = errors.New("No root object")
)

type DiskStore struct {
	// Protects mutable fields
	mu sync.Mutex

	path           string
	manifest       diskstore.Manifest
	currentLog     *logwriter.LogWriter
	currentLogFile *os.File
	storage        types.Storage

	indexRoot       *triemap.TrieMap
	checkpointTimer *time.Timer
	closing         chan struct{}
}

type Hash [sha256.Size]byte

func (h *Hash) ToContentId(cid protocol.ContentId) {
	cid.SetAlgo(protocol.ContentId_Algo_sha256)
	// FIXME: re-use existing buffer if it exists and is large enough.
	// otherwise this will leak space.
	// FIXME: check & return errors.
	cid.SetDigest(h[:])
}

func (s *DiskStore) GetRoot() (Hash, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	cid, err := s.manifest.Root()
	if err != nil {
		return Hash{}, err
	}
	if !cid.IsValid() {
		return Hash{}, ErrNoRoot
	}
	return ContentIdHash(cid)
}

func ContentIdHash(cid protocol.ContentId) (hash Hash, err error) {
	if cid.Algo() != protocol.ContentId_Algo_sha256 {
		return hash, ErrUnknownAlgo
	}
	digest, err := cid.Digest()
	if err != nil {
		return hash, err
	}
	if len(digest) != len(hash) {
		return hash, ErrBadDigestLength
	}
	copy(hash[:], digest)
	return hash, nil
}

func (s *DiskStore) SetRoot(h *Hash) error {
	defer s.dirty()
	s.mu.Lock()
	defer s.mu.Unlock()
	root, err := s.manifest.Root()
	if err != nil {
		return err
	}
	if !root.IsValid() {
		root, err = s.manifest.NewRoot()
	}
	if err != nil {
		return err
	}
	h.ToContentId(root)
	return nil
}

func (s *DiskStore) writeManifest() error {
	s.manifest.SetFormatVersion(diskstore.StorageFormatVersion)
	tmpManifestPath := filepath.Join(s.path, "manifest.tmp")
	err := func() error {
		f, err := os.Create(tmpManifestPath)
		if err != nil {
			return err
		}
		defer f.Close()
		err = capnp.NewEncoder(f).Encode(s.manifest.Struct.Message())
		if err != nil {
			return err
		}
		err = f.Sync()
		if err != nil {
			return err
		}
		return os.Rename(tmpManifestPath, filepath.Join(s.path, "manifest"))
		// TODO/FIXME: do we need to sync the parent dir?
	}()
	if err != nil {
		// Best effort cleanup:
		os.Remove(tmpManifestPath)
	}
	return err
}

func (s *DiskStore) Checkpoint() error {
	s.mu.Lock()
	defer s.mu.Unlock()

	// TODO: this all needs to be carefully audited and vetted.

	err := s.indexRoot.Flush()
	if err != nil {
		return err
	}

	bm, err := s.manifest.BlobMap()
	if err != nil {
		return err
	}
	s.indexRoot.RootAddr().EncodeInto(bm)

	lastLog := s.currentLog
	lastLogFile := s.currentLogFile
	s.manifest.LastLog().SetSize(uint64(lastLog.Offset()))

	// TODO: set lastLog.logNumber. Ok for now since we don't
	// yet break stuff up into multiple logs, so it's always zero.

	// TODO: we should un-block other activity at this point; we have
	// the data we need and in principle don't need to make the rest of the
	// system wait for the .Sync() calls.

	err = lastLogFile.Sync()
	if err != nil {
		return err
	}
	return s.writeManifest()
}

func (s *DiskStore) Close() error {
	if s.closing != nil {
		close(s.closing)
	}
	if s.currentLogFile != nil {
		s.currentLogFile.Close()
	}
	return nil
}

func (s *DiskStore) dirty() {
	s.checkpointTimer.Reset(5 * time.Second)
}

func (s *DiskStore) startCheckpointLoop() {
	s.closing = make(chan struct{})
	s.checkpointTimer = time.NewTimer(5 * time.Second)
	ticker := time.NewTicker(30 * time.Second)
	go func() {
		done := false
		for !done {
			select {
			case <-s.closing:
				done = true
			case <-ticker.C:
			case <-s.checkpointTimer.C:
			}
			err := s.Checkpoint()
			if err != nil {
				// TODO: find a better way to manage this.
				log.Printf("Error taking checkpoint: %v", err)
			}
			ticker.Reset(30 * time.Second)
		}
		ticker.Stop()
	}()
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
	msg.ResetReadLimit(math.MaxUint64)
	root, err := diskstore.ReadRootManifest(msg)
	if err != nil {
		return nil, err
	}
	ret := &DiskStore{
		path:     path,
		manifest: root,
	}
	if ret.manifest.FormatVersion() > diskstore.StorageFormatVersion {
		return nil, fmt.Errorf(
			"Disk storage format version (%v) is too new; supported version is %v.",
			ret.manifest.FormatVersion(), diskstore.StorageFormatVersion,
		)
	}
	return ret, ret.init(false)
}

func (s *DiskStore) init(create bool) error {
	blobMapAddr, err := s.manifest.BlobMap()
	if err != nil {
		return err
	}
	rootAddr := types.DecodeAddr(blobMapAddr)
	mll := s.manifest.LastLog()
	s.currentLogFile, s.currentLog, err = logwriter.Init(
		s.path,
		mll.Number(),
		int64(mll.Size()),
		create,
	)
	if err != nil {
		return err
	}
	if rootAddr.LogNumber != mll.Number() {
		panic("Blob map is not in the LastLog.")
	}
	s.storage = logwriter.NewStorage(s.currentLog)
	s.indexRoot = triemap.New(s.storage, rootAddr)
	s.startCheckpointLoop()
	return nil
}

func erase(s *DiskStore) {
	os.Remove(filepath.Join(s.path, "manifest"))
	os.Remove(filepath.Join(logwriter.MakePath(s.path, 0)))
}

func Create(path string) (*DiskStore, error) {
	err := os.MkdirAll(path, 0700)
	if err != nil {
		return nil, err
	}
	ret := &DiskStore{path: path}
	msg, seg := capnp.NewSingleSegmentMessage(nil)
	msg.ResetReadLimit(math.MaxUint64)
	manifest, err := diskstore.NewRootManifest(seg)
	if err != nil {
		return nil, err
	}
	_, err = manifest.NewBlobMap()
	if err != nil {
		return nil, err
	}
	ret.manifest = manifest
	ret.init(true)
	err = ret.writeManifest()

	if err != nil {
		erase(ret)
		return nil, err
	}
	ret.startCheckpointLoop()
	return ret, nil
}

func (s *DiskStore) lookup(hash *Hash) (types.Addr, error) {
	return s.indexRoot.Lookup(hash[:])
}

func (s *DiskStore) Get(hash *Hash) ([]byte, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	addr, err := s.lookup(hash)
	if err != nil {
		return nil, err
	}
	return types.FetchBlob(s.storage, addr)
}

func (s *DiskStore) Put(data []byte) (Hash, types.Addr, error) {
	s.dirty()
	defer s.dirty()
	s.mu.Lock()
	defer s.mu.Unlock()
	hash := Hash(sha256.Sum256(data))
	addr, err := s.lookup(&hash)
	if err == nil {
		return hash, addr, nil
	} else if err == triemap.ErrNotFound {
		_, seg, err := capnp.NewMessage(capnp.SingleSegment(nil))
		if err != nil {
			return hash, types.Addr{}, err
		}
		ent, err := diskstore.NewRootLogEntry(seg)
		if err != nil {
			return hash, types.Addr{}, err
		}
		err = ent.SetBlob(data)
		if err != nil {
			return hash, types.Addr{}, err
		}
		addr, err := s.storage.WriteEntry(ent)
		if err != nil {
			return hash, types.Addr{}, err
		}

		return hash, addr, s.insert(&hash, addr)
	}
	return hash, types.Addr{}, err
}

func (s *DiskStore) insert(hash *Hash, addr types.Addr) error {
	return s.indexRoot.Insert(hash[:], addr)
}
