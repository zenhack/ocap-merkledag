package filearena

import (
	"fmt"
	"os"
	"sync"
)

// A FileArena represents an on-disk allocation arena.
//
// This takes the form of a flat file, where allocations
// happen at the end of the file.
type FileArena struct {
	file      *os.File // The underlying file
	nextAlloc int64    // The next free offset into the file

	// syncMu is a readers/writer lock, used as follows:
	//
	// - Sync() must hold the lock in write mode, to ensure that when
	//   it measures the current offset, all space that has been allocated
	//   has also been written to. It needn't hold the lock during the call
	//   to the underlying file.Sync; that will just result in additional
	//   data hitting the disk, which is fine.
	// - Writers must hold the lock in _read_ mode when writing, somewhat
	//   counter-intuitively.
	//
	// This has the effect of excluding calls to sync whenever the offset
	// includes uninitialized storage, while allowing writes after the
	// synchronization point to proceed without waiting on the fsync call.
	syncMu *sync.RWMutex

	// allocMu must be held when accessing nextAlloc, unless syncMu is
	// held in write mod. syncMu must be acquired in read mode before.
	// taking this lock.
	allocMu *sync.Mutex
}

type Addr struct {
	Offset int64
	Size   uint32
}

// Construct a FileArnea from an open file and the next allocation offset into
// that file.
func New(file *os.File, nextAlloc int64) *FileArena {
	return &FileArena{
		syncMu:    &sync.RWMutex{},
		allocMu:   &sync.Mutex{},
		file:      file,
		nextAlloc: nextAlloc,
	}
}

// Make changes to the arena durable. Returns the size of the arena at the time
// when the snapshot was taken, and an error, if any.
func (fa *FileArena) Sync() (off int64, err error) {
	func() {
		fa.syncMu.Lock()
		defer fa.syncMu.Unlock()
		off = fa.nextAlloc
	}()
	err = fa.file.Sync()
	return
}

// Write the bytes to the arena, returning its address.
func (fa *FileArena) Put(data []byte) (addr Addr, err error) {
	size := len(data)
	fa.withAlloc(int64(size), func(off int64) {
		_, err = fa.file.WriteAt(data, off)
		addr = Addr{
			Offset: off,
			Size:   uint32(size),
		}
	})
	return
}

// Return the bytes at the specified address.
func (fa *FileArena) Get(addr Addr) (data []byte, err error) {
	// TODO(perf): Maybe mmap if the blob is large enough.
	data = make([]byte, addr.Size)
	_, err = fa.ReadAt(data, addr.Offset)
	return
}

// Delete the data at the given address.
//
// We use spares files for this under the hood, so there appear to be "gaps"
// in the file that are logically all zeros, but for which no storage is
// allocated.
func (fa *FileArena) Clear(addr Addr) error {
	return fmt.Errorf("TODO")
}

// Like `Get`, but takes a user supplied buffer.
func (fa *FileArena) ReadAt(b []byte, off int64) (n int, err error) {
	return fa.file.ReadAt(b, off)
}

func (fa *FileArena) withAlloc(size int64, f func(off int64)) {
	fa.syncMu.RLock()
	defer fa.syncMu.RUnlock()
	f(fa.alloc(size))
}

func (fa *FileArena) alloc(size int64) int64 {
	fa.allocMu.Lock()
	defer fa.allocMu.Unlock()
	off := fa.nextAlloc
	fa.nextAlloc += size
	return off
}