package filearena

import (
	"fmt"
	"os"
	"sync"
)

type FileArena struct {
	syncMu    *sync.RWMutex
	allocMu   *sync.Mutex
	file      *os.File
	nextAlloc int64
}

type Addr struct {
	Offset int64
	Size   uint32
}

func New(file *os.File, nextAlloc int64) *FileArena {
	return &FileArena{
		syncMu:    &sync.RWMutex{},
		allocMu:   &sync.Mutex{},
		file:      file,
		nextAlloc: nextAlloc,
	}
}

func (fa *FileArena) Sync() (off int64, err error) {
	func() {
		fa.syncMu.Lock()
		defer fa.syncMu.Unlock()
		off = fa.nextAlloc
	}()
	err = fa.file.Sync()
	return
}

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

func (fa *FileArena) Get(addr Addr) (data []byte, err error) {
	// TODO(perf): Maybe mmap if the blob is large enough.
	data = make([]byte, addr.Size)
	_, err = fa.ReadAt(data, addr.Offset)
	return
}

func (fa *FileArena) Clear(addr Addr) error {
	return fmt.Errorf("TODO")
}

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
