package wal

import (
	"capnproto.org/go/capnp/v3"
	"os"

	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type Wal struct {
	offset int64
	file   *os.File
	enc    *capnp.Encoder
}

func New(file *os.File) (*Wal, error) {
	// Discover the current file offset, by seeking zero bytes from
	// the current spot:
	offset, err := file.Seek(0, 1)
	if err != nil {
		return nil, err
	}
	ret := &Wal{
		offset: offset,
		file:   file,
	}
	ret.enc = capnp.NewEncoder(ret)
	return ret, nil
}

func (w *Wal) Write(p []byte) (n int, err error) {
	n, err = w.file.Write(p)
	w.offset += int64(n)
	return
}

func (w *Wal) StartEntry() (entry diskstore.WalEntry, write func() error) {
	m, seg, err := capnp.NewMessage(capnp.SingleSegment(nil))
	if err != nil {
		// Should be impossible since we're supplying an allocator
		// that doesn't fail.
		panic(err)
	}
	entry, err = diskstore.NewRootWalEntry(seg)
	if err != nil {
		// Also impossible.
		panic(err)
	}
	return entry, func() error {
		return w.enc.Encode(m)
	}
}
