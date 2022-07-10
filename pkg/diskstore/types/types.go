package types

import (
	"bytes"
	"errors"
	"io"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/packed"
	"github.com/ulikunitz/xz"

	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type Addr struct {
	LogNumber uint64
	LocalAddr
}

type LocalAddr struct {
	Offset int64
	Length uint32
}

func (a Addr) Encode() diskstore.Addr {
	_, seg := capnp.NewSingleSegmentMessage(nil)
	root, err := diskstore.NewRootAddr(seg)
	if err != nil {
		panic(err)
	}
	a.EncodeInto(root)
	return root
}

func (a Addr) EncodeInto(da diskstore.Addr) {
	da.SetLogNumber(a.LogNumber)
	da.SetOffset(uint64(a.Offset))
	da.SetLength(a.Length)
}

func DecodeAddr(addr diskstore.Addr) Addr {
	return Addr{
		LogNumber: addr.LogNumber(),
		LocalAddr: LocalAddr{
			Offset: int64(addr.Offset()),
			Length: addr.Length(),
		},
	}
}

// Abstract interface for the backing store.
type Storage interface {
	// Fetch the log entry at the given address.
	Fetch(Addr) (diskstore.LogEntry, error)

	// Store the data, returning its address.
	WriteEntry(ent diskstore.LogEntry) (Addr, error)

	// Marks the address range for deletion. Note: the current implementation
	// does not use this; proper cleanup is still TODO
	Clear(Addr) error
}

var (
	ErrWrongEntryType     = errors.New("Unexpected log entry type.")
	ErrUnknownCompression = errors.New("Unknown compression scheme.")
)

func FetchTrieMap(s Storage, addr Addr) (diskstore.TrieMap, error) {
	ent, err := s.Fetch(addr)
	if err != nil {
		return diskstore.TrieMap{}, err
	}
	if ent.Which() != diskstore.LogEntry_Which_indexNode {
		return diskstore.TrieMap{}, ErrWrongEntryType
	}
	return ent.IndexNode()
}

func FetchBlob(s Storage, addr Addr) ([]byte, error) {
	ent, err := s.Fetch(addr)
	if err != nil {
		return nil, err
	}
	if ent.Which() != diskstore.LogEntry_Which_blob {
		return nil, ErrWrongEntryType
	}
	blob := ent.Blob()
	data, err := blob.Segment()
	if err != nil {
		return nil, err
	}
	switch blob.Compression() {
	case diskstore.CompressionScheme_none:
	case diskstore.CompressionScheme_xz:
		r, err := xz.NewReader(bytes.NewBuffer(data))
		if err != nil {
			return nil, err
		}
		data, err = io.ReadAll(r)
		if err != nil {
			return nil, err
		}
	default:
		return nil, ErrUnknownCompression
	}
	if blob.Packed() {
		data, err = packed.Unpack(nil, data)
	}
	return data, err
}
