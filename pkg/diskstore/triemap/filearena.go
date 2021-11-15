package triemap

import (
	"errors"

	"zenhack.net/go/ocap-md/pkg/diskstore/filearena"
	"zenhack.net/go/ocap-md/pkg/diskstore/types"
)

var (
	ErrWrongArena = errors.New("Wrong arena index")
)

// TrieMap storage backend built on top of FileArena.
type FileArenaStorage struct {
	*filearena.FileArena
	ClearFn func(types.Addr) error
}

// Fetch the block of data at the given address.
func (s *FileArenaStorage) Fetch(addr types.Addr) (data []byte, err error) {
	return s.FileArena.Get(addr)
}

// Store the data, returning its address.
func (s *FileArenaStorage) Store(data []byte) (types.Addr, error) {
	return s.Put(data)
}

// Marks the address range for deletion.  This delegates to the ClearFn field.
// If that field is nil, this is a no-op.
func (s *FileArenaStorage) Clear(addr types.Addr) error {
	if s.ClearFn == nil {
		return nil
	}
	return s.ClearFn(addr)
}
