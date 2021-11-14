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
	ArenaIndex uint32
	ClearFn    func(types.Addr) error
}

// Fetch the block of data at the given address.
func (s *FileArenaStorage) Fetch(addr types.Addr) (data []byte, err error) {
	if addr.Arena != s.ArenaIndex {
		return nil, ErrWrongArena
	}
	return s.FileArena.Get(addr.ArenaAddr)
}

// Store the data, returning its address.
func (s *FileArenaStorage) Store(data []byte) (types.Addr, error) {
	addr, err := s.Put(data)
	return types.Addr{
		ArenaAddr: addr,
		Arena:     s.ArenaIndex,
	}, err
}

// Marks the address range for deletion. After checking that the arena index
// is correct, this delegates to the ClearFn field. If that field is nil, this
// is a no-op.
func (s *FileArenaStorage) Clear(addr types.Addr) error {
	if addr.Arena != s.ArenaIndex {
		return ErrWrongArena
	}
	if s.ClearFn == nil {
		return nil
	}
	return s.ClearFn(addr)
}
