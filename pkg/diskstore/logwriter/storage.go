package logwriter

import (
	"fmt"

	"zenhack.net/go/ocap-md/pkg/diskstore/types"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"

	"capnproto.org/go/capnp/v3"
)

/*
// Abstract interface for the backing store.
type Storage interface {
	// Fetch the log entry at the given address.
	Fetch(types.Addr) (diskstore.LogEntry, error)

	// Store the data, returning its address.
	WriteEntry(ent diskstore.LogEntry) (types.Addr, error)

	// Marks the address range for deletion. Note: the current implementation
	// does not use this; proper cleanup is still TODO
	Clear(types.Addr) error
}
*/

type logWriterStorage struct {
	l *LogWriter
}

func NewStorage(l *LogWriter) types.Storage {
	return logWriterStorage{l}
}

func (s logWriterStorage) Fetch(addr types.Addr) (diskstore.LogEntry, error) {
	if addr.LogNumber != 0 {
		return diskstore.LogEntry{}, fmt.Errorf("Bad log number: %v", addr.LogNumber)
	}
	buf := make([]byte, addr.Length)
	_, err := s.l.file.ReadAt(buf, addr.Offset)
	if err != nil {
		return diskstore.LogEntry{}, err
	}
	msg, err := capnp.Unmarshal(buf)
	if err != nil {
		return diskstore.LogEntry{}, err
	}
	return diskstore.ReadRootLogEntry(msg)
}

func (s logWriterStorage) WriteEntry(ent diskstore.LogEntry) (types.Addr, error) {
	laddr, err := s.l.WriteEntry(ent)
	return types.Addr{LocalAddr: laddr}, err
}

func (s logWriterStorage) Clear(types.Addr) error {
	// TODO: actually do something here.
	return nil
}
