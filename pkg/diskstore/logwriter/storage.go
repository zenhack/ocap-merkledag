package logwriter

import (
	"fmt"

	"zenhack.net/go/ocap-md/pkg/diskstore/types"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"

	"capnproto.org/go/capnp/v3"
)

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
