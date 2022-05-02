package logwriter

import (
	"fmt"
	"os"

	"zenhack.net/go/ocap-md/pkg/diskstore/types"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"

	"capnproto.org/go/capnp/v3"
)

type LogWriter struct {
	file   *os.File
	enc    *capnp.Encoder
	offset int64
	err    error
}

func MakePath(dir string, index uint64) string {
	return fmt.Sprintf("%s/log-%v", dir, index)
}

func Create(dir string, index uint64) (*os.File, *LogWriter, error) {
	return Init(dir, index, 0, true)
}

func Open(dir string, index uint64, size int64) (*os.File, *LogWriter, error) {
	return Init(dir, index, size, false)
}

func Init(dir string, index uint64, offset int64, create bool) (*os.File, *LogWriter, error) {
	path := MakePath(dir, index)
	oflags := os.O_RDWR
	if create {
		oflags |= os.O_CREATE
	}
	file, err := os.OpenFile(path, oflags, 0600)
	if err != nil {
		return nil, nil, err
	}
	l, err := New(file, offset)
	if err != nil {
		file.Close()
		return nil, nil, err
	}
	return file, l, nil
}
func New(file *os.File, offset int64) (*LogWriter, error) {
	fi, err := file.Stat()
	if err != nil {
		return nil, err
	}
	if fi.Size() < offset {
		err = fmt.Errorf("Log file shorter than expected: wanted %v but got %v",
			offset, fi.Size())
		return nil, err
	}
	err = file.Truncate(offset)
	if err != nil {
		return nil, err
	}
	_, err = file.Seek(offset, 0)
	if err != nil {
		return nil, err
	}
	return &LogWriter{
		file:   file,
		enc:    capnp.NewEncoder(file),
		offset: offset,
	}, nil
}

func (l *LogWriter) WriteEntry(ent diskstore.LogEntry) (types.LocalAddr, error) {
	if l.err != nil {
		return types.LocalAddr{}, l.err
	}

	msg := ent.Struct.Message()
	l.err = msg.SetRoot(ent.Struct.ToPtr())
	if l.err != nil {
		return types.LocalAddr{}, l.err
	}
	l.err = l.enc.Encode(msg)
	if l.err != nil {
		return types.LocalAddr{}, l.err
	}
	offset := l.offset
	size, err := msg.TotalSize()
	if err != nil {
		l.err = err
		return types.LocalAddr{}, err
	}
	l.offset += int64(size)
	return types.LocalAddr{
		Offset: offset,
		Length: uint32(size),
	}, nil
}

func (l *LogWriter) File() *os.File {
	return l.file
}

func (l *LogWriter) Offset() int64 {
	return l.offset
}
