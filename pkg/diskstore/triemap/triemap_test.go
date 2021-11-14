package triemap

import (
	"errors"
	"fmt"
	"testing"

	"zenhack.net/go/ocap-md/pkg/diskstore/types"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type testStorage struct {
	blobs      map[types.Addr][]byte
	nextOffset int64
}

func makeTestStorage() *testStorage {
	return &testStorage{
		blobs:      make(map[types.Addr][]byte),
		nextOffset: 0,
	}
}

// Copy a byte slice.
func cloneBytes(data []byte) []byte {
	ret := make([]byte, len(data))
	copy(ret, data)
	return ret
}

func (s *testStorage) Fetch(addr types.Addr) (data []byte, err error) {
	data, ok := s.blobs[addr]
	if !ok {
		return nil, fmt.Errorf("No blob at address %v", addr)
	}
	return cloneBytes(data), nil
}

func (s *testStorage) Store(data []byte) (types.Addr, error) {
	// FIXME: validate that data is not to big for uint32 length.
	addr := types.Addr{
		Arena: 0,
		ArenaAddr: types.ArenaAddr{
			Offset: s.nextOffset,
			Size:   uint32(len(data)),
		},
	}
	s.nextOffset += int64(len(data))
	s.blobs[addr] = cloneBytes(data)
	return addr, nil
}

func (s *testStorage) Clear(addr types.Addr) error {
	delete(s.blobs, addr)
	return nil
}

func TestTrieMap(t *testing.T) {
	e := makeEnv(t)

	expectAbsent(e, "")
	expectAbsent(e, "abc")

	doInsert(e, "abc", 2)
	expectFound(e, "abc", 2)
	expectAbsent(e, "acc")

	doInsert(e, "abc", 3)
	expectFound(e, "abc", 3)

	doInsert(e, "acc", 4)
	expectFound(e, "abc", 3)
	expectFound(e, "acc", 4)

	doInsert(e, "bcd", 5)
	expectFound(e, "abc", 3)
	expectFound(e, "acc", 4)
	expectFound(e, "bcd", 5)

	doInsert(e, "abf", 6)
	expectFound(e, "abc", 3)
	expectFound(e, "acc", 4)
	expectFound(e, "bcd", 5)
	expectFound(e, "abf", 6)
}

func makeEnv(t *testing.T) *env {
	return &env{
		t: t,
		s: makeTestStorage(),
	}
}

type env struct {
	t *testing.T
	s Storage
	m diskstore.TrieMap
}

func doInsert(e *env, key string, value uint32) {
	res, err := Insert(e.s, []byte(key), types.Addr{Arena: value}.Encode(), e.m)
	chkfatal(e.t, "doInsert", err)
	e.m = res.ResNode
}

func expectAbsent(e *env, key string) {
	_, err := Lookup(e.s, []byte(key), e.m)
	assertErr(e.t, err, ErrNotFound)
}

func expectFound(e *env, key string, want uint32) {
	have, err := Lookup(e.s, []byte(key), e.m)
	chkfatal(e.t, "expectFound", err)
	assertEq(e.t, have, types.Addr{Arena: want})
}

func chkfatal(t *testing.T, prefix string, err error) {
	if err != nil {
		t.Errorf("%v: unexpected error: %v", prefix, err)
		panic(err)
	}
}

func assertErr(t *testing.T, errHave, errWant error) {
	if !errors.Is(errHave, errWant) {
		t.Fatalf("Expected error %v, but got %v", errWant, errHave)
	}
}

func assertEq(t *testing.T, have, want types.Addr) {
	if have != want {
		t.Fatalf("values not equal; expected %v but got %v", want, have)
	}
}
