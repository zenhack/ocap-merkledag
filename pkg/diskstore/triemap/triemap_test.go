package triemap

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"testing"
	"testing/quick"

	"zenhack.net/go/ocap-md/pkg/diskstore/filearena"
	"zenhack.net/go/ocap-md/pkg/diskstore/types"
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
		Offset: s.nextOffset,
		Length: uint32(len(data)),
	}
	s.nextOffset += int64(len(data))
	s.blobs[addr] = cloneBytes(data)
	return addr, nil
}

func (s *testStorage) Clear(addr types.Addr) error {
	delete(s.blobs, addr)
	return nil
}

func TestTrieMapInMemory(t *testing.T) {
	err := quick.Check(func(flushPoints []byte) bool {
		testTrieMap(t, makeTestStorage(), flushPoints)
		return true
	}, nil)
	chkfatal(t, "quick.Check", err)
}

func TestTrieMapFileArena(t *testing.T) {
	f, err := ioutil.TempFile("", "*.filearena")
	if err != nil {
		t.Fatalf("opening temporary file: %v", err)
	}
	defer os.Remove(f.Name())
	defer f.Close()
	fa, err := filearena.New(f, 0)
	if err != nil {
		t.Fatal("filearena.New: ", err)
	}
	testTrieMap(t, &FileArenaStorage{
		FileArena: fa,
	}, nil)
}

func testTrieMap(t *testing.T, s Storage, flushPoints []byte) {
	e := makeEnv(t, s, flushPoints)

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

	doDelete(e, "xyz", false)
	expectFound(e, "abc", 3)
	expectFound(e, "acc", 4)
	expectFound(e, "bcd", 5)
	expectFound(e, "abf", 6)

	doDelete(e, "abc", true)
	expectAbsent(e, "abc")
	expectFound(e, "acc", 4)
	expectFound(e, "bcd", 5)
	expectFound(e, "abf", 6)

	doDelete(e, "bcd", true)
	expectAbsent(e, "abc")
	expectFound(e, "acc", 4)
	expectAbsent(e, "bcd")
	expectFound(e, "abf", 6)

	doInsert(e, "bcd", 7)
	expectAbsent(e, "abc")
	expectFound(e, "acc", 4)
	expectFound(e, "bcd", 7)
	expectFound(e, "abf", 6)
}

func makeEnv(t *testing.T, s Storage, flushPoints []byte) *env {
	ret := &env{
		t: t,
		s: makeTestStorage(),
	}
	for _, p := range flushPoints {
		ret.flushPoints[p] = true
	}
	ret.m = New(ret.s, types.Addr{})
	return ret
}

type env struct {
	t           *testing.T
	s           Storage
	m           *TrieMap
	ctr         byte
	flushPoints [256]bool
}

func (e *env) tick() {
	if e.flushPoints[e.ctr] {
		fmt.Println("Flushing at point", e.ctr)
		chkfatal(e.t, "e.m.Flush()", e.m.Flush())
	} else {
		fmt.Println("Not flushing at point", e.ctr)
	}
	e.ctr++
}

func doDelete(e *env, key string, found bool) {
	e.tick()
	err := e.m.Remove([]byte(key))
	if !found {
		assertErr(e.t, err, ErrNotFound)
	} else {
		chkfatal(e.t, "doDelete", err)
	}
}

func doInsert(e *env, key string, value int64) {
	e.tick()
	err := e.m.Insert([]byte(key), types.Addr{Offset: value})
	chkfatal(e.t, "doInsert", err)
}

func expectAbsent(e *env, key string) {
	e.tick()
	_, err := e.m.Lookup([]byte(key))
	assertErr(e.t, err, ErrNotFound)
}

func expectFound(e *env, key string, want int64) {
	e.tick()
	have, err := e.m.Lookup([]byte(key))
	chkfatal(e.t, "expectFound", err)
	assertEq(e.t, have, types.Addr{Offset: want})
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
