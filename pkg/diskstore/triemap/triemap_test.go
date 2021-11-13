package triemap

import (
	"errors"
	"testing"

	"zenhack.net/go/ocap-md/pkg/diskstore/filearena"
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

func (s *testStorage) Fetch(addr types.Addr) (data []byte, err error) {
	data, ok := s.blobs[addr]
	if !ok {
		return nil, ErrNotFound
	}
	return data, nil
}

func (s *testStorage) Store(data []byte) (types.Addr, error) {
	// FIXME: validate that data is not to big for uint32 length.
	addr := types.Addr{
		Arena: 0,
		ArenaAddr: filearena.Addr{
			Offset: s.nextOffset,
			Size:   uint32(len(data)),
		},
	}
	s.nextOffset += int64(len(data))
	s.blobs[addr] = data
	return addr, nil
}

func (s *testStorage) Clear(addr types.Addr) error {
	delete(s.blobs, addr)
	return nil
}

func TestTrieMap(t *testing.T) {
	s := makeTestStorage()
	m := diskstore.TrieMap{}
	_, err := Lookup(s, []byte{}, m)
	assertErr(t, err, ErrNotFound)
	_, err = Lookup(s, []byte("abc"), m)
	assertErr(t, err, ErrNotFound)

	v := types.Addr{Arena: 2}
	res, err := Insert(s, []byte("abc"), v.Encode(), m)
	chkfatal(err)
	m = res.ResNode

	vActual, err := Lookup(s, []byte("abc"), m)
	chkfatal(err)
	assertEq(t, vActual, v)

	v = types.Addr{Arena: 3}
	res, err = Insert(s, []byte("abc"), v.Encode(), m)
	chkfatal(err)
	m = res.ResNode

	vActual, err = Lookup(s, []byte("abc"), m)
	chkfatal(err)
	assertEq(t, vActual, v)
}

func chkfatal(err error) {
	if err != nil {
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
