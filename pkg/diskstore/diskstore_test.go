package diskstore

import (
	"crypto/sha256"
	"testing"
	"testing/quick"

	"github.com/stretchr/testify/assert"
)

func makeTestStore(t *testing.T) *DiskStore {
	dir := t.TempDir()
	ret, err := Create(dir)
	assert.Nil(t, err, "Failed to create disk store.")
	return ret
}

func TestGetSetRoot(t *testing.T) {
	store := makeTestStore(t)
	defer store.Close()

	_, err := store.GetRoot()
	assert.Equal(t, ErrNoRoot, err,
		"GetRoot() should return ErrNoRoot before a root has been set.")

	assert.Nil(t, quick.Check(func(rootData []byte) bool {
		putRef, err := store.Put(rootData)
		assert.Nil(t, err)

		store.SetRoot(putRef)
		getRef, err := store.GetRoot()
		assert.Nil(t, err)
		assert.Equal(t, getRef.Hash(), putRef.Hash(), "Should return the hash we set.")
		return true
	}, nil), "quick.Check failed")
}

func TestGetPut(t *testing.T) {
	store := makeTestStore(t)
	defer store.Close()

	ents := make(map[Hash]getPutEntry)

	testPut := func(data []byte) {
		r, err := store.Put(data)
		assert.Nil(t, err, "Put() should succeed.")
		h := r.Hash()
		addr := r.addr
		assert.Equal(t, Hash(sha256.Sum256(data)), h,
			"Put() should return the sha256 hash of the data we gave it.")
		oldEnt, ok := ents[h]
		if ok {
			assert.Equal(t, oldEnt.ref.addr, addr, "Put()ing a blob twice gives the same address.")
		} else {
			ents[h] = getPutEntry{
				ref:  r,
				data: data,
			}
		}
	}

	assert.Nil(t, quick.Check(func(data []byte) bool {
		// Put the same data twice, to test dedup:
		testPut(data)
		testPut(data)
		return true
	}, nil), "quick.Check failed")

	for _, ent := range ents {
		data, err := ent.ref.Get()
		assert.Nil(t, err, "Should find all the blobs we Put() earlier.")
		assert.Equal(t, ent.data, data, "Should return the data we Put() earlier.")
	}
}

type getPutEntry struct {
	ref  Ref
	data []byte
}
