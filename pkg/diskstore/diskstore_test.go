package diskstore

import (
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

	assert.Nil(t, quick.Check(func(h Hash) bool {
		store.SetRoot(&h)
		newH, err := store.GetRoot()
		assert.Nil(t, err)
		assert.Equal(t, h, newH, "Should return the hash we set.")
		return true
	}, nil), "quick.Check failed")
}
