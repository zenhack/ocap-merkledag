// package triemap implements an on-disk trie, where keys are byte slices which
// must all have the same length, and values are addresses.
package triemap

import (
	"errors"
)

var (
	ErrNotFound  = errors.New("Not Found")
	ErrShortKey  = errors.New("Key too short")
	ErrMalformed = errors.New("Malformed triemap node")
)
