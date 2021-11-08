package filearena

import (
	"bytes"
	"fmt"
	"os"
	"testing"
	"testing/quick"
)

// To test:
//
// - Sync()
// - Clear()

func writeThenRead(t *testing.T, fa *FileArena, data []byte) (Addr, error) {
	addr, err := fa.Put(data)
	if err != nil {
		return Addr{}, fmt.Errorf("writeThenRead/Put: %v", err)
	}
	readBack, err := fa.Get(addr)
	if err != nil {
		return Addr{}, fmt.Errorf("writeThenRead/Get: %v", err)
	}
	if bytes.Compare(data, readBack) != 0 {
		return Addr{}, fmt.Errorf("Read back wrong data; expected %v but got %v", data, readBack)
	}
	return addr, nil
}

type addrResult struct {
	addr Addr
	err  error
}

func testParallelWrites(t *testing.T, fa *FileArena, data [][]byte) {
	done := make([]chan addrResult, len(data))
	for i, v := range data {
		ch := make(chan addrResult)
		done[i] = ch
		go func() {
			addr, err := writeThenRead(t, fa, v)
			ch <- addrResult{
				addr: addr,
				err:  err,
			}
		}()
	}
	results := make([]addrResult, len(data))
	for i, ch := range done {
		results[i] = <-ch
	}
	for i, res := range results {
		if res.err != nil {
			t.Fatalf("chunk #%d: %v", i, res.err)
		}
		v, err := fa.Get(res.addr)
		if err != nil {
			t.Fatalf("chunk #%d: expected %v but got %v", i, data[i], v)
		}
	}
}

func TestParallelWrites(t *testing.T) {
	quick.Check(func(data [][]byte) bool {
		const path = "test-arena"
		f, err := os.Create(path)
		if err != nil {
			t.Fatalf("Failed to open test arena: %v", err)
		}
		defer os.Remove(path)
		defer f.Close()
		arena := New(f, 0)
		testParallelWrites(t, arena, data)
		return true
	}, nil)
}
