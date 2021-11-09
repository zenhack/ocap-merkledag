package types

import (
	"zenhack.net/go/ocap-md/pkg/diskstore/filearena"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type Addr struct {
	Arena     uint32
	ArenaAddr filearena.Addr
}

func DecodeAddr(addr diskstore.Addr) Addr {
	return Addr{
		Arena: addr.Arena(),
		ArenaAddr: filearena.Addr{
			Offset: int64(addr.Offset()),
			Size:   addr.Length(),
		},
	}
}
