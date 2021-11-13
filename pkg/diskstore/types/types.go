package types

import (
	"zenhack.net/go/ocap-md/pkg/diskstore/filearena"
	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type Addr struct {
	Arena     uint32
	ArenaAddr filearena.Addr
}

func (a Addr) EncodeInto(da diskstore.Addr) {
	da.SetArena(a.Arena)
	da.SetOffset(uint64(a.ArenaAddr.Offset))
	da.SetLength(a.ArenaAddr.Size)
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
