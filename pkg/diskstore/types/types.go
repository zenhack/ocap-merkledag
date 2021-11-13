package types

import (
	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type Addr struct {
	Arena     uint32
	ArenaAddr ArenaAddr
}

type ArenaAddr struct {
	Offset int64
	Size   uint32
}

func (a Addr) Encode() diskstore.Addr {
	_, seg, err := capnp.NewMessage(capnp.SingleSegment(nil))
	if err != nil {
		panic(err)
	}
	root, err := diskstore.NewRootAddr(seg)
	if err != nil {
		panic(err)
	}
	a.EncodeInto(root)
	return root
}

func (a Addr) EncodeInto(da diskstore.Addr) {
	da.SetArena(a.Arena)
	da.SetOffset(uint64(a.ArenaAddr.Offset))
	da.SetLength(a.ArenaAddr.Size)
}

func DecodeAddr(addr diskstore.Addr) Addr {
	return Addr{
		Arena: addr.Arena(),
		ArenaAddr: ArenaAddr{
			Offset: int64(addr.Offset()),
			Size:   addr.Length(),
		},
	}
}
