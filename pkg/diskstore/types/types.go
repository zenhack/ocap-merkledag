package types

import (
	"capnproto.org/go/capnp/v3"

	"zenhack.net/go/ocap-md/pkg/schema/diskstore"
)

type Addr struct {
	Offset int64
	Length uint32
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
	da.SetOffset(uint64(a.Offset))
	da.SetLength(a.Length)
}

func DecodeAddr(addr diskstore.Addr) Addr {
	return Addr{
		Offset: int64(addr.Offset()),
		Length: addr.Length(),
	}
}
