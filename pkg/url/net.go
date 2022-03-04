package url

import (
	"net"
	"net/url"

	"capnproto.org/go/capnp/v3/rpc"
)

func DialNetwork(u *url.URL) (_ rpc.Transport, release func(), _ error) {
	conn, err := net.Dial(u.Scheme, u.Opaque)
	if err != nil {
		return nil, nil, err
	}
	release = func() {
		conn.Close()
	}
	return rpc.NewStreamTransport(conn), release, nil
}
