package url

import (
	"errors"
	"net/url"

	"capnproto.org/go/capnp/v3/rpc"
)

var (
	ErrUnknownScheme = errors.New("Unknown URL scheme")
)

type Dialer struct {
	handlers map[string]Handler
}

type Handler = func(*url.URL) (_ rpc.Transport, release func(), _ error)

func (d *Dialer) init() {
	if d.handlers == nil {
		d.handlers = make(map[string]Handler)
	}
}

func (d *Dialer) Register(scheme string, h Handler) {
	d.init()
	d.handlers[scheme] = h
}

func (d *Dialer) DialURL(u *url.URL) (_ rpc.Transport, release func(), _ error) {
	d.init()
	h, ok := d.handlers[u.Scheme]
	if !ok {
		return nil, nil, ErrUnknownScheme
	}
	return h(u)
}

func (d *Dialer) Dial(addr string) (_ rpc.Transport, release func(), _ error) {
	u, err := url.Parse(addr)
	if err != nil {
		return nil, nil, err
	}
	return d.DialURL(u)
}
