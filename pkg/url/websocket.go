package url

import (
	"net/http"
	"net/url"
	"strings"

	wscapnp "zenhack.net/go/ocap-md/internal/websocket-capnp"

	"capnproto.org/go/capnp/v3/rpc"
	"github.com/gorilla/websocket"
)

func dialWebsocket(u string, h http.Header) (_ rpc.Transport, release func(), _ error) {
	c, _, err := websocket.DefaultDialer.Dial(u, h)
	if err != nil {
		return nil, nil, err
	}
	release = func() {
		c.Close()
	}
	return wscapnp.NewTransport(c), release, nil
}

func DialWebsocket(u *url.URL) (_ rpc.Transport, release func(), _ error) {
	return dialWebsocket(u.String(), nil)
}

func DialWebkey(u *url.URL) (_ rpc.Transport, release func(), _ error) {
	h := http.Header{}
	if u.Fragment != "" {
		h.Set("Authorization", "Bearer "+u.Fragment)
		u.Fragment = ""
	}
	switch u.Scheme {
	case "http":
		if !strings.HasSuffix(u.Path, "/") {
			u.Path += "/"
		}
		u.Path += "api"
		u.Scheme = "ws"
	case "https":
		u.Path += "/api"
		u.Scheme = "wss"
	}
	return dialWebsocket(u.String(), h)
}
