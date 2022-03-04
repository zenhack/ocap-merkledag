package main

import (
	"context"
	"flag"
	"log"
	"net"
	"net/http"
	"net/url"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/rpc"
	"github.com/gorilla/websocket"

	wscapnp "zenhack.net/go/ocap-md/internal/websocket-capnp"
	"zenhack.net/go/ocap-md/pkg/diskstore"
	"zenhack.net/go/ocap-md/pkg/files"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

var (
	storepath = flag.String("storepath", "", "path to store")
	addr      = flag.String("addr", ":2323", "Address to listen on")

	path = flag.String("path", "", "path to file to upload")
)

func main() {
	flag.Parse()
	cmd := flag.Args()[0]
	switch cmd {
	case "init":
		s, err := diskstore.Create(*storepath)
		if err != nil {
			log.Fatal("Error creating store: ", err)
		}
		s.Close()
	case "serve":
		l, err := net.Listen("tcp", *addr)
		if err != nil {
			log.Fatalf("Error binding to %q: %v", *addr, err)
		}

		s, err := diskstore.Open(*storepath)
		if err != nil {
			log.Fatal("Error opening store: ", err)
		}
		defer s.Close()
		api := diskstore.NewRootApi(s)
		for {
			conn, err := l.Accept()
			if err != nil {
				log.Print("Accept():", err)
				continue
			}
			trans := rpc.NewStreamTransport(conn)
			capnpConn := rpc.NewConn(trans, &rpc.Options{
				BootstrapClient: api.Client.AddRef(),
			})
			go func() {
				defer conn.Close()
				_, _ = <-capnpConn.Done()
			}()
		}
	case "put":
		ctx := context.Background()
		trans, releaseTrans, err := connect(*addr)
		chkfatal(err)
		defer releaseTrans()
		conn := rpc.NewConn(trans, nil)
		api := protocol.RootApi{conn.Bootstrap(ctx)}
		resStorage, rel := api.Storage(ctx, nil)
		defer rel()
		s := resStorage.Storage()
		ref, err := files.PutFile(ctx, s, *path)
		chkfatal(err)
		resRoot, rel := api.Root(ctx, nil)
		defer rel()
		resSet, rel := resRoot.Root().Set(ctx, func(p protocol.Setter_set_Params) error {
			seg := p.Segment()
			capId := seg.Message().AddCap(ref.Client.AddRef())
			p.SetValue(capnp.NewInterface(seg, capId).ToPtr())
			return nil
		})
		defer rel()
		_, err = resSet.Struct()
		chkfatal(err)
	case "get":
		ctx := context.Background()
		trans, releaseTrans, err := connect(*addr)
		chkfatal(err)
		defer releaseTrans()
		conn := rpc.NewConn(trans, nil)
		api := protocol.RootApi{conn.Bootstrap(ctx)}

		resRoot, rel := api.Root(ctx, nil)
		defer rel()
		resGet, rel := resRoot.Root().Get(ctx, nil)
		defer rel()
		ref := protocol.Ref{resGet.Value().Client()}
		chkfatal(files.Download(ctx, *path, ref))
	default:
		log.Fatal("Unknown command: ", cmd)
	}
}

func chkfatal(err error) {
	if err != nil {
		panic(err)
	}
}

func connect(addr string) (_ rpc.Transport, release func(), _ error) {
	u, err := url.Parse(addr)
	if err != nil {
		// Not a url. Try treating it as an address.
		return connectNetwork(addr)
	}
	h := http.Header{}
	if u.Fragment != "" {
		h.Set("Authorization", "Bearer "+u.Fragment)
		u.Fragment = ""
	}
	switch u.Scheme {
	case "http":
		u.Path += "/api"
		u.Scheme = "ws"
	case "https":
		u.Path += "/api"
		u.Scheme = "wss"
	}
	wsUrl := u.String()
	c, _, err := websocket.DefaultDialer.Dial(wsUrl, h)
	if err != nil {
		return nil, nil, err
	}
	return wscapnp.NewTransport(c), func() {}, nil
}

func connectNetwork(addr string) (_ rpc.Transport, release func(), _ error) {
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		return nil, nil, err
	}
	release = func() {
		conn.Close()
	}
	return rpc.NewStreamTransport(conn), release, nil
}
