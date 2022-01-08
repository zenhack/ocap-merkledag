package main

import (
	"context"
	"flag"
	"log"
	"net"
	"os"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/rpc"

	"zenhack.net/go/ocap-md/pkg/blobtree"
	"zenhack.net/go/ocap-md/pkg/diskstore"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
)

var (
	path = flag.String("path", "", "path to store")
	addr = flag.String("addr", ":2323", "Address to listen on")
)

func main() {
	flag.Parse()
	cmd := flag.Args()[0]
	switch cmd {
	case "init":
		s, err := diskstore.Create(*path)
		if err != nil {
			log.Fatal("Error creating store: ", err)
		}
		s.Close()
	case "serve":
		l, err := net.Listen("tcp", *addr)
		if err != nil {
			log.Fatalf("Error binding to %q: %v", *addr, err)
		}

		s, err := diskstore.Open(*path)
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
		conn, err := net.Dial("tcp", *addr)
		chkfatal(err)
		defer conn.Close()
		capnpConn := rpc.NewConn(rpc.NewStreamTransport(conn), nil)
		api := protocol.RootApi{capnpConn.Bootstrap(ctx)}
		resStorage, rel := api.Storage(ctx, nil)
		defer rel()
		s := resStorage.Storage()
		ref, err := blobtree.WriteStream(ctx, s, os.Stdin)
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
	default:
		log.Fatal("Unknow command: ", cmd)
	}
}

func chkfatal(err error) {
	if err != nil {
		panic(err)
	}
}
