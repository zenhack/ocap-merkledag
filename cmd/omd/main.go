package main

import (
	"flag"
	"log"
	"net"

	"capnproto.org/go/capnp/v3/rpc"

	"zenhack.net/go/ocap-md/pkg/diskstore"
)

var (
	path  = flag.String("path", "", "path to store")
	laddr = flag.String("laddr", ":2323", "Address to listen on")
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
		l, err := net.Listen("tcp", *laddr)
		if err != nil {
			log.Fatalf("Error binding to %q: %v", *laddr, err)
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
				BootstrapClient: api.Client,
			})
			go func() {
				defer conn.Close()
				_, _ = <-capnpConn.Done()
			}()
		}
	default:
		log.Fatal("Unknow command: ", cmd)
	}
}
