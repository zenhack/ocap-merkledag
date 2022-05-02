package main

import (
	"context"
	"flag"
	"log"
	"net"

	"bazil.org/fuse"
	"bazil.org/fuse/fs"
	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/rpc"

	"zenhack.net/go/ocap-md/pkg/diskstore"
	protoimpl "zenhack.net/go/ocap-md/pkg/diskstore/protocol"
	"zenhack.net/go/ocap-md/pkg/files"
	omdfuse "zenhack.net/go/ocap-md/pkg/files/fuse"
	filescapnp "zenhack.net/go/ocap-md/pkg/schema/files"
	"zenhack.net/go/ocap-md/pkg/schema/protocol"
	capnp_url "zenhack.net/go/ocap-md/pkg/url"
)

var (
	storepath = flag.String("storepath", "", "path to store")
	addr      = flag.String("addr", "tcp::2323", "Address to connect to")
	laddr     = flag.String("laddr", ":2323", "(tcp) address to listen on")

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
		l, err := net.Listen("tcp", *laddr)
		if err != nil {
			log.Fatalf("Error binding to %q: %v", *addr, err)
		}

		s, err := diskstore.Open(*storepath)
		if err != nil {
			log.Fatal("Error opening store: ", err)
		}
		defer s.Close()
		api := protoimpl.NewRootApi(s)
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
	case "mount":
		ctx := context.Background()
		trans, releaseTrans, err := connect(*addr)
		chkfatal(err)
		defer releaseTrans()
		conn := rpc.NewConn(trans, nil)
		api := protocol.RootApi{conn.Bootstrap(ctx)}
		resRoot, rel := api.Root(ctx, nil)
		defer rel()
		resRootGet, rel := resRoot.Root().Get(ctx, nil)
		defer rel()
		ref := protocol.Ref{resRootGet.Value().Client()}
		resGet, rel := ref.Get(ctx, nil)
		defer rel()
		s, err := resGet.Value().Struct()
		chkfatal(err)
		rootFile := filescapnp.File{s}
		filesystem := omdfuse.FS(func() (filescapnp.File, error) {
			return rootFile, nil
		})
		fuseConn, err := fuse.Mount(
			*path,
			fuse.FSName("ocap-md"),
			fuse.Subtype("ocap-md"),
		)
		chkfatal(err)
		defer fuseConn.Close()
		chkfatal(fs.Serve(fuseConn, filesystem))
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
	d := &capnp_url.Dialer{}
	d.RegisterDefaults()
	return d.Dial(addr)
}
