package main

import (
	_ "embed"
	"log"
	"net/http"
	_ "net/http/pprof"

	wscapnp "zenhack.net/go/ocap-md/internal/websocket-capnp"
	"zenhack.net/go/ocap-md/pkg/diskstore"
	protoimpl "zenhack.net/go/ocap-md/pkg/diskstore/protocol"
	"zenhack.net/go/ocap-md/pkg/webui"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/rpc"
	"github.com/gorilla/mux"
	"github.com/gorilla/websocket"
)

func main() {
	const storePath = "/var/store"
	s, err := diskstore.Open(storePath)
	if err != nil {
		s, err = diskstore.Create(storePath)
	}
	chkfatal(err)
	api := protoimpl.NewRootApi(s)

	r := mux.NewRouter()

	r.HandleFunc("/api", func(w http.ResponseWriter, req *http.Request) {
		wsConn, err := (&websocket.Upgrader{}).Upgrade(w, req, nil)
		if err != nil {
			log.Print("Error upgrading to websocket: ", err)
			return
		}
		transport := wscapnp.NewTransport(wsConn)
		defer transport.Close()
		rpcConn := rpc.NewConn(transport, &rpc.Options{
			BootstrapClient: capnp.Client(api).AddRef(),
		})
		defer rpcConn.Close()
		<-rpcConn.Done()
	})

	webui.RegisterRoutes(r)
	http.Handle("/", r)
	panic(http.ListenAndServe(":8000", nil))
}

func chkfatal(err error) {
	if err != nil {
		panic(err)
	}
}
