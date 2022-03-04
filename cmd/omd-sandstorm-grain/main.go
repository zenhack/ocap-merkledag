package main

import (
	_ "embed"
	"log"
	"net/http"
	"strconv"

	wscapnp "zenhack.net/go/ocap-md/internal/websocket-capnp"
	"zenhack.net/go/ocap-md/pkg/diskstore"

	"capnproto.org/go/capnp/v3/rpc"
	"github.com/gorilla/mux"
	"github.com/gorilla/websocket"
)

//go:embed index.html
var indexHtml []byte

func main() {
	const storePath = "/var/store"
	s, err := diskstore.Open(storePath)
	if err != nil {
		s, err = diskstore.Create(storePath)
	}
	chkfatal(err)
	api := diskstore.NewRootApi(s)

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
			BootstrapClient: api.Client,
		})
		defer rpcConn.Close()
		<-rpcConn.Done()
	})

	r.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		h := w.Header()
		h.Set("Content-Type", "text/html")
		h.Set("Content-Length", strconv.Itoa(len(indexHtml)))
		w.WriteHeader(200)
		w.Write(indexHtml)
	})
	http.Handle("/", r)
	panic(http.ListenAndServe(":8000", nil))
}

func chkfatal(err error) {
	if err != nil {
		panic(err)
	}
}
