package main

import (
	_ "embed"
	"net/http"
	"strconv"

	"zenhack.net/go/ocap-md/pkg/diskstore"
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
	_ = diskstore.NewRootApi(s) // TODO: use this

	http.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		h := w.Header()
		h.Set("Content-Type", "text/html")
		h.Set("Content-Length", strconv.Itoa(len(indexHtml)))
		w.WriteHeader(200)
		w.Write(indexHtml)
	})
	panic(http.ListenAndServe(":8000", nil))
}

func chkfatal(err error) {
	if err != nil {
		panic(err)
	}
}
