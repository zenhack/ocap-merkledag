package webui

import (
	_ "embed"
	"net/http"
	"strconv"

	"github.com/gorilla/mux"
)

var (
	//go:embed index.html
	indexHtml []byte

	//go:embed bundle.js
	bundleJs []byte
)

func handleStatic(r *mux.Router, path, contentType string, data []byte) {
	r.HandleFunc(path, func(w http.ResponseWriter, req *http.Request) {
		h := w.Header()
		h.Set("Content-Type", contentType)
		h.Set("Content-Length", strconv.Itoa(len(data)))
		w.WriteHeader(200)
		w.Write(data)
	})
}

func RegisterRoutes(r *mux.Router) {
	handleStatic(r, "/", "text/html", indexHtml)
	handleStatic(r, "/bundle.js", "application/javascript", bundleJs)
}
