// Package websocketcapnp integrates websockets with capnproto.
//
// TODO: this should probably get factored out into its own library/module
// at some point.
package websocketcapnp

import (
	"context"
	"log"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/rpc"
	rpccp "capnproto.org/go/capnp/v3/std/capnp/rpc"
	"github.com/gorilla/websocket"
)

// Return an rpc.Transport that sends messages over the websocket connection.
// Sends each capnproto message in its own websocket binary message.
func NewTransport(conn *websocket.Conn) rpc.Transport {
	return websocketTransport{conn}
}

type websocketTransport struct {
	conn *websocket.Conn
}

func (t websocketTransport) NewMessage(ctx context.Context) (
	_ rpccp.Message,
	send func() error,
	_ capnp.ReleaseFunc,
	_ error,
) {
	arena := capnp.SingleSegment(nil)
	msg, seg, err := capnp.NewMessage(arena)
	if err != nil {
		return rpccp.Message{}, func() error { return nil }, func() {}, err
	}
	send = func() error {
		data, err := msg.Marshal()
		if err != nil {
			log.Print("Error getting segment: ", err)
			return err
		}
		return t.conn.WriteMessage(websocket.BinaryMessage, data)
	}
	release := func() {}
	rpcMsg, err := rpccp.NewRootMessage(seg)
	return rpcMsg, send, release, err
}

func (t websocketTransport) RecvMessage(ctx context.Context) (rpccp.Message, capnp.ReleaseFunc, error) {
	var (
		typ  int
		data []byte
		err  error
	)
	for ctx.Err() == nil && typ != websocket.BinaryMessage {
		typ, data, err = t.conn.ReadMessage()
		if err != nil {
			return rpccp.Message{}, func() {}, err
		}
		if typ == websocket.PingMessage {
			t.conn.WriteMessage(websocket.PongMessage, nil)
		}
	}
	if err = ctx.Err(); err != nil {
		return rpccp.Message{}, func() {}, err
	}

	msg, err := capnp.Unmarshal(data)
	if err != nil {
		return rpccp.Message{}, func() {}, err
	}
	rpcMsg, err := rpccp.ReadRootMessage(msg)
	return rpcMsg, func() {}, err
}

func (t websocketTransport) Close() error {
	return t.conn.Close()
}
