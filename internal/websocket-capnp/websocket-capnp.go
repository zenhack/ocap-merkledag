// Package websocketcapnp integrates websockets with capnproto.
//
// TODO: this should probably get factored out into its own library/module
// at some point.
package websocketcapnp

import (
	"context"
	"time"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/rpc"
	"capnproto.org/go/capnp/v3/rpc/transport"
	"github.com/gorilla/websocket"
)

// Return an rpc.Transport that sends messages over the websocket connection.
// Sends each capnproto message in its own websocket binary message.
func NewTransport(conn *websocket.Conn) rpc.Transport {
	return transport.New(websocketCodec{conn})
}

type websocketCodec struct {
	conn *websocket.Conn
}

func (c websocketCodec) Encode(ctx context.Context, msg *capnp.Message) error {
	err := ctx.Err()
	if err != nil {
		return err
	}
	data, err := msg.Marshal()
	if err != nil {
		return err
	}
	return c.conn.WriteMessage(websocket.BinaryMessage, data)
}

func (c websocketCodec) Decode(ctx context.Context) (*capnp.Message, error) {
	var (
		typ  int
		data []byte
		err  error
	)
	for ctx.Err() == nil && typ != websocket.BinaryMessage {
		typ, data, err = c.conn.ReadMessage()
		if err != nil {
			return nil, err
		}
		if typ == websocket.PingMessage {
			err = c.conn.WriteMessage(websocket.PongMessage, nil)
			if err != nil {
				return nil, err
			}
		}
	}
	return capnp.Unmarshal(data)
}

func (websocketCodec) SetPartialWriteTimeout(time.Duration) {}

func (c websocketCodec) Close() error {
	return c.conn.Close()
}
