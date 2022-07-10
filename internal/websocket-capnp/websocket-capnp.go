// Package websocketcapnp integrates websockets with capnproto.
//
// TODO: this should probably get factored out into its own library/module
// at some point.
package websocketcapnp

import (
	"context"
	"io"
	"time"

	"capnproto.org/go/capnp/v3"
	"capnproto.org/go/capnp/v3/rpc"
	"capnproto.org/go/capnp/v3/rpc/transport"
	"github.com/gorilla/websocket"
	"github.com/ulikunitz/xz"
)

type Port interface {
	NextWriter(context.Context) (io.WriteCloser, error)
	NextReader(context.Context) (io.Reader, error)
	Close() error
}

func WebSocketPort(conn *websocket.Conn) Port {
	return websocketPort{conn}
}

func XZPort(p Port) Port {
	return transformPort{
		underlying: p,
		wrapR: func(r io.Reader) (io.Reader, error) {
			return xz.NewReader(r)
		},
		wrapW: func(w io.WriteCloser) (io.WriteCloser, error) {
			return xz.NewWriter(w)
		},
	}
}

func PortCodec(p Port) transport.Codec {
	return portCodec{p}
}

type websocketPort struct {
	conn *websocket.Conn
}

func (p websocketPort) NextWriter(ctx context.Context) (io.WriteCloser, error) {
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	return p.conn.NextWriter(websocket.BinaryMessage)
}

func (p websocketPort) NextReader(ctx context.Context) (io.Reader, error) {
	var (
		typ int
		r   io.Reader
		err error
	)
	for ctx.Err() == nil && typ != websocket.BinaryMessage {
		typ, r, err = p.conn.NextReader()
		if err != nil {
			return nil, err
		}
		if typ == websocket.PingMessage {
			err = p.conn.WriteMessage(websocket.PongMessage, nil)
			if err != nil {
				return nil, err
			}
		}
	}
	return r, nil
}

func (p websocketPort) Close() error {
	return p.conn.Close()
}

type portCodec struct {
	p Port
}

func (c portCodec) Encode(ctx context.Context, msg *capnp.Message) error {
	w, err := c.p.NextWriter(ctx)
	if err != nil {
		return err
	}
	defer w.Close()
	return capnp.NewEncoder(w).Encode(msg)
}

func (c portCodec) Decode(ctx context.Context) (*capnp.Message, error) {
	r, err := c.p.NextReader(ctx)
	if err != nil {
		return nil, err
	}
	return capnp.NewDecoder(r).Decode()
}

func (c portCodec) Close() error {
	return c.p.Close()
}

func (portCodec) SetPartialWriteTimeout(time.Duration) {}

type transformPort struct {
	underlying Port
	wrapR      func(io.Reader) (io.Reader, error)
	wrapW      func(io.WriteCloser) (io.WriteCloser, error)
}

func (p transformPort) NextWriter(ctx context.Context) (io.WriteCloser, error) {
	w, err := p.underlying.NextWriter(ctx)
	if err != nil {
		return nil, err
	}
	return p.wrapW(w)
}

func (p transformPort) NextReader(ctx context.Context) (io.Reader, error) {
	r, err := p.underlying.NextReader(ctx)
	if err != nil {
		return nil, err
	}
	return p.wrapR(r)
}

func (p transformPort) Close() error {
	return p.underlying.Close()
}

// Return an rpc.Transport that sends messages over the websocket connection.
// Sends each capnproto message in its own websocket binary message.
func NewTransport(conn *websocket.Conn) rpc.Transport {
	return transport.New(PortCodec(WebSocketPort(conn)))
}
