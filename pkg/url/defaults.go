package url

func (d *Dialer) RegisterDefaults() {
	d.Register("tcp", DialNetwork)
	d.Register("tcp4", DialNetwork)
	d.Register("tcp6", DialNetwork)
	d.Register("unix", DialNetwork)
	d.Register("ws", DialWebsocket)
	d.Register("wss", DialWebsocket)
	d.Register("http", DialWebkey)
	d.Register("https", DialWebkey)
}
