module zenhack.net/go/ocap-md

go 1.17

require (
	capnproto.org/go/capnp/v3 v3.0.0-alpha.1.0.20220202181208-17b4e866fb30 // indirect
	github.com/bobg/hashsplit v1.1.3 // indirect
	github.com/chmduquesne/rollinghash v4.0.0+incompatible // indirect
)

// I(zenhack) sometimes use this when hacking on go-capnp in tandem with this
// repository:
//
// replace capnproto.org/go/capnp/v3 => /home/isd/src/foreign/go-capnproto2
