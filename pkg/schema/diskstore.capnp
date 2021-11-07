@0xdc976fcd6fee6223;

using Go = import "/go.capnp";
$Go.package("diskstore");
$Go.import("zenhack.net/go/ocap-md/pkg/schema/diskstore");

using Protocol = import "protocol.capnp";

struct Addr {
  offset @0 :UInt64;
  length @1 :UInt32;
  arena @2 :UInt32;
}

struct TrieMap {
  union {
    leaf :group {
      prefix @0 :Data;
      addr @1 :Addr;
    }
    branch @2 :List(Addr);
  }
}

struct Manifest {
  blobMap @0 :Addr;
  root @1 :Protocol.ContentId;
}
