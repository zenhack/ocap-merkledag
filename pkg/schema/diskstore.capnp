@0xdc976fcd6fee6223;

using Go = import "/go.capnp";
$Go.package("diskstore");
$Go.import("zenhack.net/go/ocap-md/pkg/schema/diskstore");

using Protocol = import "protocol.capnp";

struct Addr(T) {
  offset @0 :UInt64;
  length @1 :UInt32;
  arena @2 :UInt32;
}

struct TrieMap {
  union {
    empty @0 :Void;
    leaf :group {
      prefix @1 :Data;
      addr @2 :Addr(AnyPointer);
    }
    branches @3 :Addr(Branches);
  }

  struct Branches {
    branches @0 :List(TrieMap);
  }
}

struct Manifest {
  blobMap @0 :Addr(TrieMap);
  root @1 :Protocol.ContentId;
}
