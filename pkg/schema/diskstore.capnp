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
  # A map implemented as a Trie. Keys must be byte arrays of some fixed length.
  # Values are addresses.
  #
  # Each node in the trie is a separate capnproto message; messages are
  # allocated within an `Arena` and refer to one another with `Addr`s.
  #
  # Structure sharing is not allowed; implementations may assume there is
  # only one reference to a given `TrieMap`. This is important for knowing
  # when to free the space used by a message.

  union {
    leaf :group {
      # This subtree which contains exactly one value.

      prefix @0 :Data;
      # The remaining prefix for the key that this subtree contains.

      addr @1 :Addr(AnyPointer);
      # The value stored for the key.
    }
    branches @2 :List(Addr(AnyPointer));
    # This subtree may contain more than one value. The `branches` list
    # always has length 256, where each element is the address of another
    # `TrieMap`, containing all keys prefixed with its index.
  }
}

struct Manifest {
  blobMap @0 :Addr(TrieMap);
  root @1 :Protocol.ContentId;
}
