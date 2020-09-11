@0xb4284328a4e09e0a;

using Protocol = import "protocol.capnp";

struct StoredBlob(T) {
  # The stored form of a blob.

  data @0 :T;
  # The value that was stored.

  ptrs @1 :List(Protocol.Hash);
  # Blobs pointed to by this blob. This list should be replaced with
  # `Ref(T)`s when building the capability table.
}
