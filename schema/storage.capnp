@0xb4284328a4e09e0a;

using Protocol = import "protocol.capnp";

struct StoredBlob(T) {
  # The stored form of a blob. We always canonicalize the blob before storage.
  # In addition, we normalize the order of capability pointers, such that
  # enumerating the capability pointers (ignoring null pointers) in a
  # left-to-right depth-first order will result in indicies 0, 1, 2, ...

  data @0 :T;
  # The value that was stored.

  ptrs @1 :List(Protocol.Hash);
  # Blobs pointed to by this blob. This list should be replaced with
  # `Ref(T)`s when building the capability table; the stored order is the same
  # as for the capability table.
}
