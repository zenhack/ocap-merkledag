@0xb4284328a4e09e0a;

using Protocol = import "protocol.capnp";

struct StoredBlob(T) {
  # The stored form of a blob.

  data @0 :T;
  # The value that was stored.

  ptrs @1 :List(StoredClient);
  # Blobs pointed to by this blob. This list should be replaced with
  # `Ref(T)`s when building the capability table.
}

struct StoredClient {
  # A stored client for a Ref. We could almost just use Protocol.Hash
  # directly, but we want to be able to tell the difference between
  # a null client and one that is actually stored, and struct lists
  # can't contain null pointers, so we need to add an indirection.
  #
  # We *could* just have this be a wrapper around a single null
  # pointer. But right now (2020-9-13) the Haskell implementation's
  # high-level API doesn't distinguish between null pointers and
  # default avlues.
  #
  # We also might want to just define a `Maybe` type, but right now
  # haskell capnp doesn't support generics either... Fortunately
  # we can ret-con that later without changing the wire format.

  union {
    null @0 :Void;
    hash @1 :Protocol.Hash;
  }
}
