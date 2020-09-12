@0xdcd5a61cb18421e5;
# This schema defines the wire protocol used to talk to the data store.

interface Store(T) {
  # A data store.

  put @0 (value :T) -> (hash :Hash, ref :Ref(T));
  # Add a value to the store. Returns the hash of the internally stored object
  # (the format of which is not part of this protocol, but is defined in
  # storage.capnp) and a capability to value, which can be used to fetch it
  # without a capability to the store itself.
  #
  # When storing a value, for each capability pointer reachable from the value,
  # the server will:
  #
  # 1. Wait for the capability to resolve, if it is a promise.
  # 2. If the capability is a `Ref` exported by the server, the capability will
  #    be retained and accessible from the value that is restored by calling
  #    `ref.get()`
  # 3. Otherwise, the capability will be replaced by a null pointer.
  #
  # The value will be canonicalized before storage, and the order of capabilities
  # In the capability table will be normalized as well, so structuarally equal
  # values will share the same storage.

  putBytesStreaming() -> (stream :Util.ByteStream, ref :Ref(BlobTree));
  # Add a large binary blob to the store. `stream` is a ByteStream to which the
  # bytes of the blob should be a written, and `ref` is a promise that will be
  # resolved once `stream`'s done() method is called, and will point to the
  # resulting `BlobTree`.

  findByHash @1 (hash :Hash) -> (ref :Ref(T));
  # Find a value by hash, getting a capability to the value.

  subStore @2 () -> (store :Store(T));
  # Get an empty sub-store of this store. The resulting store will share the
  # same underlying storage, but will be logically empty, i.e. initially
  # findByHash will not find anything, until objects have been inserted into
  # the sub-store via its put() and putBytesStreaming() methods.
}

struct BlobTree {
  # A tree containing the bytes of a large blob, constructed according to the
  # hashsplit spec:
  #
  # https://github.com/hashsplit/hashsplit-spec
  #
  # TODO: specify the hashsplit config used.

  union {
    leaf @0 :Data;
    branch @1 :List(Branch)
  }

  struct Branch {
    size :UInt64;
    ref :Ref(BlobTree);
  }
}

interface Ref(T) {
  # A reference to a value stored in a `Store`.

  get @0 () -> (value :T);
  # Retrieve the value pointed to by the Ref.
}

struct Hash {
  # A cryptographic hash value.

  algo @1 :HashAlgo;
  # The algorithm used to compute the hash

  digest @0 :Data;
  # The bytes of the hash itself.
}

enum HashAlgo {
  # A cryptographic hash algorithm. Currently only sha256 is supported,
  # and we have no plans to support other algorithms, but this may be
  # updated someday if vulnerabilities are found in sha256.
  sha256 @0;
}
