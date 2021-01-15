@0xa2d9c03c65e784f5;
# On-disk data structures which are pure implementation detail. `storage.capnp`
# specifies types relevant to storage which are observable via the protocol.
#
# The store is a directory (henceforth the "root" directory) which has at its
# top-level a file named `StoreInfo`, containing a message whose root pointer
# is of type `StoreInfo`, encoded as described here:
#
#   https://capnproto.org/encoding.html#serialization-over-a-stream
#
# The contents of that message describe the layout of the rest of the store.

using BigFile = import "disk-bigfile.capnp";
using Protocol = import "protocol.capnp";

struct StoreInfo {
  # Describes the contents of the repository.
  union {
    loose @0 :LooseStoreInfo;
    # This is a loose-format store.

    bigFile @1 :BigFile.StoreInfo;
    # This is a big-file store.
  }

  # root @2 :Protocol.Hash
  # The root object in the store. TODO: rework the code so we
  # can uncomment this.
}

struct LooseStoreInfo {
  # A storage backend that keeps each blob in a separate file.
  # Relative to the root directory, the store will contain:
  #
  # * `tmp/` - empty at rest but used for temporary scratch space.
  # * `blobs/` - stores the actual on-disk blobs
  #   * `sha256/` - blobs stored with a sha256 digest (currently the only
  #     supported hash). Within this there are up to 256 directores named
  #     `00` to `ff`, where each corresponds to the first byte of the hash.
  #     each of those has another up to 256 directories with the same name
  #     corresponding to the second byte of the hash, and each of those
  #     contains regular files with the actual contents of the blobs,
  #     named with the remainder of the hash in hexidecimal. The directories
  #     are created on demand, the first time a blob would go in them.
}
