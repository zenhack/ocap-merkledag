@0x96e412eec2bcb356;
# A storage backend that keeps all of the blobs in one big file.

using StoredBlob = import "storage.capnp".StoredBlob;

using Path = List(Text);
# A filesystem path, as a list of path segments. must not contain "/", and
# the path segments "", ".", and ".." are illegal.


struct StoreInfo {
  blobFile :group {
    path @2 :Path;
    # Path to the file containing all of the blobs, relative to the root directory.

    size @0 :UInt64;
    # The last known size of the file. If, on startup, the file is longer than
    # this, it means there were in-progress writes that were not committed;
    # in this case the file should just be truncated to discard the incomplete
    # work.
  }

  mapFile :group {
    path @3 :Path;
    size @4 :UInt64;
    # Analagous to the same fields in `blobFile`, but this file contains
    # interior nodes of a Trie mapping hashes to locations in `blobFile`.

    rootAddr @1 :Addr(TriePtr(StoredBlob(AnyPointer)));
    # The location of the root of the blob map.
  }
}

struct Addr(T) {
  # The location of a slice of bytes within the big file, which should be
  # interpreted as a capnp message whose root pointer has type T.

  offset @0 :UInt64;
  # The starting offset.

  length @1 :UInt32;
  # The length of the slice.

  flatMessage @2 :Bool;
  # If true, this message is "flat," i.e. there is no segment header.
}

struct TriePtr(T) {
  # A reference to a node in a trie, with byte sequences as keys, and values
  # of type `T`.
  union {
    leaf :group {
      # This points to a leaf node.

      addr @0 :Addr(T);
      # The address of the leaf.

      keySuffix @1 :Data;
      # The remainder of the key, beyond what was needed to find this node.
    }
    branch :group {
      # This points to a list of children,
      addr @2 :Addr(List(TriePtr(T)));
    }
    empty @3 :Void;
    # This is an empty trie
  }
}
