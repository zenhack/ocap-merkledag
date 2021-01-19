@0x96e412eec2bcb356;
# A storage backend that keeps all of the blobs in one big file.

using StoredBlob = import "storage.capnp".StoredBlob;
using Hash = import "protocol.capnp".Hash;

using Path = List(Text);
# A filesystem path, as a list of path segments. must not contain "/", and
# the path segments "", ".", and ".." are illegal.

using AnyBlob = StoredBlob(AnyPointer);

struct StoreInfo {
  # Metadata about a bigfile store.
  #
  # All hashes are assumed to be sha256; if in the future we want to
  # support additional hashes, the storage format will have to evolve.

  blobFile @0 :Arena;
  # Where the blobs themselves are stored.

  bookKeepingFile :group {
    arena @2 :Arena;
    # Arena in which to store all other bookkeeping info, i.e. everything
    # but the `StoreInfo` and the blobs themselves.

    garbage @4 :List(Addr(AnyPointer));
    # Queue of unused address ranges in the bookkeeping file, which need
    # be reclaimed for space by punching holes in the file.
  }

  blobs @1 :TrieMap(BlobInfo);
  # Map of the blobs in `blobArena`, keyed by their digest.

  volatileRoots @3 :TrieSet;
  # A set of digests of blobs to which there were live references at the last
  # checkpoint. On startup, each of these should have their refcounts
  # decremented by 1, as those references must have been dropped on shutdown,
  # if not sooner.

  gc :group {
    # Garbage blobs which are in the process of being reclaimed.

    toScan @5 :TrieMap(Addr(AnyBlob));
    # Blobs which need to be scanned. Scanning a blob entails:
    #
    # * decrementing the refcounts of each blob referred to by
    #   this blob
    # * Moving the blob from `toScan` to `toDelete`.

    toDelete @6 :TrieMap(Addr(AnyBlob));
    # Blobs which have already been scanned, and just need to be
    # actually deleted.
  }
}

struct Arena {
  # An on-disk arena in which to allocate storage.
  #
  # An arena is a single file, and new values are always appended
  # to the file. Space may be freed by punching holes in the file
  # (making it a sparse file), but:
  #
  # 1. Once written data may never be changed
  # 2. Care must be taken to ensure that only un-referenced spans
  #    of the arena are freed, of course.
  #
  # Freeing space normally works by making a checkpoint whose StoreInfo
  # notes that certain areas are garbage. Only once that checkpoint is
  # durable is it safe to actually delete them, since they may be live
  # in the previous checkpoint.

  path @1 :Path;
  # Path to the file, relative to the root directory.

  size @0 :UInt64;
  # The last known length of the file (which may be greater than
  # its on-disk footprint, if it is a sparse file due to deletions).
  #
  # If, on startup, the file is longer than this, it means there were
  # in-progress writes that were not committed; in this case the file
  # should just be truncated to discard the incomplete work.
}

struct Addr(T) {
  # The location of a slice of bytes within a file, which should be
  # interpreted as a capnp message whose root pointer has type T.

  offset @0 :UInt64;
  # The starting offset.

  length @1 :UInt32;
  # The length of the slice.

  flatMessage @2 :Bool;
  # If true, this message is "flat," i.e. there is no segment header.
}

struct BlobInfo {
  addr @0 :Addr(AnyBlob);
  refCount @1 :UInt64;
}

struct TrieMap(T) {
  # A map implemented as a Trie. Keys must be byte arrays of some fixed length.
  #
  # Each node in the trie is a separate capnproto message; messages are
  # allocated within an `Arena` and refer to one another with `Addr`s.
  #
  # Structure sharing is not allowed; implementations may assume there is
  # only one reference to a given `TrieMap`. This is important for knowing
  # when to free the space used by a message.

  struct Branch {
    # Contents of an interior node in the Trie.

    kids @0 :List(TrieMap(T));
    # A list, always of length 256, where kids[i] is a TrieMap
    # containing all elements whose key starts with `i`. The keys in
    # the child TrieMap are one byte shorter; the leading `i` byte is
    # omitted.
  }

  union {
    leaf :group {
      # This is a leaf node, which contains a value.

      value @0 :T;
      # The value at this leaf.

      keySuffix @1 :Data;
      # The remainder of the key, beyond what was needed to find this node.
    }

    branch @2 :Addr(Branch);
    # An interior node; contains a list of pointers to this node's
    # children. See comments for `Branch`.

    empty @3 :Void;
    # This is an empty trie
  }
}

using TrieSet = TrieMap(AnyPointer);
# A Trie intepreted as a set; the values are always null pointers
# and have no meaning.
