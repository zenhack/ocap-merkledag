@0xdc976fcd6fee6223;
# This file defines data strutures that are used in the on-disk storage format,
# but not in the protocol.
#
# The storage format consists of a directory containing files:
#
# - manifest: A Cap'n Proto message whose root is of type `Manifest`,
#   encoded using the stream encoding:
#   https://capnproto.org/encoding.html#serialization-over-a-stream
# - log-0, log-1, log-2, etc: large files containing a sequence of
#   stream-encoded messages of type LogEntry.
#
# With the exception of the `Manifest`, all structures are written
# to the log-* files sequentially, with a new file being created
# after the current one reaches a certain size; this helps maximize
# write throughput. The exact size used for rotation may change
# from one release to the next without breaking backwards compatibility;
# the implementation must not rely on assumptions about the sizes of
# logs in an existing data store.
#
# The manifest contains information necessary to bootstrap navigating
# the data store, including the addresses of various metadata structures
# and the content id of the current root object in the data store.

using Go = import "/go.capnp";
$Go.package("diskstore");
$Go.import("zenhack.net/go/ocap-md/pkg/schema/diskstore");

using Protocol = import "protocol.capnp";

const storageFormatVersion :UInt32 = 1;

struct Manifest {
  formatVersion @4 :UInt32;
  # The version of the storage format. If this is greater than
  # storageFormatVersion, the implementation assumes it may use features
  # it does not understand, and will refuse to operate on the data store.

  blobMap @0 :Addr;
  # The address of the LogEntry storing the root node of the blob map;
  # this log entry will have its union set to indexNode.

  root @1 :Protocol.ContentId;
  # The content id for the root object of the data store.

  lastLog :group {
    # Information about the most recent log-* file, which data is currently
    # being written to.

    size @2 :UInt64;
    # The size of the file. On startup, this may be smaller than the size of
    # the file found on disk -- if so, this most likely means that there was
    # a crash or power outage between writing data to the log file and making
    # a checkpoint; in this case the implementation truncates the file to
    # its expected length, thereby discarding changes since the last checkpoint.

    number @3 :UInt64;
    # The log file number, e.g. number = 2 means that `log-2` is the latest log
    # file.
  }
}

struct Addr {
  # The physical address of a stream-encoded capnp message somewhere
  # in one of the log-* files in the store.

  logNumber @2 :UInt64;
  # The target message is in the file "log-${logNumber}"

  offset @0 :UInt64;
  # The target message starts `offset` bytes into the file.

  length @1 :UInt32;
  # The target message is `length` bytes long.
}

struct LogEntry {
  # An entry in one of the log files.
  union {
    blob :group {
      # A blob of stored data.
      segment @0 :Data $Go.name("segment_");
      # A single capnp segment containing a Protocol.Stored(AnyPointer) as its root.
      # The value will be in canonical form, so that it can be hashed to derive or
      # validate its content id. Similarly, it is a `Data` field so it can be
      # either compressed, or, if not compressed, extracted as its own message
      # without a copy (e.g. for hashing).

      compression @2 :CompressionScheme;
      # The compression scheme applied to the segment, if any
      packed @3 :Bool;
      # Whether the segment uses packed encoding. If so, packing is applied *before*
      # compression, i.e. the stored Data is equal to compress(pack(originalSegment)).
    }

    indexNode @1 :TrieMap;
    # A node in the blob map, which maps hashes to physical locations in the log
    # files.
  }
}

enum CompressionScheme {
  # An identifer for a compression scheme.

  none @0; # No compression
  xz @1; # xz compression
}

struct TrieMap {
  # A map implemented as a Trie. Keys must be byte arrays of some fixed length.
  # Values are addresses.
  #
  # Each node in the trie is a separate capnproto message; messages are
  # allocated within an `Arena` and refer to one another with `Addr`s.
  #
  # Structure sharing is not allowed; implementations may assume there is
  # only one reference to a given `TrieMap`.

  union {
    leaf :group {
      # This subtree which contains exactly one value.

      prefix @0 :Data;
      # The remaining prefix for the key that this subtree contains.

      addr @1 :Addr;
      # The value stored for the key.
    }
    branches @2 :List(Addr);
    # This subtree may contain more than one value. The `branches` list
    # always has length 256, where each element is the address of another
    # `TrieMap`, containing all keys prefixed with its index.
  }
}
