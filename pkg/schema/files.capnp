@0xae1c58020a8c26ad;
# Data types for storing files.

using Go = import "/go.capnp";
$Go.package("files");
$Go.import("zenhack.net/go/ocap-md/pkg/schema/files");

using Protocol = import "protocol.capnp";
using Containers = import "containers.capnp";

struct File {
  # A node in the filesystem.

  metadata :union {
    # Other file metadata. Exactly what lives here may depend on the filesystem;
    # in the future we may add a variant for Windows filesystem metadata.
    noMetadata @1 :Void;
    unixMetadata @3 :UnixMetadata;
  }

  union {
    file @0 :BlobTree;
    # A regular file

    dir @4 :Protocol.Ref(Containers.BPlusTree(Text, File));
    # A directory; the argument is the contents of the directory.

    symlink @2 :Text;
    # A symlink. Argument is the target.
  }
}

struct UnixMetadata {
  permissions @0 :UInt32;
  # Unix permissions. masked with 0777; top bits should all be zero.
  # TODO: should we support sticky bits?

  modTime @1 :Int64;
  # Last modification time (unix timestamp).
}

struct BlobTree {
  # A tree containing the bytes of a large blob, constructed according to the
  # hashsplit spec:
  #
  # https://github.com/hashsplit/hashsplit-spec
  #
  # TODO: specify the hashsplit config used.

  union {
    leaf @0 :Protocol.Ref(Data);
    branch @1 :Protocol.Ref(List(BlobTree));
  }

  size @2 :UInt64;
}
