@0xae1c58020a8c26ad;
# Data types for storing files.

using Protocol = import "protocol.capnp";

struct File {
  # A node in the filesystem.

  name @0 :Text;
  # The name of the file

  permissions @1 :UInt32;
  # Unix permissions. masked with 0777; top bits should all be zero.
  # TODO: should we support sticky bits?

  modTime @3 :Int64;
  # Last modification time (unix timestamp).

  union {
    file @5 :Protocol.BlobTree;
    # A regular file

    dir @4 :Protocol.Ref(List(File));
    # A directory; the argument is the contents of the directory.

    symlink @2 :Text;
    # A symlink. Argument is the target.
  }
}
