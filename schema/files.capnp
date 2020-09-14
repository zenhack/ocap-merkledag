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

  mtime @4 :Int64;
  # Unix mtime attribute

  ctime @5 :Int64;
  # Unix ctime attribute

  union {
    file :group {
      # A regular file

      contents @6 :Protocol.Ref(Protocol.BlobTree);
      # The bytes of the file

      size @8 :UInt64;
      # The total size of the file.
    }

    dir @7 :Protocol.Ref(List(File));
    # A directory; the argument is the contents of the directory.

    symlink @2 :Text;
    # A symlink. Argument is the target.

    fifo @3 :Void;
    # A FIFO.
  }
}
