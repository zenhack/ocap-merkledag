@0xeb26e91f28682410;

using Go = import "/go.capnp";
$Go.package("containers");
$Go.import("zenhack.net/go/ocap-md/pkg/schema/containers");

using Ref = import "protocol.capnp".Ref;

struct KV(K, V) {
  # A key/value pair.
  key @0 :K;
  val @1 :V;
}

struct BPlusTree(K, V) {
  # A B+ tree, with keys K and values V.
  #
  # https://en.wikipedia.org/wiki/B%2B_tree

  root @0 :Node;
  # The root node of the tree.

  minBranches @1 :UInt32;
  # The minimum branch factor for a node. A node may be smaller than this if
  # there are fewer than minBranches values in the entire tree.

  maxBranches @2 :UInt32;
  # The maximum branch factor of a node.

  struct Node {
    # A node in the tree.
    union {
      leaf @0 :List(KV(K, V));
      # A leaf node; this is a list of sorted key, value pairs representing
      # entries in the map..

      interior :group {
        # An interior node in the tree.

        left @1 :Ref(Node);
        # left-most branch of the node; contains all keys < branches[0].key

        branches @2 :List(KV(K, Ref(Node)));
        # Branches. branches[i].val contains all keys >= branches[i].key and
        # (if i is not the last branch in the list) < branches[i+1].key.
      }
    }
  }
}
