@0xeb26e91f28682410;

using Go = import "/go.capnp";
$Go.package("containers");
$Go.import("zenhack.net/go/ocap-md/pkg/schema/containers");

using Protocol = import "protocol.capnp";

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
    # An interior node in the tree.
    branches @0 :List(Branch);
  }

  struct Branch {
    key @0 :K;
    # A key for this branch.

    union {
      leaf @1 :V;
      # This is a leaf branch; it stores value associated with the key.

      node @2 :Protocol.Ref(Node);
      # This is another interior node; the subtree contains only keys >=
      # they `key` field, and less than the key field of the next branch,
      # if any.
    }
  }
}
