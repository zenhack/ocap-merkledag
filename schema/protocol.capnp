@0xdcd5a61cb18421e5;

interface Store(T) {
  put @0 (value :T) -> (hash :Hash, ref :Ref(T));
  findByHash @1 (hash :Hash) -> (ref :Ref(T));
}

interface Ref(T) {
  get @0 () -> (value :T);
}

struct Hash {
  # A cryptographic hash value.

  algo @1 :HashAlgo;
  # The algorithm used to compute the hash

  digest @0 :Data;
  # The bytes of the hash itself.
}

enum HashAlgo {
  sha256 @0;
}
