@0xca7b236fb6fb07c3;

using Go = import "/go.capnp";
$Go.package("protocol");
$Go.import("zenhack.net/go/ocap-md/pkg/schema/protocol");

interface Map(K, V) {
  find @0 (key: K) -> (value: V);
}

interface Getter(T) {
  get @0 () -> (value :T);
}

interface Ref(T) extends(Getter(T)) {
  getStored @0 () -> (storedValue :Stored(T));
}

interface Setter(T) {
  set @0 (value :T);
}

interface Cell(T) extends (Setter(T), Getter(T)) {
}

interface TxCell(T) extends(Cell(T)) {
  txGet @0 () -> (value :T, setter :Setter(T));
}

interface RootPtr extends(TxCell(Ref(Dynamic))) {
}

interface Storage {
  put @0 [T] (value: T) -> (ref :Ref(T));
}

struct Dynamic {
  ref @0 :Ref(AnyPointer);
  typeId @1 :UInt64;
}

struct ContentId {
  digest @0 :Data;

  format @1 :Format;
  enum Format {
    segment @0;
    bytes @1;
  }

  algo @2 :Algo;
  enum Algo {
    sha256 @0;
  }
}

struct Stored(T) {
  data @0 :T;
  refs @1 :List(ContentId);
}

interface RootApi {
  blobMap @0 () -> (map :Map(ContentId, Ref(AnyPointer)));
  root @1 () -> (root :RootPtr);
  storage @2 () -> (storage: Storage);
}
