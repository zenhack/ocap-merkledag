# This file is lifted from Sandstorm (and paired down a bit). TODO: we should
# instead package the sandstorm schema and depend on those (perhaps packaging
# util.capnp separately to avoid dependency bloat, since it really has little
# to do with Sandstorm per se.

# Sandstorm - Personal Cloud Sandbox
# Copyright (c) 2014 Sandstorm Development Group, Inc. and contributors
# All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

@0xecd50d792c3d9992;

interface ByteStream {
  # Represents a destination for a stream of bytes. The bytes are ordered, but boundaries between
  # messages are not semantically important.
  #
  # Streams are push-oriented (traditionally, "output streams") rather than pull-oriented ("input
  # streams") because this most easily allows multiple packets to be in-flight at once while
  # allowing flow control at either end. If we tried to design a pull-oriented stream, it would
  # suffer from problems:
  # * If we used a naive read() method that returns a simple data blob, you would need to make
  #   multiple simultaneous calls to deal with network latency. However, those calls could
  #   potentially return in the wrong order. Although you could restore order by keeping track of
  #   the order in which the calls were made, this would be a lot of work, and most callers would
  #   probably fail to do it.
  # * We could instead have a read() method that returns a blob as well as a capability to read the
  #   next blob. You would then make multiple calls using pipelining. Even in this case, though,
  #   an unpredictable event loop could schedule a pipelined call's return before the parent call.
  #   Moreover, the interface would be awkward to use and implement. E.g. what happens if you call
  #   read() twice on the same capability?

  write @0 (data :Data) ; # -> stream; # TODO: re-enable stream once capnp-haskell includes stream.capnp
  # Add bytes.
  #
  # It's safe to make overlapping calls to `write()`, since Cap'n Proto enforces E-Order and so
  # the calls will be delivered in order. However, it is a good idea to limit how much data is
  # in-flight at a time, so that it doesn't fill up buffers and block other traffic. On the other
  # hand, having only one `write()` in flight at a time will not fully utilize the available
  # bandwidth if the connection has any significant latency, so parallelizing a few `write()`s is
  # a good idea.
  #
  # Similarly, the implementation of `ByteStream` can delay returning from `write()` as a way to
  # hint to the caller that it should hold off on further writes.

  done @1 ();
  # Call after the last write to indicate that there is no more data. If the `ByteStream` is
  # discarded without a call to `done()`, the callee must assume that an error occurred and that
  # the data is incomplete.
  #
  # This will not return until all bytes are successfully written to their final destination.
  # It will throw an exception if any error occurs, including if the total number of bytes written
  # did not match `expectSize()`.

  expectSize @2 (size :UInt64);
  # Optionally called to let the receiver know exactly how much data will be written. This should
  # normally be called before the first write(), but if called later, `size` indicates how many
  # more bytes to expect _after_ the call. It is an error by the caller if more or fewer bytes are
  # actually written before `done()`; this also implies that all calls to `expectSize()` must be
  # consistent. The caller will ignore any exceptions thrown from this method, therefore it
  # is not necessary for the callee to actually implement it.
}