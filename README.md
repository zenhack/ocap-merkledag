WIP OCap-friendly merkle-dag data store with [hash splitting][1].
Inspired by [Perkeep][2], but with more flexible and lighter weight
access control built on object capabilities.

I plan on using this as part of a backup solution for [sandstorm][3],
but it is conceptually a stand-alone project.

# Building

```sh
GO_CAPNP_SRC=/path/to/go-capnproto2/checkout go generate ./...
go build ./...
```

[1]: https://github.com/hashsplit/hashsplit-spec
[2]: https://perkeep.org
[3]: https://sandstorm.io
