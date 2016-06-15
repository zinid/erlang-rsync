Erlang librsync wrapper
========================

[![Build Status](https://travis-ci.org/zinid/erlang-rsync.svg?branch=master)](https://travis-ci.org/zinid/erlang-rsync)

erlang-rsync is an Erlang wrapper of [librsync](http://librsync.sourcefrog.net)
library. It provides basic functions for direct file manipulations
(`sig/1`, `delta/2`, etc) as well as low-level functions
for fine-grained control (`sig_init/0`, `delta_update/2`, etc).
The low-level interface is somewhat similar to the hashing functions
from `crypto` module.

Building
--------

### Requirements

- GNU Make.
- GCC.
- [rebar](https://github.com/rebar/rebar).
- [librsync](http://librsync.sourcefrog.net) (runtime library and header files).
- Erlang/OTP 17.1 or higher.

### Installation

```sh
$ git clone git://github.com/zinid/erlang-rsync.git
$ cd erlang-rsync
$ ./configure
$ make
```

Usage
-----

### API

The whole API documentation is located in the `doc` directory and can be built
using `edoc`

```sh
$ make doc
```

### Example

Let's say we have two files: `local_file` and `remote_file`, and we want `local_file`
to have the content of `remote_file`.
We first generate the signature of `local_file`. The signature is written in `sig_file`:
```
> rsync:sig("local_file", "sig_file").
ok.
```
We then generate the delta from `remote_file` and `sig_file`.
The delta is written in `delta_file`:
```
> rsync:delta("sig_file", "remote_file", "delta_file").
ok.
```
Finally, using `delta_file` and `local_file` we reconstruct `remote_file`. The
result is written in `new_local_file`.
```
> rsync:patch("local_file", "delta_file", "new_local_file").
ok.
```

### Limitation

For `patch/2`, `patch/3` and `patch_init/1` functions to work the whole content of
the original file should be read into the memory.
This is due to a limitation of librsync API and Erlang's NIF API.
So don't use these functions for extremely large files.
