## The Free Pascal Shell (FPSH)

This is a shell for operating systems, not an interpreter for Pascal!

## Features

This has basic things of a shell:

* Customizable shell prompt (not now)

* Has internal commands

* Switching between directories

* Command history

This shell is mainly made for FPOS (Free Pascal Operating System) but also available for other OSes to use.

## Building

Install `libyaml` and remember where its installed.

Install make, git and of course fpc.

Clone this project.

Set `LIBYAML_PATH` to `<libyaml installation directory>/lib` and run `make all` or `make build` to fetch submodules and build. After that run `src/fsh`.

Set `NO_SMARTLINK` to 1 to turn off smart linking option for fpc, also `NO_STRIP` to 1 to stop stripping the binary.

## TODOs

* Add shell settings (~/.fshrc)

* Add `popd` and `pushd` implementations

* Add aliases support

* Add environment variables support