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

Install make, git and of course fpc.

Clone this project.

Run `make all` or `make build` to fetch submodules and build. After that run `src/fsh`.

Set `NO_SMARTLINK` to 1 to turn off smart linking option for fpc, also `NO_STRIP` to 1 to stop stripping the binary. Set `DEBUG` to 1 to not only enable 2 options above, but also add developer-oriented (add debug symbols etc) to the compiler. All of these are environment variables.

## TODOs

* Add shell settings (~/.fshrc)

* Add `popd` and `pushd` implementations

* Add aliases support

* Add environment variables support