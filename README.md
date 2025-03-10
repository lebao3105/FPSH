## The Free Pascal Shell (FPSH)

This is a shell for operating systems.

## Features

This has basic things of a shell:

* Customizable shell prompt

* Switching between directories

* Command history (in fact it works on some platforms without any code from me - yes from RTL!)

Actually this shell was made for Free Pascal Operating System (FPOS), but it's also usable for other UNIXes as well.

Sorry FPOS developers :( There should be a compilation flag for FPOS.

Not tested on Windows yet.

## Building

Install make, git and of course fpc >=3.2.0 (this is a MUST).

Clone this project.

Run `make all` or `make build` to fetch submodules and build. After that run `src/fsh`.

Set `NO_SMARTLINK` to 1 to turn off smart linking option for fpc, also `NO_STRIP` to 1 to stop stripping the binary. Set `DEBUG` to 1 to add developer-oriented (add debug symbols etc) to the compiler. All of these are environment variables.

Run the program with `DEBUG=1` will enable FPSH's debugging messages, even in production builds.

## TODOs

* Add `popd` and `pushd` implementations (testing right now)

* Add aliases support (partially works)

* Add environment variables support (soon)