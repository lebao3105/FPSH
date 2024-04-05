C = fpc

# -Xs = auto strip at program compile time
# -XX = smartlinking (ignore uneccessary code)
C_FLAGS = -Xs -XX

# Include flags for libpasyaml
C_FLAGS += -Fusubprojects/libpasyaml/libpasc-algorithms/source
C_FLAGS += -Fusubprojects/libpasyaml/pascalutils/source
C_FLAGS += -Fusubprojects/libpasyaml/source

# Install prefix and destination dir
PREFIX = /usr/local
DESTDIR =

ifeq ($(NO_SMARTLINK), 1)
	C_FLAGS := $(filter-out -XX, $(C_FLAGS))
endif

ifeq ($(NO_STRIP), 1)
	C_FLAGS := $(filter-out -Xs, $(C_FLAGS))
endif

all: clean build

build: update
	$(C) $(C_FLAGS) src/fsh.lpr

update:
	git submodule update --init --recursive

clean: $(wildcard ./**/*.ppu) $(wildcard ./**/*.o) src/fsh
	$(RM) -f $?

distclean: clean

install: build
	install -D src/fsh $(DESTDIR)$(PREFIX)/bin/fsh

uninstall:
	$(RM) -f $(DESTDIR)$(PREFIX)/bin/fsh

.PHONY: all build update clean distclean install uninstall