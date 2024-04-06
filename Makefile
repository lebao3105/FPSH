C = fpc

# -Xs = auto strip at program compile time
# -XX = smartlinking (ignore uneccessary code)
C_FLAGS = -Xs -XX

# libyaml path. NOT C INCLUDE HEADERS PATH!
# The latest one on macOS (with Homebrew), change it for your PC (and DO NOT COMMIT this)
LIBYAML_PATH = /usr/local/Cellar/libyaml/0.2.5/lib

# Include flags for libpasyaml
C_FLAGS += -Fusubprojects/libpasyaml/libpasc-algorithms/source
C_FLAGS += -Fusubprojects/libpasyaml/pascalutils/source
C_FLAGS += -Fusubprojects/libpasyaml/source
C_FLAGS += -Fl$(LIBYAML_PATH) -k-lyaml # -k = add flag to linker

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

clean: 
	$(RM) $(wildcard ./**/*.ppu) $(wildcard ./**/*.o) src/fsh src/fsh.exe

distclean: clean

install: build
	install -D src/fsh $(DESTDIR)$(PREFIX)/bin/fsh

uninstall:
	$(RM) -f $(DESTDIR)$(PREFIX)/bin/fsh

.PHONY: all build update clean distclean install uninstall