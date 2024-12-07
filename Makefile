C = fpc

# -Xs = auto strip at program compile time
# -XX = smartlinking (ignore uneccessary code)
C_FLAGS = -Xs -XX -Fusrc/language -Fusrc/readtime 
C_FLAGS += -Isrc/language/include

# Install prefix and destination dir
PREFIX = /usr/local
DESTDIR =

ifeq ($(DEBUG), 1)
	NO_SMARTLINK = 1
	NO_STRIP = 1
	C_FLAGS += -g -B
# https://castle-engine.io/memory_leaks
	C_FLAGS += -glh
endif

ifeq ($(NO_SMARTLINK), 1)
	C_FLAGS := $(filter-out -XX, $(C_FLAGS))
endif

ifeq ($(NO_STRIP), 1)
	C_FLAGS := $(filter-out -Xs, $(C_FLAGS))
endif

all: clean build

build:
	$(C) $(C_FLAGS) src/fsh.lpr

#update:
#	git submodule update --init --recursive

clean: 
	$(RM) $(wildcard ./**/*.ppu) $(wildcard ./**/*.o) src/fsh src/fsh.exe

distclean: clean

install: build
	install -D src/fsh $(DESTDIR)$(PREFIX)/bin/fsh

uninstall:
	$(RM) -f $(DESTDIR)$(PREFIX)/bin/fsh

.PHONY: all build update clean distclean install uninstall