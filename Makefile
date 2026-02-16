.PHONY: all build run clean clean_project clean_all debug debug-configure debug-build gdb

# Find all source files using wildcards
CPP_SOURCES := $(shell find src -name '*.cpp')
HEADER_SOURCES := $(shell find src -name '*.h')
SOURCES := $(CPP_SOURCES) $(HEADER_SOURCES)
BINARY := build/custom_hdl_compiler
passes ?= 1

all: $(BINARY) run

$(BINARY): $(SOURCES)
	cmake --build build -j$(shell nproc)

build: $(BINARY)

run: $(BINARY)
	./$(BINARY) --passes $(passes) examples/test.v

# Debug targets
debug-configure:
	cmake -B build -DCMAKE_BUILD_TYPE=Debug

debug-build: debug-configure
	cmake --build build -j$(shell nproc)

debug: debug-build
	./$(BINARY) --passes $(passes) examples/test.v

gdb: debug-build
	gdb --args ./$(BINARY) --passes $(passes) examples/test.v

# Clean only project artifacts (preserves slang)
clean_project:
	cmake --build build --target clean_project

clean: clean_project

# Clean all artifacts including slang
clean_all:
	cmake --build build --target clean
