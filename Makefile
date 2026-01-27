.PHONY: all build run clean

# Find all source files using wildcards
CPP_SOURCES := $(wildcard src/*.cpp)
HEADER_SOURCES := $(wildcard src/*.h)
SOURCES := $(CPP_SOURCES) $(HEADER_SOURCES)
BINARY := build/custom_hdl_compiler

all: $(BINARY) run

$(BINARY): $(SOURCES)
	cmake --build build -j$(shell nproc)

build: $(BINARY)

run: $(BINARY)
	./$(BINARY) examples/test.v

clean:
	cmake --build build --target clean
