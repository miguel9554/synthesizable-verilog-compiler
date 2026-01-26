.PHONY: all build run clean

SOURCES := src/main.cpp src/ir_builder.cpp src/ir_builder.h
BINARY := build/custom_hdl_compiler

all: $(BINARY) run

$(BINARY): $(SOURCES)
	cmake --build build -j$(shell nproc)

build: $(BINARY)

run: $(BINARY)
	./$(BINARY) examples/test.v

clean:
	cmake --build build --target clean
