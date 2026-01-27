.PHONY: all build run clean

SOURCES := src/main.cpp \
           src/types.h src/types.cpp \
           src/ir_nodes.h src/ir_nodes.cpp \
           src/syntax_extract.h src/syntax_extract.cpp \
           src/ir_builder.h src/ir_builder.cpp
BINARY := build/custom_hdl_compiler

all: $(BINARY) run

$(BINARY): $(SOURCES)
	cmake --build build -j$(shell nproc)

build: $(BINARY)

run: $(BINARY)
	./$(BINARY) examples/test.v

clean:
	cmake --build build --target clean
