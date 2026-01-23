# Custom HDL Compiler

A compiler for a custom Hardware Description Language (HDL) with Verilog interoperability, built on top of Slang.

## Project Structure

```
custom-hdl/
├── external/
│   └── slang/              # Slang submodule (Verilog parser)
├── src/
│   ├── main.cpp            # Entry point
│   ├── ir_builder.cpp      # AST to IR transformation
│   └── ir_builder.h        # IR node definitions
├── examples/
│   └── test.v              # Example Verilog file
├── Dockerfile              # Docker environment
├── CMakeLists.txt          # Build configuration
└── README.md               # This file
```

## Quick Start with Docker (Recommended)

### 1. Build the Docker image

```bash
docker build -t custom-hdl .
```

### 2. Run the container

```bash
docker run -it --rm -v $(pwd):/workspace custom-hdl
```

This mounts your current directory into the container, so changes you make are reflected immediately.

### 3. Inside the container, build the project

```bash
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### 4. Run the parser

```bash
./custom_hdl_compiler ../examples/test.v
```

## Manual Setup (Without Docker)

### Prerequisites

- CMake 3.15+
- C++20 compatible compiler (GCC 10+, Clang 12+)
- Git

### Clone with Submodules

```bash
git clone --recursive https://github.com/yourusername/custom-hdl.git
cd custom-hdl
```

If you already cloned without `--recursive`:

```bash
git submodule update --init --recursive
```

### Build

```bash
mkdir build
cd build
cmake ..
make -j$(nproc)
```

### Run

```bash
./custom_hdl_compiler ../examples/test.v
```

## Development Workflow

### Building After Changes

```bash
cd build
make -j$(nproc)
```

### Modifying Slang

The `external/slang` directory is a git submodule. You can modify it directly:

```bash
cd external/slang
# Make your changes
git add .
git commit -m "Custom slang modifications"
cd ../..
git add external/slang
git commit -m "Update slang submodule with custom changes"
```

### Updating Slang from Upstream

```bash
cd external/slang
git fetch origin
git merge origin/master
cd ../..
git add external/slang
git commit -m "Update slang to latest upstream"
```

## CMake Commands Reference

### Clean Build

```bash
rm -rf build
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### Debug Build

```bash
mkdir build-debug && cd build-debug
cmake -DCMAKE_BUILD_TYPE=Debug ..
make -j$(nproc)
```

### Release Build

```bash
mkdir build-release && cd build-release
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j$(nproc)
```

### Verbose Build (see actual commands)

```bash
make VERBOSE=1
```

### Build Only (without running cmake again)

```bash
cd build
make -j$(nproc)
```

### Install (optional)

```bash
cd build
cmake --install . --prefix /usr/local
```

## Project Goals

1. **Verilog Compatibility**: Parse and understand existing Verilog code
2. **Custom IR**: Build an intermediate representation suitable for HDL extensions
3. **Extensibility**: Easy to add new language features and optimizations

## Current Status

- ✅ Slang integration for Verilog parsing
- ✅ Basic AST visitor infrastructure
- 🚧 IR node definitions (in progress)
- 🚧 AST to IR transformation (in progress)
- ⬜ Custom syntax extensions
- ⬜ IR optimizations
- ⬜ Output generation

## Contributing

Feel free to modify and extend! The visitor pattern in `src/ir_builder.cpp` is where AST transformation happens.

## Resources

- [Slang Documentation](https://sv-lang.com/)
- [Slang GitHub](https://github.com/MikePopoloski/slang)
- [SystemVerilog Spec](https://ieeexplore.ieee.org/document/8299595)
