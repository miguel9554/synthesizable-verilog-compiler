# Setup Instructions for Claude Code

Follow these steps to set up the Custom HDL compiler project.

## 1. Initialize the Repository

```bash
# Create project directory
mkdir custom-hdl
cd custom-hdl

# Initialize git
git init

# Copy all the provided files into this directory
# (README.md, Dockerfile, CMakeLists.txt, src/, examples/, .gitignore)
```

## 2. Add Slang as a Submodule

```bash
# Add slang submodule
git submodule add https://github.com/MikePopoloski/slang.git external/slang

# Initialize and fetch the submodule
git submodule update --init --recursive
```

## 3. Build with Docker (Recommended)

```bash
# Build the Docker image (first time only)
docker build -t custom-hdl .

# Run the container with your workspace mounted
docker run -it --rm -v $(pwd):/workspace custom-hdl

# Inside the container:
mkdir build && cd build
cmake ..
make -j$(nproc)

# Test it
./custom_hdl_compiler ../examples/test.v
```

## 4. Manual Build (Alternative)

If you prefer to build without Docker:

```bash
# Install dependencies (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install -y build-essential cmake git clang-18

# Build
mkdir build && cd build
cmake ..
make -j$(nproc)

# Run
./custom_hdl_compiler ../examples/test.v
```

## 5. Commit Initial Setup

```bash
# Add files to git
git add .
git commit -m "Initial project setup with Slang integration"
```

## Expected Output

When you run `./custom_hdl_compiler ../examples/test.v`, you should see:

```
========================================
Custom HDL Compiler
========================================
Parsing: ../examples/test.v
----------------------------------------
Processing module: counter
  Found input port: clk
  Found input port: reset
  Found output port: count
  Found variable: count of type reg[7:0]
  Found always block
Processing module: adder
  Found input port: a
  Found input port: b
  Found output port: sum
Parsing successful!
----------------------------------------

Building IR...
----------------------------------------

Generated IR:
========================================
Module: counter
  Inputs: clk reset 
  Outputs: count 
  Body:
    Variable: count [reg[7:0], 1 bits]
    Always (always)

Module: adder
  Inputs: a b 
  Outputs: sum 
  Body:

========================================
Compilation completed successfully!
Found 2 module(s).
```

## Next Steps

1. Extend the IR nodes in `src/ir_builder.h` to capture more information
2. Improve the visitor in `src/ir_builder.cpp` to handle more Verilog constructs
3. Add your custom HDL syntax extensions
4. Implement IR transformations and optimizations
5. Add output generation for your custom format

## Modifying Slang

If you need to modify Slang's source code:

```bash
cd external/slang
# Make your changes
git add .
git commit -m "Custom Slang modifications"
cd ../..
git add external/slang
git commit -m "Update Slang submodule"
```

## Troubleshooting

### Submodule not initialized
```bash
git submodule update --init --recursive
```

### CMake can't find Slang
Make sure the submodule is in `external/slang/` and contains a `CMakeLists.txt`.

### Compiler errors
Ensure you have C++20 support (GCC 10+ or Clang 12+).
