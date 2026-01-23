# Use Ubuntu 24.04 as base
FROM ubuntu:24.04

# Avoid prompts from apt
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    python3 \
    python3-pip \
    ninja-build \
    clang-18 \
    libc++-18-dev \
    libc++abi-18-dev \
    && rm -rf /var/lib/apt/lists/*

# Set clang as default compiler (optional, but slang works well with clang)
ENV CC=clang-18
ENV CXX=clang++-18

# Create workspace
WORKDIR /workspace

# Set up git for submodules
RUN git config --global --add safe.directory /workspace
RUN git config --global --add safe.directory /workspace/external/slang

# Default command: open a bash shell
CMD ["/bin/bash"]
