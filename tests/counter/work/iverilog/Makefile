.PHONY: simulate clean

# Waveform database name
WAVES = waves.vcd

# Compiled file name
OUT = tb.out

# Directories
ROOT_DIR = ../..
RTL_DIR = $(ROOT_DIR)/rtl
TB_DIR = $(ROOT_DIR)/tb

# Source files
RTL_SRCS = $(shell find $(RTL_DIR) -type f)
TB_SRCS = $(shell find $(TB_DIR) -type f)
SRCS= $(RTL_SRCS) $(TB_SRCS)

# Default target
all: simulate

# Compile the TB and RTL
$(OUT): $(SRCS)
	iverilog -g2012 -o $(OUT) $(SRCS)

# Generate waves database
$(WAVES): $(OUT)
	vvp $(OUT) +WAVES=$(WAVES)

simulate: $(WAVES)

clean:
	rm -rf $(WAVES) $(OUT)
