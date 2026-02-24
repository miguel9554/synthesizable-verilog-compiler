.PHONY: simulate clean

# Waveform database name
WAVES = waves.vcd

# Directories
ROOT_DIR = ../..
RTL_DIR = $(ROOT_DIR)/rtl
TB_DIR = $(ROOT_DIR)/tb

# Source files
RTL_SRCS = $(shell find $(RTL_DIR) -type f)
TB_SRCS = $(shell find $(TB_DIR) -type f)
SRCS= $(RTL_SRCS) $(TB_SRCS)
TOP_MODULE = tb

# Compiled file name
OUT = obj_dir/V$(TOP_MODULE)

# Default target
all: simulate

# Compile the TB and RTL
$(OUT): $(SRCS)
	verilator --trace --binary -Wno-TIMESCALEMOD --top $(TOP_MODULE) $(SRCS)

# Generate waves database
$(WAVES): $(OUT)
	./obj_dir/V$(TOP_MODULE) +WAVES=$(WAVES)

simulate: $(WAVES)

clean:
	rm -rf obj_dir $(WAVES)
