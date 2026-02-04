import os
import sys

if len(sys.argv) != 2:
    print(f"Usage: {sys.argv[0]} <base>")
    sys.exit(1)

base = sys.argv[1]

with open("../../external/slang/scripts/syntax.txt") as f:
    blocks = f.read().strip().split("\n\n")

os.makedirs(base, exist_ok=True)

with open(f"{base}/member_blocks.txt", "w") as out, \
     open(f"{base}/member_summary.txt", "w") as out_summary:
    for b in blocks:
        if f"base={base}" in b:
            out.write(b + "\n\n")
            lines = b.split("\n")
            out_summary.write(lines[0] + "\n")
