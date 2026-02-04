#!/usr/bin/env python3
"""
Explore Verilog AST as JSON.

Usage:
    python explore_ast.py <verilog_file>

This will parse the Verilog file and open an interactive Python session
with the AST loaded as a dictionary.
"""

import json
import subprocess
import sys
import tempfile
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent
BUILD_DIR = SCRIPT_DIR.parent.parent / "build" / "scratchpad" / "ast-explore"
AST_TO_JSON = BUILD_DIR / "ast_to_json"


def parse_verilog(verilog_path: str) -> dict:
    """Parse a Verilog file and return the AST as a dictionary."""
    verilog_path = Path(verilog_path).resolve()

    if not verilog_path.exists():
        raise FileNotFoundError(f"Verilog file not found: {verilog_path}")

    if not AST_TO_JSON.exists():
        raise FileNotFoundError(
            f"ast_to_json executable not found at {AST_TO_JSON}\n"
            "Build it first with: make build"
        )

    with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as tmp:
        json_path = tmp.name

    try:
        result = subprocess.run(
            [str(AST_TO_JSON), str(verilog_path), json_path],
            capture_output=True,
            text=True,
        )

        if result.returncode != 0:
            raise RuntimeError(f"ast_to_json failed:\n{result.stderr}")

        with open(json_path) as f:
            return json.load(f)
    finally:
        Path(json_path).unlink(missing_ok=True)


def print_tree(node, indent=0, max_depth=3):
    """Print a simplified view of the AST tree."""
    if indent > max_depth * 2:
        print("  " * indent + "...")
        return

    if isinstance(node, dict):
        kind = node.get("kind", "?")
        print("  " * indent + f"{kind}")
        for key, value in node.items():
            if key == "kind":
                continue
            if isinstance(value, (dict, list)):
                print("  " * (indent + 1) + f"{key}:")
                print_tree(value, indent + 2, max_depth)
    elif isinstance(node, list):
        for i, item in enumerate(node):
            print_tree(item, indent, max_depth)


def main():
    if len(sys.argv) != 2:
        print(__doc__)
        sys.exit(1)

    verilog_file = sys.argv[1]
    print(f"Parsing: {verilog_file}")

    ast = parse_verilog(verilog_file)

    print("\nAST loaded into 'ast' variable.")
    print("Useful functions:")
    print("  - print_tree(ast, max_depth=3)  # print tree structure")
    print("  - ast.keys()                    # see top-level keys")
    print("  - json.dumps(ast, indent=2)    # pretty print as JSON")
    print()

    # Drop into interactive mode
    import code
    code.interact(local={
        "ast": ast,
        "print_tree": print_tree,
        "json": json,
    })


if __name__ == "__main__":
    main()
