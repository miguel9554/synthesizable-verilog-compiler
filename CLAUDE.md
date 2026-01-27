# Project Instructions

## Slang Library Context Optimization

This project uses the slang library (external/slang/) for SystemVerilog parsing. To optimize context window usage, follow these rules:

### Allowed slang files (read freely)
Only these slang header files define the interface used by this project:
- `external/slang/include/slang/syntax/SyntaxTree.h`
- `external/slang/include/slang/syntax/SyntaxVisitor.h`
- `external/slang/include/slang/syntax/SyntaxKind.h`
- `external/slang/include/slang/syntax/AllSyntax.h`

### Restricted slang files (ask first)
Before reading any other file under `external/slang/`, ask the user for permission and explain why it's needed.

### Never read (unless explicitly requested)
- Slang implementation files (`*.cpp`)
- Slang test files
- Slang documentation files
