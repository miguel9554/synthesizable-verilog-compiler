#include <iostream>
#include <memory>
#include "ir_builder.h"

#include "slang/syntax/SyntaxTree.h"

using namespace slang;
using namespace slang::syntax;
using namespace custom_hdl;

void printUsage(const char* progName) {
    std::cerr << "Usage: " << progName << " <verilog_file>" << std::endl;
    std::cerr << "\nExample:" << std::endl;
    std::cerr << "  " << progName << " examples/test.v" << std::endl;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        printUsage(argv[0]);
        return 1;
    }

    std::string filename = argv[1];

    std::cout << "========================================" << std::endl;
    std::cout << "Custom HDL Compiler" << std::endl;
    std::cout << "========================================" << std::endl;
    std::cout << "Parsing: " << filename << std::endl;
    std::cout << "----------------------------------------" << std::endl;

    // Parse the Verilog file using Slang
    auto treeResult = SyntaxTree::fromFile(filename);

    if (!treeResult) {
        std::cerr << "ERROR: Failed to load file: " << filename << std::endl;
        return 1;
    }

    auto tree = std::move(treeResult).value();

    // Check for syntax errors
    auto& diagnostics = tree->diagnostics();
    bool hasErrors = false;
    for (const auto& diag : diagnostics) {
        if (diag.isError()) {
            hasErrors = true;
            break;
        }
    }
    if (hasErrors) {
        std::cerr << "Syntax errors found: " << diagnostics.size() << " diagnostic(s)" << std::endl;
        return 1;
    }

    std::cout << "Parsing successful!" << std::endl;
    std::cout << "----------------------------------------" << std::endl;

    // Build our custom IR
    std::cout << "\nBuilding IR..." << std::endl;
    auto modules = buildIR(*tree);

    std::cout << "----------------------------------------" << std::endl;
    std::cout << "\nGenerated IR:" << std::endl;
    std::cout << "========================================" << std::endl;

    // Print the IR
    for (const auto& module : modules) {
        module->print();
        std::cout << std::endl;
    }

    std::cout << "========================================" << std::endl;
    std::cout << "Compilation completed successfully!" << std::endl;
    std::cout << "Found " << modules.size() << " module(s)." << std::endl;

    return 0;
}
