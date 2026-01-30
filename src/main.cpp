#include <cstring>
#include <iostream>
#include <memory>
#include <fstream>

#include "ir_builder.h"
#include "resolver.h"

#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/CSTSerializer.h"
#include "slang/text/Json.h"

using namespace slang;
using namespace slang::syntax;
using namespace custom_hdl;

void printUsage(const char* progName) {
    std::cerr << "Usage: " << progName << " [options] <verilog_file>" << std::endl;
    std::cerr << "\nOptions:" << std::endl;
    std::cerr << "  --passes <1|2>  Number of passes to run (default: 1)" << std::endl;
    std::cerr << "                  1 = extraction only (unresolved IR)" << std::endl;
    std::cerr << "                  2 = extraction + resolution" << std::endl;
    std::cerr << "\nExample:" << std::endl;
    std::cerr << "  " << progName << " examples/test.v" << std::endl;
    std::cerr << "  " << progName << " --passes 2 examples/test.v" << std::endl;
}

int main(int argc, char** argv) {
    // Parse command line arguments
    int numPasses = 1;
    std::string filename;

    for (int i = 1; i < argc; ++i) {
        if (std::strcmp(argv[i], "--passes") == 0) {
            if (i + 1 >= argc) {
                std::cerr << "ERROR: --passes requires an argument" << std::endl;
                printUsage(argv[0]);
                return 1;
            }
            numPasses = std::atoi(argv[++i]);
            if (numPasses < 1 || numPasses > 2) {
                std::cerr << "ERROR: --passes must be 1 or 2" << std::endl;
                printUsage(argv[0]);
                return 1;
            }
        } else if (argv[i][0] == '-') {
            std::cerr << "ERROR: Unknown option: " << argv[i] << std::endl;
            printUsage(argv[0]);
            return 1;
        } else {
            if (!filename.empty()) {
                std::cerr << "ERROR: Multiple input files not supported" << std::endl;
                printUsage(argv[0]);
                return 1;
            }
            filename = argv[i];
        }
    }

    if (filename.empty()) {
        printUsage(argv[0]);
        return 1;
    }

    std::cout << "========================================" << std::endl;
    std::cout << "Custom HDL Compiler" << std::endl;
    std::cout << "========================================" << std::endl;
    std::cout << "Parsing: " << filename << std::endl;
    std::cout << "Passes: " << numPasses << std::endl;
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

    // Pass 1: Build unresolved IR
    std::cout << "\nPass 1: Building unresolved IR..." << std::endl;
    auto modules = buildIR(*tree);

    std::cout << "----------------------------------------" << std::endl;
    std::cout << "\nUnresolved IR (Pass 1):" << std::endl;
    std::cout << "========================================" << std::endl;

    for (const auto& module : modules) {
        module->print();
        std::cout << std::endl;
        for (const auto& assign: module->assignStatements){
            if (assign) {
                const char* outputFile = "assign.json";
                JsonWriter writer;
                writer.setPrettyPrint(true);

                CSTSerializer serializer(writer);
                serializer.serialize(*assign);

                std::ofstream out(outputFile);
                if (!out) {
                    std::cerr << "Failed to open output file: " << outputFile << "\n";
                    return 1;
                }

                out << writer.view();
                std::cout << "Wrote AST to: " << outputFile << "\n";
            }
        }
    }

    // Pass 2: Resolution (optional)
    if (numPasses >= 2) {
        std::cout << "========================================" << std::endl;
        std::cout << "\nPass 2: Resolving types and expressions..." << std::endl;
        std::cout << "(STUB - returning placeholder values)" << std::endl;

        auto resolvedModules = resolveModules(modules);

        std::cout << "----------------------------------------" << std::endl;
        std::cout << "\nResolved IR (Pass 2):" << std::endl;
        std::cout << "========================================" << std::endl;

        for (const auto& module : resolvedModules) {
            module.print();
            std::cout << std::endl;
        }
    }

    std::cout << "========================================" << std::endl;
    std::cout << "Compilation completed successfully!" << std::endl;
    std::cout << "Found " << modules.size() << " module(s)." << std::endl;

    return 0;
}
