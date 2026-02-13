#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <sys/types.h>

#include "passes/extractor.h"
#include "passes/elaboration.h"
#include "passes/constant_fold.h"
#include "passes/condition_normalization.h"
#include "passes/dce.h"
#include "passes/flop_resolve.h"
#include "passes/type_propagation.h"
#include "util/debug.h"

#include "slang/syntax/SyntaxTree.h"

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
    }

    // Pass 2: Resolution (optional)
    if (numPasses >= 2) {
        std::cout << "========================================" << std::endl;
        std::cout << "\nPass 2: Resolving types and expressions..." << std::endl;

        std::cout << "========================================" << std::endl;
        std::cout << "Performing elaboration..." << std::endl;
        std::cout << "========================================" << std::endl;
        auto resolvedModules = resolveModules(modules);

        // Optimization passes
        for (auto& module : resolvedModules) {
            if (!module.dfg) continue;

            std::cout << "========================================" << std::endl;
            std::cout << "Module: " << module.name << std::endl;
            std::cout << "========================================" << std::endl;


            auto dumpDFG = [&](const int number, const std::string& passName) {
                std::string dir = DEBUG_OUTPUT_DIR + "/" + module.name;
                std::filesystem::create_directories(dir);
                std::ofstream(std::format("{}/{}_{}.dot", dir, number, passName)) << module.dfg->toDot(passName);
                std::ofstream(std::format("{}/{}_{}.josn", dir, number, passName)) << module.dfg->toJson();
            };
            dumpDFG(0, "elaboration");

            std::cout << "========================================" << std::endl;
            std::cout << "Performing type propagation..." << std::endl;
            std::cout << "========================================" << std::endl;
            propagateTypes(*module.dfg);
            dumpDFG(1, "type_propagation");

            std::cout << "========================================" << std::endl;
            std::cout << "Performing condition normalization..." << std::endl;
            std::cout << "========================================" << std::endl;
            normalizeConditions(*module.dfg);
            dumpDFG(2, "condition_normalization");

            std::cout << "========================================" << std::endl;
            std::cout << "Performing constant folding..." << std::endl;
            std::cout << "========================================" << std::endl;
            constantFold(*module.dfg);
            dumpDFG(3, "constant_fold");

            std::cout << "========================================" << std::endl;
            std::cout << "Performing Dead Code Elimination..." << std::endl;
            std::cout << "========================================" << std::endl;
            eliminateDeadCode(*module.dfg);
            dumpDFG(4, "dce");

            std::cout << "========================================" << std::endl;
            std::cout << "Performing Flop resolution..." << std::endl;
            std::cout << "========================================" << std::endl;
            resolveFlops(module);
            dumpDFG(5, "flop_resolve");
        }

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
