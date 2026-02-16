#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <set>
#include <string>
#include <sys/types.h>

#include "passes/extractor.h"
#include "passes/elaboration.h"
#include "passes/constant_fold.h"
#include "passes/condition_normalization.h"
#include "passes/dce.h"
#include "passes/flop_resolve.h"
#include "passes/domain_resolve.h"
#include "passes/type_propagation.h"
#include "sim/simulator.h"
#include "util/debug.h"
#include "util/source_loc.h"

#include "slang/syntax/SyntaxTree.h"

using namespace slang::syntax;
using namespace custom_hdl;

void printUsage(const char* progName) {
    std::cerr << "Usage: " << progName << " [options] <verilog_file>" << std::endl;
    std::cerr << "\nOptions:" << std::endl;
    std::cerr << "  --passes <1|2>          Number of passes to run (default: 1)" << std::endl;
    std::cerr << "                          1 = extraction only (unresolved IR)" << std::endl;
    std::cerr << "                          2 = extraction + resolution" << std::endl;
    std::cerr << "  --simulate <config.yaml> Run cycle-based simulation (implies --passes 2)" << std::endl;
    std::cerr << "\nExample:" << std::endl;
    std::cerr << "  " << progName << " examples/test.v" << std::endl;
    std::cerr << "  " << progName << " --passes 2 examples/test.v" << std::endl;
    std::cerr << "  " << progName << " --simulate sim_config.yaml examples/test.v" << std::endl;
}

int main(int argc, char** argv) {
    // Parse command line arguments
    int numPasses = 1;
    std::string filename;
    std::string simConfigPath;

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
        } else if (std::strcmp(argv[i], "--simulate") == 0) {
            if (i + 1 >= argc) {
                std::cerr << "ERROR: --simulate requires a config file argument" << std::endl;
                printUsage(argv[0]);
                return 1;
            }
            simConfigPath = argv[++i];
            numPasses = 2;  // --simulate implies --passes 2
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

    try {

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
        auto resolvedModules = resolveModules(modules, tree->sourceManager());

        // Optimization passes
        for (auto& module : resolvedModules) {
            if (!module.dfg) continue;

            std::cout << "========================================" << std::endl;
            std::cout << "Module: " << module.name << std::endl;
            std::cout << "========================================" << std::endl;


            auto runPass = [&](const int number, const std::string& passName, auto passFn) {
                std::cout << "========================================" << std::endl;
                std::cout << "Performing " << passName << "..." << std::endl;
                std::cout << "========================================" << std::endl;
                module.dfg->validate();

                try {
                    passFn();
                    module.dfg->validate();
                } catch (const CompilerError& e) {
                    std::string dir = DEBUG_OUTPUT_DIR + "/" + module.name;
                    std::filesystem::create_directories(dir);
                    std::set<const DFGNode*> errorNodes;
                    if (e.errorNode) errorNodes.insert(e.errorNode);
                    std::ofstream(std::format("{}/{}_{}_ERROR.dot", dir, number, passName))
                        << module.dfg->toDot(passName + "_ERROR", errorNodes);
                    throw;
                }

                std::string dir = DEBUG_OUTPUT_DIR + "/" + module.name;
                std::filesystem::create_directories(dir);
                std::ofstream(std::format("{}/{}_{}.dot", dir, number, passName)) << module.dfg->toDot(passName);
                std::ofstream(std::format("{}/{}_{}.json", dir, number, passName)) << module.dfg->toJson();
            };

            runPass(0, "elaboration", []{});
            runPass(1, "type_propagation", [&]{ propagateTypes(*module.dfg); });
            runPass(2, "condition_normalization", [&]{ normalizeConditions(*module.dfg); });
            runPass(3, "constant_fold", [&]{ constantFold(*module.dfg); });
            runPass(4, "dce", [&]{ eliminateDeadCode(*module.dfg); });
            module.dfg->validateNoOrphans();
            runPass(5, "flop_resolve", [&]{ resolveFlops(module); });
            runPass(6, "domain_resolve", [&]{ resolveDomains(module); });
            validateNoCombLoops(module);
        }

        std::cout << "----------------------------------------" << std::endl;
        std::cout << "\nResolved IR (Pass 2):" << std::endl;
        std::cout << "========================================" << std::endl;

        for (const auto& module : resolvedModules) {
            module.print();
            std::cout << std::endl;
        }

        // Run simulation if requested
        if (!simConfigPath.empty()) {
            std::cout << "========================================" << std::endl;
            std::cout << "Running simulation..." << std::endl;
            std::cout << "========================================" << std::endl;

            auto simConfig = parseSimConfig(simConfigPath);

            // Find the top module
            const ResolvedModule* topModule = nullptr;
            for (const auto& mod : resolvedModules) {
                if (mod.name == simConfig.top_module) {
                    topModule = &mod;
                    break;
                }
            }
            if (!topModule) {
                throw CompilerError(std::format(
                    "Simulator: top module '{}' not found in resolved modules",
                    simConfig.top_module));
            }

            Simulator sim(*topModule, simConfig);
            sim.run();
        }
    }

    std::cout << "========================================" << std::endl;
    std::cout << "Compilation completed successfully!" << std::endl;
    std::cout << "Found " << modules.size() << " module(s)." << std::endl;

    } catch (const CompilerError& e) {
        std::cerr << "ERROR: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
