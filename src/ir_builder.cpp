#include "ir_builder.h"
#include "syntax_extract.h"

#include <iostream>
#include <stdexcept>

#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/SyntaxVisitor.h"

using namespace slang::syntax;
using namespace custom_hdl;

namespace {

// Visitor class that builds our custom IR from slang syntax tree
class IRBuilderVisitor : public SyntaxVisitor<IRBuilderVisitor> {
public:
    std::vector<std::unique_ptr<IRModule>> modules;
    IRModule* currentModule = nullptr;

    void handle(const ModuleDeclarationSyntax& node) {
        auto headerInfo = extractModuleHeader(*node.header);

        auto module = std::make_unique<IRModule>();
        module->name = headerInfo.name;
        module->parameters = std::move(headerInfo.parameters);
        module->inputs = std::move(headerInfo.inputs);
        module->outputs = std::move(headerInfo.outputs);

        if (node.blockName) throw std::runtime_error("Can't parse blockName");

        // std::cout << "Processing module: " << module->name << std::endl;

        // Set current module context
        currentModule = module.get();

        // Visit module members
        visitDefault(node);

        // Store the completed module
        modules.push_back(std::move(module));
        currentModule = nullptr;
    }

    void handle(const ProceduralBlockSyntax& node) {
        if (!currentModule) return;

        auto always = std::make_unique<IRAlways>();

        // Extract sensitivity list based on block kind
        switch (node.kind) {
            case SyntaxKind::AlwaysBlock:
                always->sensitivity = "always";
                // std::cout << "  Found always block" << std::endl;
                break;
            case SyntaxKind::AlwaysCombBlock:
                always->sensitivity = "always_comb";
                // std::cout << "  Found always_comb block" << std::endl;
                break;
            case SyntaxKind::AlwaysFFBlock:
                always->sensitivity = "always_ff";
                // std::cout << "  Found always_ff block" << std::endl;
                break;
            case SyntaxKind::AlwaysLatchBlock:
                throw std::runtime_error("Latch not allowed.");
            case SyntaxKind::InitialBlock:
                throw std::runtime_error("Initial block not synthesizable");
            default:
                throw std::runtime_error("Unknown procedural block kind");
        }

        currentModule->body.push_back(std::move(always));

        visitDefault(node);
    }
};

} // anonymous namespace

namespace custom_hdl {

std::vector<std::unique_ptr<IRModule>> buildIR(const SyntaxTree& tree) {
    IRBuilderVisitor visitor;
    tree.root().visit(visitor);
    return std::move(visitor.modules);
}

} // namespace custom_hdl
