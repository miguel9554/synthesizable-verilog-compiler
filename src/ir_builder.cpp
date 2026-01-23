#include "ir_builder.h"
#include <iostream>
#include <sstream>

#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/SyntaxVisitor.h"
#include "slang/syntax/AllSyntax.h"

using namespace slang;
using namespace slang::syntax;
using namespace custom_hdl;

namespace {

// Helper function to create indentation
std::string indent_str(int indent) {
    return std::string(indent * 2, ' ');
}

// Visitor class that builds our custom IR
class IRBuilderVisitor : public SyntaxVisitor<IRBuilderVisitor> {
public:
    std::vector<std::unique_ptr<IRModule>> modules;
    IRModule* currentModule = nullptr;
    
    void handle(const ModuleDeclarationSyntax& node) {
        auto module = std::make_unique<IRModule>();
        module->name = std::string(node.header->name.valueText());
        
        std::cout << "Processing module: " << module->name << std::endl;
        
        // Set current module context
        currentModule = module.get();
        
        // Visit module members
        visitDefault(node);
        
        // Store the completed module
        modules.push_back(std::move(module));
        currentModule = nullptr;
    }
    
    void handle(const PortDeclarationSyntax& node) {
        if (!currentModule) return;

        // Determine direction from header
        std::string direction;
        if (node.header) {
            direction = std::string(node.header->toString());
        }

        // Extract port names from declarators
        for (auto decl : node.declarators) {
            std::string portName = std::string(decl->name.valueText());

            if (direction.find("input") != std::string::npos) {
                currentModule->inputs.push_back(portName);
                std::cout << "  Found input port: " << portName << std::endl;
            } else if (direction.find("output") != std::string::npos) {
                currentModule->outputs.push_back(portName);
                std::cout << "  Found output port: " << portName << std::endl;
            }
        }

        visitDefault(node);
    }
    
    void handle(const DataDeclarationSyntax& node) {
        if (!currentModule) return;
        
        auto var = std::make_unique<IRVariable>();
        
        // Extract type (simplified)
        if (node.type) {
            var->type = std::string(node.type->toString());
        }
        
        // Extract variable name
        for (auto decl : node.declarators) {
            var->name = std::string(decl->name.valueText());
            var->width = 1; // Simplified, would need proper width extraction
            
            std::cout << "  Found variable: " << var->name 
                      << " of type " << var->type << std::endl;
            
            currentModule->body.push_back(std::move(var));
            break; // Handle first declarator for now
        }
        
        visitDefault(node);
    }
    
    void handle(const ProceduralBlockSyntax& node) {
        if (!currentModule) return;

        auto always = std::make_unique<IRAlways>();

        // Extract sensitivity list based on block kind
        switch (node.kind) {
            case SyntaxKind::AlwaysBlock:
                always->sensitivity = "always";
                std::cout << "  Found always block" << std::endl;
                break;
            case SyntaxKind::AlwaysCombBlock:
                always->sensitivity = "always_comb";
                std::cout << "  Found always_comb block" << std::endl;
                break;
            case SyntaxKind::AlwaysFFBlock:
                always->sensitivity = "always_ff";
                std::cout << "  Found always_ff block" << std::endl;
                break;
            case SyntaxKind::AlwaysLatchBlock:
                always->sensitivity = "always_latch";
                std::cout << "  Found always_latch block" << std::endl;
                break;
            case SyntaxKind::InitialBlock:
                always->sensitivity = "initial";
                std::cout << "  Found initial block" << std::endl;
                break;
            default:
                always->sensitivity = "unknown";
                break;
        }

        currentModule->body.push_back(std::move(always));

        visitDefault(node);
    }
};

} // anonymous namespace

namespace custom_hdl {

// Implementation of print methods for debugging

void IRModule::print(int indent) const {
    std::cout << indent_str(indent) << "Module: " << name << std::endl;
    
    if (!inputs.empty()) {
        std::cout << indent_str(indent + 1) << "Inputs: ";
        for (const auto& in : inputs) std::cout << in << " ";
        std::cout << std::endl;
    }
    
    if (!outputs.empty()) {
        std::cout << indent_str(indent + 1) << "Outputs: ";
        for (const auto& out : outputs) std::cout << out << " ";
        std::cout << std::endl;
    }
    
    std::cout << indent_str(indent + 1) << "Body:" << std::endl;
    for (const auto& node : body) {
        node->print(indent + 2);
    }
}

void IRAlways::print(int indent) const {
    std::cout << indent_str(indent) << "Always (" << sensitivity << ")" << std::endl;
    if (body) {
        body->print(indent + 1);
    }
}

void IRVariable::print(int indent) const {
    std::cout << indent_str(indent) << "Variable: " << name 
              << " [" << type << ", " << width << " bits]" << std::endl;
}

void IRAssignment::print(int indent) const {
    std::cout << indent_str(indent) << "Assignment: " << target 
              << (blocking ? " = " : " <= ") << value << std::endl;
}

void IRIf::print(int indent) const {
    std::cout << indent_str(indent) << "If (" << condition << ")" << std::endl;
    if (thenBranch) {
        std::cout << indent_str(indent + 1) << "Then:" << std::endl;
        thenBranch->print(indent + 2);
    }
    if (elseBranch) {
        std::cout << indent_str(indent + 1) << "Else:" << std::endl;
        elseBranch->print(indent + 2);
    }
}

void IRBlock::print(int indent) const {
    std::cout << indent_str(indent) << "Block {" << std::endl;
    for (const auto& stmt : statements) {
        stmt->print(indent + 1);
    }
    std::cout << indent_str(indent) << "}" << std::endl;
}

} // namespace custom_hdl

// Public API to build IR from a syntax tree
std::vector<std::unique_ptr<IRModule>> 
buildIR(const SyntaxTree& tree) {
    IRBuilderVisitor visitor;
    tree.root().visit(visitor);
    return std::move(visitor.modules);
}
