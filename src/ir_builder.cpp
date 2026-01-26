#include "ir_builder.h"
#include <iostream>
#include <sstream>

#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/SyntaxVisitor.h"
#include "slang/syntax/AllSyntax.h"

using namespace slang;
using namespace slang::syntax;
using namespace slang::parsing;
using namespace custom_hdl;

namespace custom_hdl {

ModuleHeaderInfo extractModuleHeader(const ModuleHeaderSyntax& header) {
    ModuleHeaderInfo info;
    info.name = std::string(header.name.valueText());

    if (header.lifetime) throw std::runtime_error("Can't parse lifetime");
    if (!header.imports.empty()) throw std::runtime_error("Can't parse imports");
    if (header.parameters) throw std::runtime_error("Can't parse parameters");

    // Extract ports
    if (header.ports) {
        if (header.ports->kind != SyntaxKind::AnsiPortList) {
            throw std::runtime_error("Only ANSI port lists supported");
        }

        auto& ansiPorts = header.ports->as<AnsiPortListSyntax>();
        for (auto* member : ansiPorts.ports) {
            // Explicit ports have different ID and expression?
            if (member->kind != SyntaxKind::ImplicitAnsiPort) {
                throw std::runtime_error("Only implicit ANSI ports supported");
            }

            auto& port = member->as<ImplicitAnsiPortSyntax>();
            // TODO port.declarator->dimensions has the *unpacked* dimensions! need to parse.
            std::string portName = std::string(port.declarator->name.valueText());

            // Get direction from header
            // 3 kinds: InterfacePortHeaderSyntax, VariablePortHeaderSyntax, NetPortHeaderSyntax
            // TODO still need to extract DataTypeSyntax!
            if (port.header->kind == SyntaxKind::InterfacePortHeader) {
                throw std::runtime_error("Can't parse interface ports.");
            }
            // Parse net and variable together
            else if (port.header->kind == SyntaxKind::VariablePortHeader) {
                // TODO need to parse CONST keyword, probably has no effect.
                auto& varHeader = port.header->as<VariablePortHeaderSyntax>();
                auto dir = varHeader.direction.kind;

                if (dir == TokenKind::InputKeyword) {
                    info.inputs.push_back(portName);
                } else if (dir == TokenKind::OutputKeyword) {
                    info.outputs.push_back(portName);
                } else {
                    throw std::runtime_error("Unsupported port direction");
                }
            }
            else if (port.header->kind == SyntaxKind::NetPortHeader) {
                // TODO we do not care about netType but should we raise error for any?
                auto& varHeader = port.header->as<NetPortHeaderSyntax>();
                auto dir = varHeader.direction.kind;

                if (dir == TokenKind::InputKeyword) {
                    info.inputs.push_back(portName);
                } else if (dir == TokenKind::OutputKeyword) {
                    info.outputs.push_back(portName);
                } else {
                    throw std::runtime_error("Unsupported port direction");
                }
            }
        }
    }

    return info;
}

} // namespace custom_hdl

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
        auto headerInfo = extractModuleHeader(*node.header);

        auto module = std::make_unique<IRModule>();
        module->name = headerInfo.name;
        module->inputs = std::move(headerInfo.inputs);
        module->outputs = std::move(headerInfo.outputs);

        if (node.blockName) throw std::runtime_error("Can't parse blockName");

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
        throw std::runtime_error("Should not visit PortDeclarationSyntax");
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
