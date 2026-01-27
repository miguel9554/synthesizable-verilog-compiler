#include "ir_builder.h"
#include <iostream>
#include <sstream>

#include "slang/syntax/SyntaxKind.h"
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

    if (header.parameters) {
        for (auto* declaration : header.parameters->declarations) {
            std::cout << declaration->toString() << std::endl;
            if (declaration->kind == SyntaxKind::TypeParameterDeclaration){
                throw std::runtime_error("Can't parse Type parameters");
            }
            auto& paramDeclaration = declaration->as<ParameterDeclarationSyntax>();
            TypeInfo typeInfo = extractDataType(paramDeclaration.type);
        }
    }

    // Extract ports
    if (header.ports) {
        if (header.ports->kind != SyntaxKind::AnsiPortList) {
            throw std::runtime_error("Only ANSI port lists supported");
        }

        auto& ansiPorts = header.ports->as<AnsiPortListSyntax>();
        for (auto* member : ansiPorts.ports) {
            if (member->kind != SyntaxKind::ImplicitAnsiPort) {
                throw std::runtime_error("Only implicit ANSI ports supported");
            }

            auto& port = member->as<ImplicitAnsiPortSyntax>();
            // TODO port.declarator->dimensions has the *unpacked* dimensions! need to parse.
            std::string portName = std::string(port.declarator->name.valueText());

            // Get direction and dataType from header
            if (port.header->kind == SyntaxKind::InterfacePortHeader) {
                throw std::runtime_error("Can't parse interface ports.");
            }
            else if (port.header->kind == SyntaxKind::VariablePortHeader) {
                auto& varHeader = port.header->as<VariablePortHeaderSyntax>();
                auto dir = varHeader.direction.kind;
                TypeInfo typeInfo = extractDataType(*varHeader.dataType);

                SignalInfo portInfo{portName, typeInfo};
                if (dir == TokenKind::InputKeyword) {
                    info.inputs.push_back(portInfo);
                } else if (dir == TokenKind::OutputKeyword) {
                    info.outputs.push_back(portInfo);
                } else {
                    throw std::runtime_error("Unsupported port direction");
                }
            }
            else if (port.header->kind == SyntaxKind::NetPortHeader) {
                auto& netHeader = port.header->as<NetPortHeaderSyntax>();
                auto dir = netHeader.direction.kind;
                TypeInfo typeInfo = extractDataType(*netHeader.dataType);

                SignalInfo portInfo{portName, typeInfo};
                if (dir == TokenKind::InputKeyword) {
                    info.inputs.push_back(portInfo);
                } else if (dir == TokenKind::OutputKeyword) {
                    info.outputs.push_back(portInfo);
                } else {
                    throw std::runtime_error("Unsupported port direction");
                }
            }
        }
    }

    return info;
}

std::vector<DimensionRange> extractDimensions(
    const SyntaxList<VariableDimensionSyntax>& dimensions) {
    std::vector<DimensionRange> result;

    for (auto* dim : dimensions) {
        if (!dim->specifier) {
            throw std::runtime_error("Unsupported dimension: missing specifier");
        }

        if (dim->specifier->kind != SyntaxKind::RangeDimensionSpecifier) {
            throw std::runtime_error("Unsupported dimension specifier kind");
        }

        auto& rangeSpec = dim->specifier->as<RangeDimensionSpecifierSyntax>();
        SelectorSyntax* selector = rangeSpec.selector;

        if (!RangeSelectSyntax::isKind(selector->kind)) {
            throw std::runtime_error("Unsupported selector kind in dimension");
        }

        auto& rangeSelect = selector->as<RangeSelectSyntax>();

        // Extract left bound
        if (rangeSelect.left->kind != SyntaxKind::IntegerLiteralExpression) {
            throw std::runtime_error("Dimension left bound must be integer literal");
        }
        auto& leftLit = rangeSelect.left->as<LiteralExpressionSyntax>();
        auto leftVal = leftLit.literal.intValue().template as<int>();
        if (!leftVal) {
            throw std::runtime_error("Dimension left bound out of range");
        }

        // Extract right bound
        if (rangeSelect.right->kind != SyntaxKind::IntegerLiteralExpression) {
            throw std::runtime_error("Dimension right bound must be integer literal");
        }
        auto& rightLit = rangeSelect.right->as<LiteralExpressionSyntax>();
        auto rightVal = rightLit.literal.intValue().template as<int>();
        if (!rightVal) {
            throw std::runtime_error("Dimension right bound out of range");
        }

        result.emplace_back(*leftVal, *rightVal);
    }

    return result;
}

TypeInfo extractDataType(const DataTypeSyntax& syntax) {
    TypeInfo info;

    // Handle IntegerTypeSyntax (logic, reg, bit, int, etc.)
    if (IntegerTypeSyntax::isKind(syntax.kind)) {
        auto& intType = syntax.as<IntegerTypeSyntax>();

        // Extract type name from keyword
        info.name = "Integer";

        // Extract signing
        bool isSigned = false;
        if (intType.signing.kind == TokenKind::SignedKeyword) {
            isSigned = true;
        } else if (intType.signing.kind == TokenKind::UnsignedKeyword) {
            isSigned = false;
        }

        // Extract dimensions and compute width
        auto dims = extractDimensions(intType.dimensions);
        info.width = 1;
        for (const auto& dim : dims) {
            info.width *= std::abs(dim.first - dim.second) + 1;
        }

        info.metadata = IntegerInfo{.is_signed = isSigned};
        return info;
    }

    // Handle ImplicitTypeSyntax (default/inferred type, e.g., "input clk")
    if (syntax.kind == SyntaxKind::ImplicitType) {
        auto& implType = syntax.as<ImplicitTypeSyntax>();

        // TODO this name is just a string, should be an enum
        info.name = "Integer";  // No explicit type keyword

        bool isSigned = false;
        if (implType.signing.kind == TokenKind::SignedKeyword) {
            isSigned = true;
        }

        auto dims = extractDimensions(implType.dimensions);
        info.width = 1;
        for (const auto& dim : dims) {
            info.width *= std::abs(dim.first - dim.second) + 1;
        }

        info.metadata = IntegerInfo{.is_signed = isSigned};
        return info;
    }

    // Unsupported DataTypeSyntax kind
    throw std::runtime_error("Unsupported DataTypeSyntax kind: " +
                             std::string(toString(syntax.kind)));
}

void TypeInfo::print(std::ostream& os) const {
    if (name.empty()) {
        os << "[" << width << "]";
    } else {
        os << name << "[" << width << "]";
    }
    if (std::holds_alternative<IntegerInfo>(metadata)) {
        auto& intInfo = std::get<IntegerInfo>(metadata);
        os << (intInfo.is_signed ? " signed" : " unsigned");
    }
}

void SignalInfo::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
}

void ModuleHeaderInfo::print(std::ostream& os) const {
    os << "Module: " << name << "\n";
    if (!inputs.empty()) {
        os << "  Inputs:\n";
        for (const auto& port : inputs) {
            os << "    ";
            port.print(os);
            os << "\n";
        }
    }
    if (!outputs.empty()) {
        os << "  Outputs:\n";
        for (const auto& port : outputs) {
            os << "    ";
            port.print(os);
            os << "\n";
        }
    }
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

    void handle() {
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
                throw std::runtime_error("Latch not allowed.");
            case SyntaxKind::InitialBlock:
                throw std::runtime_error("Initial block not synthesizable");
            default:
                throw std::runtime_error("Unknown syntax");
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
        std::cout << indent_str(indent + 1) << "Inputs:" << std::endl;
        for (const auto& in : inputs) {
            std::cout << indent_str(indent + 2);
            in.print(std::cout);
            std::cout << std::endl;
        }
    }

    if (!outputs.empty()) {
        std::cout << indent_str(indent + 1) << "Outputs:" << std::endl;
        for (const auto& out : outputs) {
            std::cout << indent_str(indent + 2);
            out.print(std::cout);
            std::cout << std::endl;
        }
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
