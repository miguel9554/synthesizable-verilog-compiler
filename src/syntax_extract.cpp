#include "syntax_extract.h"

#include <cmath>
#include <iostream>
#include <stdexcept>

#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxKind.h"

using namespace slang::syntax;
using namespace slang::parsing;

namespace custom_hdl {

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

        info.name = "Integer";

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

ModuleHeaderInfo extractModuleHeader(const ModuleHeaderSyntax& header) {
    ModuleHeaderInfo info;
    info.name = std::string(header.name.valueText());

    if (header.lifetime) throw std::runtime_error("Can't parse lifetime");
    if (!header.imports.empty()) throw std::runtime_error("Can't parse imports");

    // Extract parameters
    if (header.parameters) {
        for (auto* declaration : header.parameters->declarations) {
            if (declaration->kind == SyntaxKind::TypeParameterDeclaration) {
                throw std::runtime_error("Can't parse Type parameters");
            }
            auto& paramDeclaration = declaration->as<ParameterDeclarationSyntax>();
            TypeInfo typeInfo = extractDataType(*paramDeclaration.type);

            for (auto* declarator : paramDeclaration.declarators) {
                std::string paramName = std::string(declarator->name.valueText());
                info.parameters.push_back(SignalInfo{paramName, typeInfo});
            }
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

} // namespace custom_hdl
