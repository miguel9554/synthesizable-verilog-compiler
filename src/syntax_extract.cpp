#include "syntax_extract.h"

#include <iostream>
#include <stdexcept>

#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxKind.h"

using namespace slang::syntax;
using namespace slang::parsing;

namespace custom_hdl {

TypeInfo extractDataType(const DataTypeSyntax& syntax) {
    // Simply capture the syntax pointer - resolution happens in pass 2
    return TypeInfo{&syntax};
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

        // Capture expression pointers - no evaluation yet
        result.push_back(DimensionRange{
            .left = rangeSelect.left,
            .right = rangeSelect.right
        });
    }

    return result;
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
                info.parameters.push_back(SignalInfo{
                    .name = paramName,
                    .type = typeInfo,
                    .dimensions = {}
                });
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

                SignalInfo portInfo{
                    .name = portName,
                    .type = typeInfo,
                    .dimensions = {}
                };

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

                SignalInfo portInfo{
                    .name = portName,
                    .type = typeInfo,
                    .dimensions = {}
                };

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
