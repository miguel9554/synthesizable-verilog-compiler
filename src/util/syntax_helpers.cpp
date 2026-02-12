#include "util/syntax_helpers.h"

#include <stdexcept>
#include <vector>

#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxKind.h"
#include "ir/unresolved.h"

using namespace slang::syntax;
using namespace slang::parsing;

namespace custom_hdl {

UnresolvedType extractDataType(const DataTypeSyntax& syntax) {
    // Simply capture the syntax pointer - resolution happens in pass 2
    return UnresolvedType{&syntax};
}

std::vector<UnresolvedParam> extractParameter(const ParameterDeclarationBaseSyntax* declaration, std::vector<UnresolvedParam> params) {
    if (declaration->kind == SyntaxKind::TypeParameterDeclaration) {
        throw std::runtime_error("Can't parse Type parameters");
    }
    auto& paramDeclaration = declaration->as<ParameterDeclarationSyntax>();
    UnresolvedType typeInfo = extractDataType(*paramDeclaration.type);

    for (auto* declarator : paramDeclaration.declarators) {
        std::string paramName = std::string(declarator->name.valueText());
        const ExpressionSyntax* defaultValue = nullptr;
        if (declarator->initializer) {
            defaultValue = declarator->initializer->expr;
        }
        params.push_back(UnresolvedParam{
            .name = paramName,
            .type = typeInfo,
            .dimensions = {},
            .defaultValue = defaultValue
        });
    }
    return params;
}

UnresolvedModule extractModuleHeader(const ModuleHeaderSyntax& header) {
    UnresolvedModule info;
    info.name = std::string(header.name.valueText());

    if (header.lifetime) throw std::runtime_error("Can't parse lifetime");
    if (!header.imports.empty()) throw std::runtime_error("Can't parse imports");

    // Extract parameters
    if (header.parameters) {
        for (auto* declaration : header.parameters->declarations) {
            info.parameters = extractParameter(declaration, info.parameters);
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
                UnresolvedType typeInfo = extractDataType(*varHeader.dataType);

                UnresolvedSignal portInfo{
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
                UnresolvedType typeInfo = extractDataType(*netHeader.dataType);

                UnresolvedSignal portInfo{
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
