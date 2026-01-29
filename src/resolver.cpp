#include "resolver.h"
#include "slang/syntax/SyntaxKind.h"

#include <iostream>

using namespace slang::syntax;

namespace {

std::string indent_str(int indent) {
    return std::string(indent * 2, ' ');
}

} // anonymous namespace

namespace custom_hdl {

// ============================================================================
// ResolvedType implementation
// ============================================================================

ResolvedType ResolvedType::makeInteger(int width, bool is_signed) {
    return ResolvedType{
        .kind = ResolvedTypeKind::Integer,
        .width = width,
        .metadata = ResolvedIntegerInfo{.is_signed = is_signed}
    };
}

void ResolvedType::print(std::ostream& os) const {
    switch (kind) {
        case ResolvedTypeKind::Integer:
            os << "Integer";
            break;
    }
    os << "[" << width << "]";
    if (std::holds_alternative<ResolvedIntegerInfo>(metadata)) {
        auto& intInfo = std::get<ResolvedIntegerInfo>(metadata);
        os << (intInfo.is_signed ? " signed" : " unsigned");
    }
}

void ResolvedSignal::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
    for (const auto& dim : dimensions) {
        os << "[" << dim.left << ":" << dim.right << "]";
    }
}

void ResolvedParam::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
    for (const auto& dim : dimensions) {
        os << "[" << dim.left << ":" << dim.right << "]";
    }
    os << " = " << value;
}

void printResolvedModule(const ResolvedModule& module, int indent) {
    std::cout << indent_str(indent) << "ResolvedModule: " << module.name << std::endl;

    if (!module.parameters.empty()) {
        std::cout << indent_str(indent + 1) << "Parameters:" << std::endl;
        for (const auto& param : module.parameters) {
            std::cout << indent_str(indent + 2);
            param.print(std::cout);
            std::cout << std::endl;
        }
    }

    if (!module.inputs.empty()) {
        std::cout << indent_str(indent + 1) << "Inputs:" << std::endl;
        for (const auto& in : module.inputs) {
            std::cout << indent_str(indent + 2);
            in.print(std::cout);
            std::cout << std::endl;
        }
    }

    if (!module.outputs.empty()) {
        std::cout << indent_str(indent + 1) << "Outputs:" << std::endl;
        for (const auto& out : module.outputs) {
            std::cout << indent_str(indent + 2);
            out.print(std::cout);
            std::cout << std::endl;
        }
    }
}

// ============================================================================
// Resolution functions (STUB implementations)
// ============================================================================

namespace {

// IntegerType
// KeywordType
// NamedType
// StructUnionType
// EnumType
// TypeReference
// VirtualInterfaceType
// ImplicitType

// Resolve an UnresolvedParam to ResolvedParam
// TODO: Actually evaluate the type syntax and dimension expressions
ResolvedParam resolveParameter(const UnresolvedParam& param, const ParameterContext& /*ctx*/) {
    ResolvedParam resolved;
    resolved.name = param.name;

    // TODO: currently only support for implicit type
    if (param.type.syntax->isKind(SyntaxKind::ImplicitType)){
        resolved.type = ResolvedType::makeInteger(32, false);
    } else{
        throw std::runtime_error("Only implicit param type supported");
    }

    // TODO only support for scalar params
    if (param.dimensions.syntax) {
        throw std::runtime_error("Param with dimensions not supported.");
    }

    // TODO: evaluate the default value expression
    // For now, just set to 0
    resolved.value = 0;

    return resolved;
}

ResolvedSignal resolveSignal(const UnresolvedSignal& signal, const ParameterContext& /*ctx*/) {
    ResolvedSignal resolved;
    resolved.name = signal.name;

    // STUB: Return a placeholder type (1-bit unsigned)
    // TODO: Evaluate signal.type.syntax using ctx
    if (signal.type.syntax->isKind(SyntaxKind::ImplicitType)){
        resolved.type = ResolvedType::makeInteger(32, false);
    }


    // STUB: Return empty dimensions
    // TODO: Evaluate signal.dimensions expressions using ctx
    // for (size_t i = 0; i < signal.dimensions.size(); ++i) {
        // resolved.dimensions.push_back(ResolvedDimension{0, 0});
    // }

    return resolved;
}

} // anonymous namespace

ResolvedModule resolveModule(const IRModule& unresolved, const ParameterContext& topCtx) {
    ResolvedModule resolved;
    resolved.name = unresolved.name;

    // Resolve parameters
    for (const auto& param : unresolved.parameters) {
        resolved.parameters.push_back(resolveParameter(param, topCtx));
    }

    /*
    // Resolve inputs
    for (const auto& input : unresolved.inputs) {
        resolved.inputs.push_back(resolveSignal(input, topCtx));
    }

    // Resolve outputs
    for (const auto& output : unresolved.outputs) {
        resolved.outputs.push_back(resolveSignal(output, topCtx));
    }

    // Resolve signals
    for (const auto& signal : unresolved.signals) {
        resolved.signals.push_back(resolveSignal(signal, topCtx));
    }

    // Resolve flops
    for (const auto& flop : unresolved.flops) {
        resolved.flops.push_back(resolveSignal(flop, topCtx));
    }
    */

    return resolved;
}

std::vector<ResolvedModule> resolveModules(
    const std::vector<std::unique_ptr<IRModule>>& modules) {

    std::vector<ResolvedModule> resolved;
    ParameterContext emptyCtx;  // Use default/empty context for now

    for (const auto& module : modules) {
        resolved.push_back(resolveModule(*module, emptyCtx));
    }

    return resolved;
}

} // namespace custom_hdl
