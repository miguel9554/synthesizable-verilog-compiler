#include "resolver.h"

#include <iostream>

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

void ResolvedModule::print(int indent) const {
    std::cout << indent_str(indent) << "ResolvedModule: " << name << std::endl;

    if (!parameters.empty()) {
        std::cout << indent_str(indent + 1) << "Parameters:" << std::endl;
        for (const auto& param : parameters) {
            std::cout << indent_str(indent + 2);
            param.print(std::cout);
            std::cout << std::endl;
        }
    }

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
}

// ============================================================================
// Resolution functions (STUB implementations)
// ============================================================================

namespace {

// STUB: Resolve a SignalInfo to ResolvedSignal
// TODO: Actually evaluate the type syntax and dimension expressions
ResolvedSignal resolveSignal(const SignalInfo& signal, const ParameterContext& /*ctx*/) {
    ResolvedSignal resolved;
    resolved.name = signal.name;

    // STUB: Return a placeholder type (1-bit unsigned)
    // TODO: Evaluate signal.type.syntax using ctx
    resolved.type = ResolvedType::makeInteger(1, false);

    // STUB: Return empty dimensions
    // TODO: Evaluate signal.dimensions expressions using ctx
    // for (size_t i = 0; i < signal.dimensions.size(); ++i) {
        // resolved.dimensions.push_back(ResolvedDimension{0, 0});
    // }

    return resolved;
}

} // anonymous namespace

ResolvedModule resolveModule(const IRModule& module, const ParameterContext& ctx) {
    ResolvedModule resolved;
    resolved.name = module.name;

    // Resolve parameters
    for (const auto& param : module.parameters) {
        resolved.parameters.push_back(resolveSignal(param, ctx));
    }

    // Resolve inputs
    for (const auto& input : module.inputs) {
        resolved.inputs.push_back(resolveSignal(input, ctx));
    }

    // Resolve outputs
    for (const auto& output : module.outputs) {
        resolved.outputs.push_back(resolveSignal(output, ctx));
    }

    // Resolve signals
    for (const auto& signal : module.signals) {
        resolved.signals.push_back(resolveSignal(signal, ctx));
    }

    // Resolve flops
    for (const auto& flop : module.flops) {
        resolved.flops.push_back(resolveSignal(flop, ctx));
    }

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
