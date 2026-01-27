#pragma once

#include "ir_nodes.h"
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace custom_hdl {

// ============================================================================
// Resolved types (output of pass 2)
// ============================================================================

enum class ResolvedTypeKind {
    Integer,
    // Future: Real, Struct, Enum, etc.
};

struct ResolvedIntegerInfo {
    bool is_signed = false;
};

using ResolvedTypeMetadata = std::variant<
    std::monostate,
    ResolvedIntegerInfo
>;

struct ResolvedType {
    ResolvedTypeKind kind = ResolvedTypeKind::Integer;
    int width = 0;
    ResolvedTypeMetadata metadata;

    void print(std::ostream& os) const;

    static ResolvedType makeInteger(int width, bool is_signed);
};

struct ResolvedDimension {
    int left = 0;
    int right = 0;

    int size() const { return std::abs(left - right) + 1; }
};

struct ResolvedSignal {
    std::string name;
    ResolvedType type;
    std::vector<ResolvedDimension> dimensions;

    void print(std::ostream& os) const;
};

// ============================================================================
// Resolved IR module (output of pass 2)
// ============================================================================

struct ResolvedModule {
    std::string name;
    std::vector<ResolvedSignal> parameters;
    std::vector<ResolvedSignal> inputs;
    std::vector<ResolvedSignal> outputs;
    std::vector<ResolvedSignal> signals;
    std::vector<ResolvedSignal> flops;
    // Note: body is not resolved yet - that's a future extension

    void print(int indent = 0) const;
};

// ============================================================================
// Parameter context for resolution
// ============================================================================

struct ParameterContext {
    // Map from parameter name to its value
    std::map<std::string, int> values;
};

// ============================================================================
// Resolution functions (pass 2)
// ============================================================================

// Resolve a single module given parameter values
// Returns nullptr if resolution fails
ResolvedModule resolveModule(const IRModule& module, const ParameterContext& ctx);

// Resolve all modules (using default parameter values)
std::vector<ResolvedModule> resolveModules(
    const std::vector<std::unique_ptr<IRModule>>& modules);

} // namespace custom_hdl
