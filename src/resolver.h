#pragma once

#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "types.h"
#include "dfg.h"

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

// TODO should actually have a packed dimension? or we don't care?
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

    double value;

    void print(std::ostream& os) const;
};

// Parameter extends signal with a resolved value
struct ResolvedParam {
    std::string name;
    ResolvedType type;
    std::vector<ResolvedDimension> dimensions;
    int64_t value = 0;  // TODO: extend to variant for different types

    void print(std::ostream& os) const;
};

// ============================================================================
// Type traits for resolved types
// ============================================================================

struct ResolvedTypes {
    using Type = ResolvedType;
    using Dimension = ResolvedDimension;
    using Signal = ResolvedSignal;
    using Param = ResolvedParam;
    using Assign = std::unique_ptr<DFG>;
    // Same as in unresolved IR
    using ProceduralTiming = std::unique_ptr<DFG>;
    using ProceduralCombo = std::unique_ptr<DFG>;
    using Hierarchy = UnresolvedTypes::Hierarchy;
};

// ============================================================================
// Resolved IR module (output of pass 2)
// ============================================================================

using ResolvedModule = ModuleBase<ResolvedTypes>;

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
ResolvedModule resolveModule(const UnresolvedModule& module, const ParameterContext& ctx);

// Resolve all modules (using default parameter values)
std::vector<ResolvedModule> resolveModules(
    const std::vector<std::unique_ptr<UnresolvedModule>>& modules);

} // namespace custom_hdl
