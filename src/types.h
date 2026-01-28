#pragma once

#include <iosfwd>
#include <string>
#include <vector>

// Forward declarations for slang syntax types
namespace slang::syntax {
struct DataTypeSyntax;
struct VariableDimensionSyntax;
}

namespace custom_hdl {

// Unresolved type - stores pointer to slang syntax node
// Resolution happens in pass 2 when parameter values are known
struct TypeInfo {
    const slang::syntax::DataTypeSyntax* syntax = nullptr;

    void print(std::ostream& os) const;
};

// Unresolved dimension - stores pointer to slang syntax node
// Resolution into specific dimension kind happens in pass 2
struct DimensionRange {
    const slang::syntax::VariableDimensionSyntax* syntax = nullptr;
};

// Signal with name and unresolved type information
struct SignalInfo {
    std::string name;
    TypeInfo type;
    std::vector<DimensionRange> dimensions;  // array dimensions (if any)

    void print(std::ostream& os) const;
};

// Extracted info from a module header (unresolved)
struct ModuleHeaderInfo {
    std::string name;
    std::vector<SignalInfo> parameters;
    std::vector<SignalInfo> inputs;
    std::vector<SignalInfo> outputs;
    std::vector<SignalInfo> flops;
    std::vector<SignalInfo> signals;

    void print(std::ostream& os) const;
};

} // namespace custom_hdl
