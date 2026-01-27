#pragma once

#include <iosfwd>
#include <string>
#include <vector>

// Forward declarations for slang syntax types
namespace slang::syntax {
struct DataTypeSyntax;
struct ExpressionSyntax;
}

namespace custom_hdl {

// Unresolved type - stores pointer to slang syntax node
// Resolution happens in pass 2 when parameter values are known
struct TypeInfo {
    const slang::syntax::DataTypeSyntax* syntax = nullptr;

    void print(std::ostream& os) const;
};

// Unresolved dimension range - stores pointers to slang expression nodes
// e.g., [WIDTH-1:0] stores pointers to the "WIDTH-1" and "0" expressions
struct DimensionRange {
    const slang::syntax::ExpressionSyntax* left = nullptr;
    const slang::syntax::ExpressionSyntax* right = nullptr;
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
