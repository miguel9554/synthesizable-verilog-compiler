#pragma once

#include "slang/syntax/SyntaxNode.h"
#include <iosfwd>
#include <string>
#include <vector>

// Forward declarations for slang syntax types
namespace slang::syntax {
struct DataTypeSyntax;
struct VariableDimensionSyntax;
}

namespace custom_hdl {

// ============================================================================
// Unresolved types - store pointers to slang syntax nodes
// Resolution happens in pass 2 when parameter values are known
// ============================================================================

struct UnresolvedType {
    const slang::syntax::DataTypeSyntax* syntax = nullptr;

    void print(std::ostream& os, bool debug) const;
};

struct UnresolvedDimension {
    const slang::syntax::SyntaxList<slang::syntax::VariableDimensionSyntax>* syntax = nullptr;
};

struct UnresolvedSignal {
    std::string name;
    UnresolvedType type;
    UnresolvedDimension dimensions;  // array dimensions (if any)

    void print(std::ostream& os) const;
};

// ============================================================================
// Type traits for unresolved types
// ============================================================================

struct UnresolvedTypes {
    using Type = UnresolvedType;
    using Dimension = UnresolvedDimension;
    using Signal = UnresolvedSignal;
};

// ============================================================================
// Template module structure - parameterized by type traits
// ============================================================================

template<typename Types>
struct ModuleBase {
    std::string name;
    std::vector<typename Types::Signal> parameters;
    std::vector<typename Types::Signal> inputs;
    std::vector<typename Types::Signal> outputs;
    std::vector<typename Types::Signal> signals;
    std::vector<typename Types::Signal> flops;
};

// Convenience alias for unresolved module
using UnresolvedModule = ModuleBase<UnresolvedTypes>;

} // namespace custom_hdl
