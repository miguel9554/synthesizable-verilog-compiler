#pragma once

#include "types.h"

// Forward declarations for slang types
namespace slang::syntax {
struct ModuleHeaderSyntax;
struct DataTypeSyntax;
struct VariableDimensionSyntax;
template<typename T> class SyntaxList;
}

namespace custom_hdl {

// Extract type information - just captures the syntax pointer (no resolution)
TypeInfo extractDataType(const slang::syntax::DataTypeSyntax& syntax);

// Extract dimension ranges - captures expression syntax pointers (no evaluation)
std::vector<DimensionRange> extractDimensions(
    const slang::syntax::SyntaxList<slang::syntax::VariableDimensionSyntax>& dimensions);

// Extract module header information (name, ports, parameters) - all unresolved
ModuleHeaderInfo extractModuleHeader(const slang::syntax::ModuleHeaderSyntax& header);

} // namespace custom_hdl
