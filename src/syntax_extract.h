#pragma once

#include "types.h"

// Forward declarations for slang types
namespace slang::syntax {
class ModuleHeaderSyntax;
class DataTypeSyntax;
template<typename T> class SyntaxList;
class VariableDimensionSyntax;
}

namespace custom_hdl {

// Extract type information from a DataTypeSyntax node
TypeInfo extractDataType(const slang::syntax::DataTypeSyntax& syntax);

// Extract dimension ranges from a list of VariableDimensionSyntax nodes
std::vector<DimensionRange> extractDimensions(
    const slang::syntax::SyntaxList<slang::syntax::VariableDimensionSyntax>& dimensions);

// Extract module header information (name, ports, parameters)
ModuleHeaderInfo extractModuleHeader(const slang::syntax::ModuleHeaderSyntax& header);

} // namespace custom_hdl
