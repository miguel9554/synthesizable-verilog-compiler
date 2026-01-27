#pragma once

#include <iosfwd>
#include <string>
#include <variant>
#include <vector>

namespace custom_hdl {

// Type metadata variants
struct IntegerInfo {
    bool is_signed = false;
};

// Variant holding type-specific metadata
// Add new structs (FixedPointInfo, StructInfo, etc.) as needed
using TypeMetadata = std::variant<
    std::monostate,    // unspecified/implicit type
    IntegerInfo
>;

// Extracted type information from DataTypeSyntax
struct TypeInfo {
    std::string name;
    int width = 0;
    TypeMetadata metadata;

    void print(std::ostream& os) const;
};

// Dimension range: pair of (left, right) bounds, e.g., [7:0] -> {7, 0}
using DimensionRange = std::pair<int, int>;

// Signal with name and type information
struct SignalInfo {
    std::string name;
    TypeInfo type;

    void print(std::ostream& os) const;
};

// Extracted info from a module header
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
