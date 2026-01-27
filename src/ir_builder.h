#pragma once

#include <iosfwd>
#include <memory>
#include <string>
#include <variant>
#include <vector>

// Forward declarations for slang types
namespace slang::syntax {
class ModuleHeaderSyntax;
class DataTypeSyntax;
template<typename T> class SyntaxList;
class VariableDimensionSyntax;
}

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

TypeInfo extractDataType(const slang::syntax::DataTypeSyntax& syntax);

// Dimension range: pair of (left, right) bounds, e.g., [7:0] -> {7, 0}
using DimensionRange = std::pair<int, int>;

std::vector<DimensionRange> extractDimensions(
    const slang::syntax::SyntaxList<slang::syntax::VariableDimensionSyntax>& dimensions);

// Port with name and type information
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
    // TODO: parameters, etc.

    void print(std::ostream& os) const;
};

ModuleHeaderInfo extractModuleHeader(const slang::syntax::ModuleHeaderSyntax& header);

// Base class for all IR nodes
struct IRNode {
    virtual ~IRNode() = default;

    // For debugging: print the IR tree
    virtual void print(int indent = 0) const = 0;
};

// Module definition
struct IRModule : IRNode {
    std::string name;
    std::vector<SignalInfo> inputs;
    std::vector<SignalInfo> outputs;
    std::vector<SignalInfo> signals;
    std::vector<SignalInfo> flops;
    std::vector<SignalInfo> parameters;
    std::vector<std::unique_ptr<IRNode>> body;

    void print(int indent = 0) const override;
};

// Always block (procedural logic)
struct IRAlways : IRNode {
    std::string sensitivity;  // e.g., "posedge clk"
    std::unique_ptr<IRNode> body;

    void print(int indent = 0) const override;
};

// Variable declaration
struct IRVariable : IRNode {
    std::string name;
    std::string type;  // e.g., "reg", "wire"
    int width;         // bit width

    void print(int indent = 0) const override;
};

// Assignment statement
struct IRAssignment : IRNode {
    std::string target;
    std::string value;  // Simplified for now
    bool blocking;      // true for =, false for <=

    void print(int indent = 0) const override;
};

// If statement
struct IRIf : IRNode {
    std::string condition;
    std::unique_ptr<IRNode> thenBranch;
    std::unique_ptr<IRNode> elseBranch;

    void print(int indent = 0) const override;
};

// Statement block
struct IRBlock : IRNode {
    std::vector<std::unique_ptr<IRNode>> statements;

    void print(int indent = 0) const override;
};

} // namespace custom_hdl
