#pragma once

#include "slang/syntax/SyntaxNode.h"
#include <iosfwd>
#include <iostream>
#include <string>
#include <vector>

// Forward declarations for slang syntax types
namespace slang::syntax {
struct DataTypeSyntax;
struct VariableDimensionSyntax;
struct ExpressionSyntax;
struct TimingControlStatementSyntax;
struct StatementSyntax;
struct ContinuousAssignSyntax;
struct HierarchyInstantiationSyntax;
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

// Parameter extends signal with a default value expression
struct UnresolvedParam {
    std::string name;
    UnresolvedType type;
    UnresolvedDimension dimensions;
    const slang::syntax::ExpressionSyntax* defaultValue = nullptr;

    void print(std::ostream& os) const;
};

// ============================================================================
// Type traits for unresolved types
// ============================================================================

struct UnresolvedTypes {
    using Type = UnresolvedType;
    using Dimension = UnresolvedDimension;
    using Signal = UnresolvedSignal;
    using Param = UnresolvedParam;
    using ProceduralTiming = const slang::syntax::TimingControlStatementSyntax*;
    using ProceduralCombo = const slang::syntax::StatementSyntax*;
    using Assign = const slang::syntax::ContinuousAssignSyntax*;
    using Hierarchy = const slang::syntax::HierarchyInstantiationSyntax*;
};

// ============================================================================
// Template module structure - parameterized by type traits
// ============================================================================

template<typename Types>
struct ModuleBase {
    std::string name;
    std::vector<typename Types::Param> parameters;
    std::vector<typename Types::Signal> inputs;
    std::vector<typename Types::Signal> outputs;
    std::vector<typename Types::Signal> signals;
    std::vector<typename Types::Signal> flops;

    // Procedural timing blocks
    // Can be combo @(*)
    // or seq @(posedge/negedge c)
    // Functions from (inputs, flops outputs) -> outputs
    // Functions from (inputs, flops) -> flops inputs
    std::vector<typename Types::ProceduralTiming> proceduralTimingBlocks;

    // Procedural combo blocks from always_comb
    // These have no timing.
    std::vector<typename Types::ProceduralCombo> proceduralComboBlocks;

    // Assignments
    std::vector<typename Types::Assign> assignStatements;

    // TODO a list of instantiated modules.
    // TODO prob. should be a list of pairs of params and modules
    std::vector<typename Types::Hierarchy> hierarchyInstantiation;

    void print(int indent = 0) const {
        auto indent_str = [](int n) { return std::string(n * 2, ' '); };

        std::cout << indent_str(indent) << "Module: " << this->name << std::endl;

        std::cout << indent_str(indent + 1) << "Parameters:" << std::endl;
        for (const auto& param : this->parameters) {
            std::cout << indent_str(indent + 2);
            param.print(std::cout);
            std::cout << std::endl;
        }

        std::cout << indent_str(indent + 1) << "Inputs:" << std::endl;
        for (const auto& in : this->inputs) {
            std::cout << indent_str(indent + 2);
            in.print(std::cout);
            std::cout << std::endl;
        }

        std::cout << indent_str(indent + 1) << "Outputs:" << std::endl;
        for (const auto& out : this->outputs) {
            std::cout << indent_str(indent + 2);
            out.print(std::cout);
            std::cout << std::endl;
        }

        std::cout << indent_str(indent + 1) << "Signals:" << std::endl;
        for (const auto& signal : this->signals) {
            std::cout << indent_str(indent + 2);
            signal.print(std::cout);
            std::cout << std::endl;
        }

        std::cout << indent_str(indent + 1) << "Flops:" << std::endl;
        for (const auto& flop : this->flops) {
            std::cout << indent_str(indent + 2);
            flop.print(std::cout);
            std::cout << std::endl;
        }

        std::cout << indent_str(indent + 1) << "No of procedural timing: " << this->proceduralTimingBlocks.size() << std::endl;
        std::cout << indent_str(indent + 1) << "No of procedural combo: " << this->proceduralComboBlocks.size() << std::endl;
        std::cout << indent_str(indent + 1) << "No of cont. assign.: " << this->assignStatements.size() << std::endl;
        std::cout << indent_str(indent + 1) << "No of hier. inst.: " << this->hierarchyInstantiation.size() << std::endl;
    }
};

// Convenience alias for unresolved module
using UnresolvedModule = ModuleBase<UnresolvedTypes>;

} // namespace custom_hdl
