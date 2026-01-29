#pragma once

#include "slang/syntax/AllSyntax.h"
#include "types.h"
#include <vector>

namespace custom_hdl {

// Base class for all IR nodes
struct IRNode {
    virtual ~IRNode() = default;
    virtual void print(int indent = 0) const = 0;
};

// Module definition - extends ModuleBase with IR-specific members
struct IRModule : ModuleBase<UnresolvedTypes>, IRNode {
    // Clocks: all the clocks.
    // Reset: all the resets.
    // TODO for the moment we'll store async triggers
    // TODO 2nd pass we parse the behavioral block and extract
    std::vector<UnresolvedSignal> async_triggers;

    // Procedural timing blocks
    // Can be combo @(*)
    // or seq @(posedge/negedge c)
    // Functions from (inputs, flops outputs) -> outputs
    // Functions from (inputs, flops) -> flops inputs
    std::vector<const slang::syntax::TimingControlStatementSyntax*> proceduralTimingBlocks;

    // Procedural combo blocks from always_comb
    // These have no timing.
    std::vector<const slang::syntax::StatementSyntax*> proceduralComboBlocks;

    // Assignments
    std::vector<const slang::syntax::ContinuousAssignSyntax*> assignStatements;

    // TODO a list of instantiated modules.
    // TODO prob. should be a list of pairs of params and modules
    std::vector<const slang::syntax::HierarchyInstantiationSyntax*> hierarchyInstantiation;

    void print(int indent = 0) const override;
};

} // namespace custom_hdl
