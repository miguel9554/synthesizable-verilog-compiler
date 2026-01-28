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

// Module definition
struct IRModule : IRNode {
    std::string name;
    std::vector<SignalInfo> parameters;
    std::vector<SignalInfo> inputs;
    std::vector<SignalInfo> outputs;
    std::vector<SignalInfo> signals;
    std::vector<SignalInfo> flops;

    // Clocks: all the clocks.
    // Reset: all the resets.
    // TODO for the moment we'll store async triggers
    // TODO 2nd pass we parse the behavioral block and extract
    std::vector<SignalInfo> async_triggers;

    // Procedural timing blocks
    // Can be combo @(*)
    // or seq @(posedge/negedge c)
    // Functions from (inputs, flops outputs) -> outputs
    // Functions from (inputs, flops) -> flops inputs
    std::vector<const slang::syntax::TimingControlStatementSyntax*> proceduralTimingBlocks;

    // Procedural combo blocks from always_comb
    // These have no timing.
    std::vector<const slang::syntax::StatementSyntax*> proceduralComboBlocks;

    // TODO a list of instantiated modules.

    void print(int indent = 0) const override;
};

} // namespace custom_hdl
