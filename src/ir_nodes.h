#pragma once

#include "types.h"

namespace custom_hdl {

// Base class for all IR nodes
struct IRNode {
    virtual ~IRNode() = default;
    virtual void print(int indent = 0) const = 0;
};

// Module definition - extends ModuleBase with IR-specific members
// TODO the extended ones should have their own unresolved-resolved pairs.
struct IRModule : ModuleBase<UnresolvedTypes>, IRNode {
    void print(int indent = 0) const override;
};

} // namespace custom_hdl
