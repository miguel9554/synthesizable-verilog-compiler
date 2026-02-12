#pragma once

#include "ir/dfg.h"

namespace custom_hdl {

// Run constant folding and algebraic simplification on a DFG.
// Mutates the graph in-place. Returns true if any changes were made.
bool constantFold(DFG& graph);

} // namespace custom_hdl
