#pragma once

#include "ir/dfg.h"

namespace custom_hdl {

// Remove dead (unreachable) nodes from a DFG.
// Roots: outputs, signals, and MODULE nodes.
// Mutates the graph in-place. Returns true if any nodes were removed.
bool eliminateDeadCode(DFG& graph);

} // namespace custom_hdl
