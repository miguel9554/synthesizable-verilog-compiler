#pragma once

#include "ir/dfg.h"

namespace custom_hdl {

// Propagate type info (width, signedness) from leaf nodes through the DFG.
// Mutates the graph in-place. Returns true if any changes were made.
bool propagateTypes(DFG& graph);

} // namespace custom_hdl
