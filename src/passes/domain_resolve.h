#pragma once

#include "ir/resolved.h"

namespace custom_hdl {

// Resolve clock domains for all signals, inputs, and outputs.
// Sets clock_domain pointer on every ResolvedSignal that is not
// a Clock or Reset type. Currently only single-clock-domain modules
// are supported; throws if zero or more than one clock is found.
// Must run after flop_resolve (which tags Clock/Reset inputs).
void resolveDomains(ResolvedModule& module);

} // namespace custom_hdl
