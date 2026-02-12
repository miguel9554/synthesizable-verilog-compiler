#pragma once

#include "ir/unresolved.h"
#include "ir/resolved.h"

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace custom_hdl {

// ============================================================================
// Resolution functions (pass 2)
// ============================================================================

// Module lookup table: maps module name -> unresolved module
using ModuleLookup = std::unordered_map<std::string, const UnresolvedModule*>;

// Resolve a single module given parameter values and a lookup table for submodules
ResolvedModule resolveModule(const UnresolvedModule& module, const ParameterContext& ctx,
                             const ModuleLookup& moduleLookup);

// Resolve all modules (using default parameter values)
std::vector<ResolvedModule> resolveModules(
    const std::vector<std::unique_ptr<UnresolvedModule>>& modules);

} // namespace custom_hdl
