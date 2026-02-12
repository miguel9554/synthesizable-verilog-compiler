#pragma once

#include "ir/unresolved.h"
#include <memory>
#include <vector>

// Forward declaration
namespace slang::syntax {
class SyntaxTree;
}

namespace custom_hdl {

// Build IR from a slang syntax tree
std::vector<std::unique_ptr<UnresolvedModule>> buildIR(const slang::syntax::SyntaxTree& tree);

} // namespace custom_hdl
