#include "passes/type_propagation.h"

#include <algorithm>
#include <unordered_set>
#include <vector>

namespace custom_hdl {

// ---------------------------------------------------------------------------
// Post-order traversal (same pattern as constant_fold)
// ---------------------------------------------------------------------------

static void postOrderVisit(DFGNode* node,
                           std::unordered_set<DFGNode*>& visited,
                           std::vector<DFGNode*>& order) {
    if (!node || visited.count(node)) return;
    visited.insert(node);
    for (auto& input : node->in) {
        postOrderVisit(input.node, visited, order);
    }
    order.push_back(node);
}

static std::vector<DFGNode*> buildPostOrder(DFG& graph) {
    std::unordered_set<DFGNode*> visited;
    std::vector<DFGNode*> order;
    for (auto& [name, node] : graph.outputs) {
        postOrderVisit(node, visited, order);
    }
    for (auto& [name, node] : graph.signals) {
        postOrderVisit(node, visited, order);
    }
    for (auto& node : graph.nodes) {
        postOrderVisit(node.get(), visited, order);
    }
    return order;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

static ResolvedType makeOneBitUnsigned() {
    return ResolvedType::makeInteger(1, false);
}

// Return the "wider" type: max width, signed if both signed
static ResolvedType widenTypes(const ResolvedType& a, const ResolvedType& b) {
    int width = std::max(a.width, b.width);
    bool is_signed = a.isSigned() && b.isSigned();
    return ResolvedType::makeInteger(width, is_signed);
}

// ---------------------------------------------------------------------------
// Per-node type inference
// ---------------------------------------------------------------------------

static bool tryInferType(DFGNode* node) {
    if (node->hasType()) return false;

    switch (node->op) {
        // Leaf nodes — type should already be set during construction
        case DFGOp::INPUT:
        case DFGOp::CONST:
        case DFGOp::MODULE:
            return false;

        // Arithmetic binary ops: result = max(operand widths), signed if both signed
        case DFGOp::ADD:
        case DFGOp::SUB:
        case DFGOp::MUL:
        case DFGOp::DIV:
        case DFGOp::POWER: {
            if (node->in.size() < 2) return false;
            auto* lhs = node->in[0].node;
            auto* rhs = node->in[1].node;
            if (!lhs->hasType() || !rhs->hasType()) return false;
            node->type = widenTypes(*lhs->type, *rhs->type);
            return true;
        }

        // Comparison ops: result is 1-bit unsigned
        case DFGOp::EQ:
        case DFGOp::LT:
        case DFGOp::LE:
        case DFGOp::GT:
        case DFGOp::GE: {
            if (node->in.size() < 2) return false;
            if (!node->in[0].node->hasType() || !node->in[1].node->hasType()) return false;
            node->type = makeOneBitUnsigned();
            return true;
        }

        // Shift ops: result = left operand type
        case DFGOp::SHL:
        case DFGOp::ASR: {
            if (node->in.size() < 2) return false;
            auto* lhs = node->in[0].node;
            if (!lhs->hasType()) return false;
            node->type = *lhs->type;
            return true;
        }

        // MUX: result = type of data inputs (widened)
        case DFGOp::MUX: {
            if (node->in.size() < 3) return false;
            auto* tval = node->in[1].node;
            auto* fval = node->in[2].node;
            if (!tval->hasType() || !fval->hasType()) return false;
            node->type = widenTypes(*tval->type, *fval->type);
            return true;
        }

        // MUX_N: result = widened type of all data inputs
        case DFGOp::MUX_N: {
            size_t n = node->in.size() / 2;
            if (n == 0) return false;
            // Data values are in[n..2n-1]
            for (size_t i = n; i < node->in.size(); ++i) {
                if (!node->in[i].node->hasType()) return false;
            }
            ResolvedType result = *node->in[n].node->type;
            for (size_t i = n + 1; i < node->in.size(); ++i) {
                result = widenTypes(result, *node->in[i].node->type);
            }
            node->type = result;
            return true;
        }

        // Unary arithmetic: same as operand
        case DFGOp::UNARY_PLUS:
        case DFGOp::UNARY_NEGATE:
        case DFGOp::BITWISE_NOT: {
            if (node->in.empty()) return false;
            auto* operand = node->in[0].node;
            if (!operand->hasType()) return false;
            node->type = *operand->type;
            return true;
        }

        // Logical NOT: 1-bit unsigned
        case DFGOp::LOGICAL_NOT: {
            if (node->in.empty()) return false;
            if (!node->in[0].node->hasType()) return false;
            node->type = makeOneBitUnsigned();
            return true;
        }

        // Reduction ops: 1-bit unsigned
        case DFGOp::REDUCTION_AND:
        case DFGOp::REDUCTION_NAND:
        case DFGOp::REDUCTION_OR:
        case DFGOp::REDUCTION_NOR:
        case DFGOp::REDUCTION_XOR:
        case DFGOp::REDUCTION_XNOR: {
            if (node->in.empty()) return false;
            if (!node->in[0].node->hasType()) return false;
            node->type = makeOneBitUnsigned();
            return true;
        }

        // OUTPUT/SIGNAL: inherit from driver if present
        case DFGOp::OUTPUT:
        case DFGOp::SIGNAL: {
            if (node->in.empty()) return false;
            auto* driver = node->in[0].node;
            if (!driver->hasType()) return false;
            node->type = *driver->type;
            return true;
        }

        // INDEX: skip for now (future refinement)
        case DFGOp::INDEX:
            return false;
    }

    return false;
}

// ---------------------------------------------------------------------------
// Main pass
// ---------------------------------------------------------------------------

bool propagateTypes(DFG& graph) {
    bool anyChanged = false;
    bool changed;
    do {
        changed = false;
        auto order = buildPostOrder(graph);
        for (DFGNode* node : order) {
            if (tryInferType(node)) {
                changed = true;
            }
        }
        anyChanged |= changed;
    } while (changed);
    return anyChanged;
}

} // namespace custom_hdl
