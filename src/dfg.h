#pragma once

#include <map>
#include <memory>
#include <format>
#include <vector>
#include <string>
#include <variant>
#include "expression_tree.h"

namespace custom_hdl {

enum class DFGOp {
    INPUT,      // Primary input signal
    OUTPUT,     // Primary output signal
    CONST,      // Constant value (data: int64_t)
    ADD,
    SUB,
    MUL,
    DIV,
    MUX,        // 2:1 mux: in[0]=sel, in[1]=true_val, in[2]=false_val
    ASSIGN,     // Assignment with expression tree (data: unique_ptr<ExprNode>)
};

struct DFGNode {
    DFGOp op;
    std::vector<DFGNode*> in;  // fan-in (operands)

    // Metadata depending on op type:
    // - monostate: no extra data (ADD, SUB, MUL, DIV, MUX)
    // - int64_t: constant value (CONST)
    // - string: signal name (INPUT, OUTPUT)
    // - unique_ptr<ExprNode>: expression tree (ASSIGN)
    std::variant<
        std::monostate,
        int64_t,
        std::string,
        std::unique_ptr<ExprNode>
    > data;

    DFGNode(DFGOp o) : op(o) {}
    DFGNode(DFGOp o, std::string name) : op(o), data(std::move(name)) {}
    DFGNode(DFGOp o, int64_t val) : op(o), data(val) {}
    DFGNode(DFGOp o, std::unique_ptr<ExprNode> expr) : op(o), data(std::move(expr)) {}
};

struct DFG {
    std::vector<std::unique_ptr<DFGNode>> nodes;

    // Quick access to primary I/O
    std::map<std::string, DFGNode*> inputs;
    std::map<std::string, DFGNode*> outputs;

    // INPUT nodes, they have no inputs.

    DFGNode* input(const std::string& name) {
        nodes.push_back(std::make_unique<DFGNode>(DFGOp::INPUT, name));
        auto result = inputs.insert({name, nodes.back().get()});
        if (!result.second) {
            // Key already exists - insertion failed
            throw std::runtime_error(std::format("Input {} already exists", name));
        }
        return nodes.back().get();
    }

    DFGNode* constant(int64_t v) {
        nodes.push_back(std::make_unique<DFGNode>(DFGOp::CONST, v));
        return nodes.back().get();
    }

    DFGNode* output(DFGNode* a, const std::string& name) {
        auto n = std::make_unique<DFGNode>(DFGOp::OUTPUT, name);
        n->in = {a};
        nodes.push_back(std::move(n));
        auto result = outputs.insert({name, nodes.back().get()});
        if (!result.second) {
            // Key already exists - insertion failed
            throw std::runtime_error(std::format("Output {} already exists", name));
        }
        return nodes.back().get();
    }

    DFGNode* add(DFGNode* a, DFGNode* b) {
        auto n = std::make_unique<DFGNode>(DFGOp::ADD);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        return nodes.back().get();
    }

    DFGNode* sub(DFGNode* a, DFGNode* b) {
        auto n = std::make_unique<DFGNode>(DFGOp::SUB);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        return nodes.back().get();
    }

    DFGNode* mul(DFGNode* a, DFGNode* b) {
        auto n = std::make_unique<DFGNode>(DFGOp::MUL);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        return nodes.back().get();
    }

    DFGNode* mux(DFGNode* sel, DFGNode* t, DFGNode* f) {
        auto n = std::make_unique<DFGNode>(DFGOp::MUX);
        n->in = {sel, t, f};
        nodes.push_back(std::move(n));
        return nodes.back().get();
    }

    // Assignment: embeds an expression tree
    // The expression tree's leaves reference signals by name (NamedReferenceNode)
    // or by pointer (ReferenceNode) - to be resolved later
    DFGNode* assign(std::unique_ptr<ExprNode> expr) {
        nodes.push_back(std::make_unique<DFGNode>(DFGOp::ASSIGN, std::move(expr)));
        return nodes.back().get();
    }
};

} // namespace custom_hdl
