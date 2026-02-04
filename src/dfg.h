#pragma once

#include <algorithm>
#include <map>
#include <memory>
#include <format>
#include <vector>
#include <string>
#include <sstream>
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
    EQ,         // Equal (==)
    LT,         // Less than (<)
    LE,         // Less or equal (<=)
    GT,         // Greater than (>)
    GE,         // Greater or equal (>=)
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

    // Quick access named nodes
    std::map<std::string, DFGNode*> inputs;
    std::map<std::string, DFGNode*> outputs;
    std::map<std::string, DFGNode*> signals;

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

    // An output can be recreated, if it's assigned again.
    DFGNode* output(DFGNode* a, const std::string& name, bool rename) {
        auto n = std::make_unique<DFGNode>(DFGOp::OUTPUT, name);
        n->in = {a};
        nodes.push_back(std::move(n));
        auto result = outputs.insert({name, nodes.back().get()});
        if (!result.second) {
            if (!rename) throw std::runtime_error(std::format("Output {} already exists", name));
            // Remove old output node from the nodes vector
            auto oldNode = outputs[name];
            auto it = std::find_if(nodes.begin(), nodes.end(),
                [oldNode](const std::unique_ptr<DFGNode>& n) {
                    return n.get() == oldNode;
                });
            if (it != nodes.end()) {
                nodes.erase(it);
            }
            outputs[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* add(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::ADD)
            : std::make_unique<DFGNode>(DFGOp::ADD, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* sub(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::SUB)
            : std::make_unique<DFGNode>(DFGOp::SUB, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* mul(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::MUL)
            : std::make_unique<DFGNode>(DFGOp::MUL, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* eq(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::EQ)
            : std::make_unique<DFGNode>(DFGOp::EQ, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* lt(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::LT)
            : std::make_unique<DFGNode>(DFGOp::LT, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* le(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::LE)
            : std::make_unique<DFGNode>(DFGOp::LE, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* gt(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::GT)
            : std::make_unique<DFGNode>(DFGOp::GT, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* ge(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::GE)
            : std::make_unique<DFGNode>(DFGOp::GE, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* mux(DFGNode* sel, DFGNode* t, DFGNode* f, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::MUX)
            : std::make_unique<DFGNode>(DFGOp::MUX, name);
        n->in = {sel, t, f};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    std::string toDot(const std::string& graphName = "DFG") const {
        std::ostringstream ss;
        ss << "digraph " << graphName << " {\n";
        ss << "  rankdir=TB;\n";
        ss << "  node [shape=record];\n\n";

        // Build node index map
        std::map<const DFGNode*, size_t> nodeIndex;
        for (size_t i = 0; i < nodes.size(); ++i) {
            nodeIndex[nodes[i].get()] = i;
        }

        // Output nodes
        for (size_t i = 0; i < nodes.size(); ++i) {
            const auto& node = nodes[i];
            ss << "  n" << i << " [label=\"";

            switch (node->op) {
                case DFGOp::INPUT:
                    ss << "INPUT\\n" << std::get<std::string>(node->data);
                    break;
                case DFGOp::OUTPUT:
                    ss << "OUTPUT\\n" << std::get<std::string>(node->data);
                    break;
                case DFGOp::CONST:
                    ss << "CONST\\n" << std::get<int64_t>(node->data);
                    break;
                case DFGOp::ADD: ss << "+"; break;
                case DFGOp::SUB: ss << "-"; break;
                case DFGOp::MUL: ss << "*"; break;
                case DFGOp::DIV: ss << "/"; break;
                case DFGOp::EQ:  ss << "=="; break;
                case DFGOp::LT:  ss << "<"; break;
                case DFGOp::LE:  ss << "<="; break;
                case DFGOp::GT:  ss << ">"; break;
                case DFGOp::GE:  ss << ">="; break;
                case DFGOp::MUX: ss << "MUX"; break;
                case DFGOp::ASSIGN: ss << "ASSIGN"; break;
            }

            ss << "\"];\n";
        }

        ss << "\n";

        // Output edges
        for (size_t i = 0; i < nodes.size(); ++i) {
            const auto& node = nodes[i];
            for (const auto* input : node->in) {
                ss << "  n" << nodeIndex.at(input) << " -> n" << i << ";\n";
            }
        }

        ss << "}\n";
        return ss.str();
    }
};

} // namespace custom_hdl
