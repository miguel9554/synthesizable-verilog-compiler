#pragma once

#include <algorithm>
#include <map>
#include <memory>
#include <format>
#include <vector>
#include <string>
#include <sstream>
#include <variant>

namespace custom_hdl {

enum class DFGOp {
    INPUT,      // Primary input (module port)
    OUTPUT,     // Primary output (module port)
    SIGNAL,     // Internal signal (named placeholder)
    CONST,      // Constant value (data: int64_t)
    // Binary ops
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,         // Equal (==)
    LT,         // Less than (<)
    LE,         // Less or equal (<=)
    GT,         // Greater than (>)
    GE,         // Greater or equal (>=)
    SHL,        // Arithmetic shift left (<<<)
    ASR,        // Arithmetic shift right (>>>)
    MUX,        // 2:1 mux: in[0]=sel, in[1]=true_val, in[2]=false_val
    MUX_N,      // N:1 mux: in[0..N-1]=selectors, in[N..2N-1]=data values (one-hot select)
    // Unary ops (single input)
    UNARY_PLUS,
    UNARY_NEGATE,
    BITWISE_NOT,
    LOGICAL_NOT,
    REDUCTION_AND,
    REDUCTION_NAND,
    REDUCTION_OR,
    REDUCTION_NOR,
    REDUCTION_XOR,
    REDUCTION_XNOR,
};

struct DFGNode {
    DFGOp op;
    std::vector<DFGNode*> in;  // fan-in (operands)

    // Metadata depending on op type:
    // - monostate: no extra data (ADD, SUB, MUL, DIV, MUX)
    // - int64_t: constant value (CONST)
    // - string: signal name (INPUT, OUTPUT)
    std::variant<
        std::monostate,
        int64_t,
        std::string
    > data;

    DFGNode(DFGOp o) : op(o) {}
    DFGNode(DFGOp o, std::string name) : op(o), data(std::move(name)) {}
    DFGNode(DFGOp o, int64_t val) : op(o), data(val) {}
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

    // Create a named signal placeholder (for internal signals, not ports)
    DFGNode* createSignal(const std::string& name) {
        nodes.push_back(std::make_unique<DFGNode>(DFGOp::SIGNAL, name));
        auto result = signals.insert({name, nodes.back().get()});
        if (!result.second) {
            throw std::runtime_error(std::format("Signal {} already exists", name));
        }
        return nodes.back().get();
    }

    // Lookup signal for READING in expressions
    // Returns the node representing the signal's value
    DFGNode* lookupSignal(const std::string& name) const {
        if (auto it = inputs.find(name); it != inputs.end()) return it->second;
        if (auto it = signals.find(name); it != signals.end()) return it->second;
        if (auto it = outputs.find(name); it != outputs.end()) {
            // For outputs, return the driver if connected (reading output value)
            auto* outNode = it->second;
            return outNode->in.empty() ? outNode : outNode->in[0];
        }
        return nullptr;
    }

    // Connect a driver to an existing output node
    void connectOutput(const std::string& name, DFGNode* driver) {
        auto it = outputs.find(name);
        if (it == outputs.end()) {
            throw std::runtime_error(std::format("Output {} not found", name));
        }
        it->second->in = {driver};
    }

    // Connect a driver to an existing signal node
    void connectSignal(const std::string& name, DFGNode* driver) {
        auto it = signals.find(name);
        if (it == signals.end()) {
            throw std::runtime_error(std::format("Signal {} not found", name));
        }
        it->second->in = {driver};
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

    DFGNode* shl(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::SHL)
            : std::make_unique<DFGNode>(DFGOp::SHL, name);
        n->in = {a, b};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* asr(DFGNode* a, DFGNode* b, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::ASR)
            : std::make_unique<DFGNode>(DFGOp::ASR, name);
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

    // N:1 mux with one-hot selectors
    // selectors[i] active => output = data[i]
    DFGNode* muxN(const std::vector<DFGNode*>& selectors,
                  const std::vector<DFGNode*>& data,
                  const std::string& name = "") {
        if (selectors.size() != data.size()) {
            throw std::runtime_error("muxN: selectors and data must have same size");
        }
        if (selectors.empty()) {
            throw std::runtime_error("muxN: must have at least one input");
        }
        auto n = name.empty()
            ? std::make_unique<DFGNode>(DFGOp::MUX_N)
            : std::make_unique<DFGNode>(DFGOp::MUX_N, name);
        // Layout: selectors first, then data
        n->in.reserve(selectors.size() + data.size());
        for (auto* s : selectors) n->in.push_back(s);
        for (auto* d : data) n->in.push_back(d);
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    // Helper for unary operations (single input)
    DFGNode* unaryOp(DFGOp op, DFGNode* a, const std::string& name = "") {
        auto n = name.empty()
            ? std::make_unique<DFGNode>(op)
            : std::make_unique<DFGNode>(op, name);
        n->in = {a};
        nodes.push_back(std::move(n));
        if (!name.empty()) {
            signals[name] = nodes.back().get();
        }
        return nodes.back().get();
    }

    DFGNode* unaryPlus(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::UNARY_PLUS, a, name);
    }

    DFGNode* unaryNegate(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::UNARY_NEGATE, a, name);
    }

    DFGNode* bitwiseNot(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::BITWISE_NOT, a, name);
    }

    DFGNode* logicalNot(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::LOGICAL_NOT, a, name);
    }

    DFGNode* reductionAnd(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::REDUCTION_AND, a, name);
    }

    DFGNode* reductionNand(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::REDUCTION_NAND, a, name);
    }

    DFGNode* reductionOr(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::REDUCTION_OR, a, name);
    }

    DFGNode* reductionNor(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::REDUCTION_NOR, a, name);
    }

    DFGNode* reductionXor(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::REDUCTION_XOR, a, name);
    }

    DFGNode* reductionXnor(DFGNode* a, const std::string& name = "") {
        return unaryOp(DFGOp::REDUCTION_XNOR, a, name);
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
                case DFGOp::SIGNAL:
                    ss << "SIGNAL\\n" << std::get<std::string>(node->data);
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
                case DFGOp::SHL: ss << "<<<"; break;
                case DFGOp::ASR: ss << ">>>"; break;
                case DFGOp::MUX: ss << "MUX"; break;
                case DFGOp::MUX_N: ss << "MUX_N"; break;
                case DFGOp::UNARY_PLUS: ss << "+"; break;
                case DFGOp::UNARY_NEGATE: ss << "-"; break;
                case DFGOp::BITWISE_NOT: ss << "~"; break;
                case DFGOp::LOGICAL_NOT: ss << "!"; break;
                case DFGOp::REDUCTION_AND: ss << "&"; break;
                case DFGOp::REDUCTION_NAND: ss << "~&"; break;
                case DFGOp::REDUCTION_OR: ss << "|"; break;
                case DFGOp::REDUCTION_NOR: ss << "~|"; break;
                case DFGOp::REDUCTION_XOR: ss << "^"; break;
                case DFGOp::REDUCTION_XNOR: ss << "~^"; break;
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

    std::string toJson(int indent = 0) const {
        auto indentStr = [](int n) { return std::string(n * 2, ' '); };

        std::ostringstream ss;
        ss << indentStr(indent) << "{\n";
        ss << indentStr(indent + 1) << "\"nodes\": [\n";

        for (size_t i = 0; i < nodes.size(); ++i) {
            const auto& node = nodes[i];
            ss << indentStr(indent + 2) << "{\n";
            ss << indentStr(indent + 3) << "\"id\": " << i << ",\n";
            ss << indentStr(indent + 3) << "\"op\": \"";

            switch (node->op) {
                case DFGOp::INPUT: ss << "INPUT"; break;
                case DFGOp::OUTPUT: ss << "OUTPUT"; break;
                case DFGOp::SIGNAL: ss << "SIGNAL"; break;
                case DFGOp::CONST: ss << "CONST"; break;
                case DFGOp::ADD: ss << "ADD"; break;
                case DFGOp::SUB: ss << "SUB"; break;
                case DFGOp::MUL: ss << "MUL"; break;
                case DFGOp::DIV: ss << "DIV"; break;
                case DFGOp::EQ: ss << "EQ"; break;
                case DFGOp::LT: ss << "LT"; break;
                case DFGOp::LE: ss << "LE"; break;
                case DFGOp::GT: ss << "GT"; break;
                case DFGOp::GE: ss << "GE"; break;
                case DFGOp::SHL: ss << "SHL"; break;
                case DFGOp::ASR: ss << "ASR"; break;
                case DFGOp::MUX: ss << "MUX"; break;
                case DFGOp::MUX_N: ss << "MUX_N"; break;
                case DFGOp::UNARY_PLUS: ss << "UNARY_PLUS"; break;
                case DFGOp::UNARY_NEGATE: ss << "UNARY_NEGATE"; break;
                case DFGOp::BITWISE_NOT: ss << "BITWISE_NOT"; break;
                case DFGOp::LOGICAL_NOT: ss << "LOGICAL_NOT"; break;
                case DFGOp::REDUCTION_AND: ss << "REDUCTION_AND"; break;
                case DFGOp::REDUCTION_NAND: ss << "REDUCTION_NAND"; break;
                case DFGOp::REDUCTION_OR: ss << "REDUCTION_OR"; break;
                case DFGOp::REDUCTION_NOR: ss << "REDUCTION_NOR"; break;
                case DFGOp::REDUCTION_XOR: ss << "REDUCTION_XOR"; break;
                case DFGOp::REDUCTION_XNOR: ss << "REDUCTION_XNOR"; break;
            }
            ss << "\",\n";

            // Add data field based on variant type
            if (std::holds_alternative<int64_t>(node->data)) {
                ss << indentStr(indent + 3) << "\"value\": " << std::get<int64_t>(node->data) << ",\n";
            } else if (std::holds_alternative<std::string>(node->data)) {
                ss << indentStr(indent + 3) << "\"name\": \"" << std::get<std::string>(node->data) << "\",\n";
            }

            // Add inputs
            ss << indentStr(indent + 3) << "\"inputs\": [";
            for (size_t j = 0; j < node->in.size(); ++j) {
                // Find the index of the input node
                for (size_t k = 0; k < nodes.size(); ++k) {
                    if (nodes[k].get() == node->in[j]) {
                        ss << k;
                        break;
                    }
                }
                if (j < node->in.size() - 1) ss << ", ";
            }
            ss << "]\n";

            ss << indentStr(indent + 2) << "}";
            if (i < nodes.size() - 1) ss << ",";
            ss << "\n";
        }

        ss << indentStr(indent + 1) << "]\n";
        ss << indentStr(indent) << "}";
        return ss.str();
    }
};

} // namespace custom_hdl
