#include "ir/dfg.h"

#include <map>
#include <sstream>

namespace custom_hdl {

std::string DFG::toDot(const std::string& graphName) const {
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
                ss << "INPUT\\n" << node->name;
                break;
            case DFGOp::OUTPUT:
                ss << "OUTPUT\\n" << node->name;
                break;
            case DFGOp::SIGNAL:
                ss << "SIGNAL\\n" << node->name;
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
            case DFGOp::POWER: ss << "**"; break;
            case DFGOp::MUX: ss << "MUX"; break;
            case DFGOp::MUX_N: ss << "MUX_N"; break;
            case DFGOp::MODULE:
                ss << "MODULE\\n" << node->name;
                if (std::holds_alternative<std::string>(node->data))
                    ss << "\\n(" << std::get<std::string>(node->data) << ")";
                if (!node->output_names.empty()) {
                    ss << "\\nouts: ";
                    for (size_t oi = 0; oi < node->output_names.size(); ++oi) {
                        if (oi > 0) ss << ", ";
                        ss << node->output_names[oi];
                    }
                }
                break;
            case DFGOp::INDEX: ss << "INDEX"; break;
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
        for (const auto& input : node->in) {
            ss << "  n" << nodeIndex.at(input.node) << " -> n" << i;
            if (input.port != 0) {
                ss << " [label=\"port " << input.port << "\"]";
            }
            ss << ";\n";
        }
    }

    ss << "}\n";
    return ss.str();
}

std::string DFG::toJson(int indent) const {
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
            case DFGOp::POWER: ss << "POWER"; break;
            case DFGOp::MUX: ss << "MUX"; break;
            case DFGOp::MUX_N: ss << "MUX_N"; break;
            case DFGOp::MODULE: ss << "MODULE"; break;
            case DFGOp::INDEX: ss << "INDEX"; break;
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

        // Add name if present
        if (!node->name.empty()) {
            ss << indentStr(indent + 3) << "\"name\": \"" << node->name << "\",\n";
        }
        // Add data field based on variant type
        if (std::holds_alternative<int64_t>(node->data)) {
            ss << indentStr(indent + 3) << "\"value\": " << std::get<int64_t>(node->data) << ",\n";
        }
        if (std::holds_alternative<std::string>(node->data)) {
            ss << indentStr(indent + 3) << "\"module_type\": \"" << std::get<std::string>(node->data) << "\",\n";
        }

        // Add output_names for multi-output nodes
        if (!node->output_names.empty()) {
            ss << indentStr(indent + 3) << "\"output_names\": [";
            for (size_t j = 0; j < node->output_names.size(); ++j) {
                ss << "\"" << node->output_names[j] << "\"";
                if (j < node->output_names.size() - 1) ss << ", ";
            }
            ss << "],\n";
        }

        // Add inputs
        ss << indentStr(indent + 3) << "\"inputs\": [";
        for (size_t j = 0; j < node->in.size(); ++j) {
            // Find the index of the input node
            for (size_t k = 0; k < nodes.size(); ++k) {
                if (nodes[k].get() == node->in[j].node) {
                    if (node->in[j].port != 0) {
                        ss << "{\"node\": " << k << ", \"port\": " << node->in[j].port << "}";
                    } else {
                        ss << k;
                    }
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

} // namespace custom_hdl
