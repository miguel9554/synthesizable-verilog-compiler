#include "ir_nodes.h"
#include <iostream>

namespace {

std::string indent_str(int indent) {
    return std::string(indent * 2, ' ');
}

} // anonymous namespace

namespace custom_hdl {

void IRModule::print(int indent) const {
    std::cout << indent_str(indent) << "Module: " << name << std::endl;

    if (!parameters.empty()) {
        std::cout << indent_str(indent + 1) << "Parameters:" << std::endl;
        for (const auto& param : parameters) {
            std::cout << indent_str(indent + 2);
            param.print(std::cout);
            std::cout << std::endl;
        }
    }

    if (!inputs.empty()) {
        std::cout << indent_str(indent + 1) << "Inputs:" << std::endl;
        for (const auto& in : inputs) {
            std::cout << indent_str(indent + 2);
            in.print(std::cout);
            std::cout << std::endl;
        }
    }

    if (!outputs.empty()) {
        std::cout << indent_str(indent + 1) << "Outputs:" << std::endl;
        for (const auto& out : outputs) {
            std::cout << indent_str(indent + 2);
            out.print(std::cout);
            std::cout << std::endl;
        }
    }

    std::cout << indent_str(indent + 1) << "Body:" << std::endl;
    for (const auto& node : body) {
        node->print(indent + 2);
    }
}

void IRAlways::print(int indent) const {
    std::cout << indent_str(indent) << "Always (" << sensitivity << ")" << std::endl;
    if (body) {
        body->print(indent + 1);
    }
}

void IRVariable::print(int indent) const {
    std::cout << indent_str(indent) << "Variable: " << name
              << " [" << type << ", " << width << " bits]" << std::endl;
}

void IRAssignment::print(int indent) const {
    std::cout << indent_str(indent) << "Assignment: " << target
              << (blocking ? " = " : " <= ") << value << std::endl;
}

void IRIf::print(int indent) const {
    std::cout << indent_str(indent) << "If (" << condition << ")" << std::endl;
    if (thenBranch) {
        std::cout << indent_str(indent + 1) << "Then:" << std::endl;
        thenBranch->print(indent + 2);
    }
    if (elseBranch) {
        std::cout << indent_str(indent + 1) << "Else:" << std::endl;
        elseBranch->print(indent + 2);
    }
}

void IRBlock::print(int indent) const {
    std::cout << indent_str(indent) << "Block {" << std::endl;
    for (const auto& stmt : statements) {
        stmt->print(indent + 1);
    }
    std::cout << indent_str(indent) << "}" << std::endl;
}

} // namespace custom_hdl
