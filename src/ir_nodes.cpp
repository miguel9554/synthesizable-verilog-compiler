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

    std::cout << indent_str(indent + 1) << "Parameters:" << std::endl;
    for (const auto& param : parameters) {
        std::cout << indent_str(indent + 2);
        param.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Inputs:" << std::endl;
    for (const auto& in : inputs) {
        std::cout << indent_str(indent + 2);
        in.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Outputs:" << std::endl;
    for (const auto& out : outputs) {
        std::cout << indent_str(indent + 2);
        out.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "signals:" << std::endl;
    for (const auto& signal : signals) {
        std::cout << indent_str(indent + 2);
        signal.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "flops:" << std::endl;
    for (const auto& flop : flops) {
        std::cout << indent_str(indent + 2);
        flop.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "No of procedural timing:" << proceduralTimingBlocks.size() << std::endl;

    std::cout << indent_str(indent + 1) << "No of procedural combo:" << proceduralComboBlocks.size() << std::endl;

    std::cout << indent_str(indent + 1) << "No of cont. assign.: " << assignStatements.size() << std::endl;

    std::cout << indent_str(indent + 1) << "No of hier. inst.: " << hierarchyInstantiation.size() << std::endl;

}

} // namespace custom_hdl
