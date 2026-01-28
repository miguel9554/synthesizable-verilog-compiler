#include "types.h"

#include <iostream>

#include "slang/syntax/AllSyntax.h"

namespace custom_hdl {

void TypeInfo::print(std::ostream& os) const {
    if (syntax) {
        os << syntax->toString();
    } else {
        os << "<no type>";
    }
}

void SignalInfo::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
    for (const auto& dim : dimensions) {
        if (dim.syntax) os << dim.syntax->toString();
    }
}

void ModuleHeaderInfo::print(std::ostream& os) const {
    os << "Module: " << name << "\n";
    if (!parameters.empty()) {
        os << "  Parameters:\n";
        for (const auto& param : parameters) {
            os << "    ";
            param.print(os);
            os << "\n";
        }
    }
    if (!inputs.empty()) {
        os << "  Inputs:\n";
        for (const auto& port : inputs) {
            os << "    ";
            port.print(os);
            os << "\n";
        }
    }
    if (!outputs.empty()) {
        os << "  Outputs:\n";
        for (const auto& port : outputs) {
            os << "    ";
            port.print(os);
            os << "\n";
        }
    }
}

} // namespace custom_hdl
