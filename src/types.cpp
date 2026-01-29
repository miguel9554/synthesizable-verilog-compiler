#include "types.h"

#include <iostream>

#include "slang/syntax/AllSyntax.h"

namespace custom_hdl {

void TypeInfo::print(std::ostream& os, bool debug) const {
    if (syntax) {
        if (debug) os << "\n";
        // Avoid printing Trivia (comments), just rawText of tokens.
        for (auto it = syntax->tokens_begin(); it != syntax->tokens_end(); ++it) {
            if (debug){
                os << "Token: ";
                os << (*it).rawText();
                os << "\n";
            } else {
                os << (*it).rawText();
                os << " ";
            }
        }
    } else {
        os << "<no type>";
    }
}

void SignalInfo::print(std::ostream& os) const {
    os << name << ": ";
    os << "type (";
    type.print(os, 0);
    if (dimensions.syntax) os << ") dim (" << dimensions.syntax->toString() << ")";
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
