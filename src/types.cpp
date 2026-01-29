#include "types.h"

#include <iostream>

#include "slang/syntax/AllSyntax.h"

namespace custom_hdl {

void UnresolvedType::print(std::ostream& os, bool debug) const {
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

void UnresolvedSignal::print(std::ostream& os) const {
    os << name << ": ";
    os << "type (";
    type.print(os, 0);
    if (dimensions.syntax) os << ") dim (" << dimensions.syntax->toString() << ")";
}

} // namespace custom_hdl
