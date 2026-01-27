#include "types.h"
#include <iostream>

namespace custom_hdl {

TypeInfo TypeInfo::makeInteger(int width, bool is_signed) {
    return TypeInfo{
        .kind = TypeKind::Integer,
        .width = width,
        .metadata = IntegerInfo{.is_signed = is_signed}
    };
}

void TypeInfo::print(std::ostream& os) const {
    switch (kind) {
        case TypeKind::Integer:
            os << "Integer";
            break;
    }
    os << "[" << width << "]";
    if (std::holds_alternative<IntegerInfo>(metadata)) {
        auto& intInfo = std::get<IntegerInfo>(metadata);
        os << (intInfo.is_signed ? " signed" : " unsigned");
    }
}

void SignalInfo::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
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
