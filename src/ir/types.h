#pragma once

#include <ostream>
#include <variant>

namespace custom_hdl {

enum class ResolvedTypeKind {
    Integer,
    Clock,
    Reset,
    // Future: Real, Struct, Enum, etc.
};

struct ResolvedIntegerInfo {
    bool is_signed = false;
};

using ResolvedTypeMetadata = std::variant<
    std::monostate,
    ResolvedIntegerInfo
>;

// TODO should actually have a packed dimension? or we don't care?
struct ResolvedType {
    ResolvedTypeKind kind = ResolvedTypeKind::Integer;
    int width = 0;
    ResolvedTypeMetadata metadata;

    void print(std::ostream& os) const;

    static ResolvedType makeInteger(int width, bool is_signed);

    bool isSigned() const {
        if (std::holds_alternative<ResolvedIntegerInfo>(metadata)) {
            return std::get<ResolvedIntegerInfo>(metadata).is_signed;
        }
        return false;
    }
};

} // namespace custom_hdl
