#pragma once

#include <cstdlib>
#include <ostream>
#include <variant>
#include <vector>

namespace custom_hdl {

struct ResolvedDimension {
    int left = 0;
    int right = 0;

    int size() const { return std::abs(left - right) + 1; }
};

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

struct ResolvedType {
    ResolvedTypeKind kind = ResolvedTypeKind::Integer;
    int width = 0;
    ResolvedTypeMetadata metadata;
    std::vector<ResolvedDimension> packed_dims;
    std::vector<ResolvedDimension> unpacked_dims;

    void print(std::ostream& os) const;

    static ResolvedType makeInteger(int width, bool is_signed,
                                    std::vector<ResolvedDimension> packed_dims = {},
                                    std::vector<ResolvedDimension> unpacked_dims = {});

    bool isSigned() const {
        if (std::holds_alternative<ResolvedIntegerInfo>(metadata)) {
            return std::get<ResolvedIntegerInfo>(metadata).is_signed;
        }
        return false;
    }
};

} // namespace custom_hdl
