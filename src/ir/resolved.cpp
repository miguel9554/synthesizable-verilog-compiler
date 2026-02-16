#include "ir/resolved.h"
#include "util/debug.h"

#include <fstream>
#include <iostream>

namespace custom_hdl {

// ============================================================================
// ResolvedType implementation
// ============================================================================

ResolvedType ResolvedType::makeInteger(int width, bool is_signed) {
    return ResolvedType{
        .kind = ResolvedTypeKind::Integer,
        .width = width,
        .metadata = ResolvedIntegerInfo{.is_signed = is_signed}
    };
}

void ResolvedType::print(std::ostream& os) const {
    switch (kind) {
        case ResolvedTypeKind::Clock:
            os << "Clock";
            break;
        case ResolvedTypeKind::Reset:
            os << "Reset";
            break;
        case ResolvedTypeKind::Integer:
            os << "Integer";
            break;
    }
    os << "[" << width << "]";
    if (std::holds_alternative<ResolvedIntegerInfo>(metadata)) {
        auto& intInfo = std::get<ResolvedIntegerInfo>(metadata);
        os << (intInfo.is_signed ? " signed" : " unsigned");
    }
}

std::ostream& operator<<(std::ostream& os, const asyncTrigger_t& t) {
    os << (t.edge == POSEDGE ? "posedge" : "negedge")
       << ":" << t.name;
    return os;
}

void FlopInfo::print(std::ostream& os, int indent) const {
    auto indent_str = [](int n) { return std::string(n * 2, ' '); };

    os << indent_str(indent) << "Flop: " << name << std::endl;
    os << indent_str(indent + 1) << "type: ";
    type.print(os);
    os << std::endl;
    os << indent_str(indent + 1) << "flop_type: ";
    switch (flop_type) {
        case FLOP_D: os << "FLOP_D"; break;
    }
    os << std::endl;
    os << indent_str(indent + 1) << "clock: " << clock << std::endl;
    if (reset) {
        os << indent_str(indent + 1) << "reset: " << *reset << std::endl;
    }
    if (reset_value) {
        os << indent_str(indent + 1) << "reset_value: " << *reset_value << std::endl;
    }
}

void ResolvedSignal::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
    for (const auto& dim : dimensions) {
        os << "[" << dim.left << ":" << dim.right << "]";
    }
}

void ResolvedModule::print(int indent) const {
    auto indent_str = [](int n) { return std::string(n * 2, ' '); };

    std::cout << indent_str(indent) << "Module: " << this->name << std::endl;

    std::cout << indent_str(indent + 1) << "Parameters:" << std::endl;
    for (const auto& param : this->parameters) {
        std::cout << indent_str(indent + 2);
        param.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Inputs:" << std::endl;
    for (const auto& in : this->inputs) {
        std::cout << indent_str(indent + 2);
        in.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Outputs:" << std::endl;
    for (const auto& out : this->outputs) {
        std::cout << indent_str(indent + 2);
        out.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Signals:" << std::endl;
    for (const auto& signal : this->signals) {
        std::cout << indent_str(indent + 2);
        signal.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Flops:" << std::endl;
    for (const auto& flop : this->flops) {
        flop.print(std::cout, indent + 2);
    }

    std::cout << indent_str(indent + 1) << "Submodules:" << std::endl;
    for (const auto& sub : this->hierarchyInstantiation) {
        std::cout << indent_str(indent + 2) << sub.name;
        for (const auto& p : sub.parameters) {
            std::cout << " " << p.name << "=" << p.value;
        }
        std::cout << std::endl;
    }

    // Write DFG to files
    if (this->dfg) {
        ensureDebugOutputDir();
        std::string graphName = this->name + "_dfg";

        // Write DOT file
        std::string dotFilename = DEBUG_OUTPUT_DIR + "/" + graphName + ".dot";
        std::ofstream dotOut(dotFilename);
        if (dotOut) {
            dotOut << this->dfg->toDot(graphName);
            std::cout << indent_str(indent + 1) << "Wrote DFG to: " << dotFilename << std::endl;
        }

        // Write JSON file
        std::string jsonFilename = DEBUG_OUTPUT_DIR + "/" + graphName + ".json";
        std::ofstream jsonOut(jsonFilename);
        if (jsonOut) {
            jsonOut << this->dfg->toJson();
            std::cout << indent_str(indent + 1) << "Wrote DFG JSON to: " << jsonFilename << std::endl;
        }
    }
}

} // namespace custom_hdl
