#include "passes/flop_resolve.h"
#include "ir/dfg.h"

#include <algorithm>
#include <format>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

namespace custom_hdl {

namespace {

// Generate all index suffixes for multi-dimensional arrays
// For [0:1], returns ["[0]", "[1]"]
// For [0:1][0:1], returns ["[0][0]", "[[0][1]", "[1][0]", "[1][1]"]
std::vector<std::string> generateIndexSuffixes(const std::vector<ResolvedDimension>& dimensions) {
    if (dimensions.empty()) {
        return {""};
    }

    std::vector<std::string> result = {""};
    for (const auto& dim : dimensions) {
        std::vector<std::string> newResult;
        int step = (dim.left <= dim.right) ? 1 : -1;
        for (int i = dim.left; step > 0 ? i <= dim.right : i >= dim.right; i += step) {
            for (const auto& prefix : result) {
                newResult.push_back(prefix + "[" + std::to_string(i) + "]");
            }
        }
        result = std::move(newResult);
    }
    return result;
}

// Return all leaf element names for a signal (expanding dimensions)
std::vector<std::string> allElements(const ResolvedSignal& signal) {
    std::vector<std::string> current = {signal.name};

    for (const auto& dimension : signal.dimensions) {
        std::vector<std::string> next;
        int start = std::min(dimension.left, dimension.right);
        int end = std::max(dimension.left, dimension.right);
        for (const auto& prefix : current) {
            for (int i = start; i <= end; i++) {
                next.push_back(prefix + "[" + std::to_string(i) + "]");
            }
        }
        current = std::move(next);
    }

    return current;
}

bool extract_reset(
    const DFGNode* dNodeDriver,
    const std::vector<asyncTrigger_t>& triggers,
    const std::string& flop_name,
    asyncTrigger_t& reset,
    asyncTrigger_t& clock,
    int& reset_value,
    DFGNode*& functionalLogic)
{
    bool has_reset;

    if (dNodeDriver->op != DFGOp::MUX) {
        throw std::runtime_error("Reset MUX not present.");
    }
    auto* mux_sel = dNodeDriver->in[0].node;
    auto* mux_true = dNodeDriver->in[1].node;
    auto* mux_else = dNodeDriver->in[2].node;
    bool is_negated = false;
    DFGNode* expectedResetNode;

    if (mux_sel->op == DFGOp::BITWISE_NOT ||
            mux_sel->op == DFGOp::LOGICAL_NOT) {
        is_negated = true;
        expectedResetNode = mux_sel->in[0].node;
    } else {
        expectedResetNode = mux_sel;
    }

    if (expectedResetNode->op != DFGOp::INPUT) {
        throw std::runtime_error("Reset MUX NOT driven by input.");
    }

    const std::string& reset_name = expectedResetNode->name;
    if (triggers[0].name == reset_name) {
        reset = triggers[0];
        clock = triggers[1];
    } else if (triggers[1].name == reset_name) {
        reset = triggers[1];
        clock = triggers[0];
    } else {
        throw std::runtime_error("Reset not present in sensitivity list.");
    }

    if (reset.edge == edge_t::NEGEDGE && !is_negated) {
        throw std::runtime_error("NEGEDGE reset should have NEG polarity.");
    }
    if (reset.edge == edge_t::POSEDGE && is_negated) {
        throw std::runtime_error("POSEDGE reset should have PLUS polarity.");
    }

    if (mux_true->op == DFGOp::SIGNAL) {
        if (mux_true->name != flop_name + ".q") {
            throw std::runtime_error("Unsupported SIGNAL for reset MUX TRUE: " + mux_true->name);
        }
        has_reset = false;
    } else if (mux_true->op == DFGOp::CONST) {
        has_reset = true;
        reset_value = std::get<int64_t>(mux_true->data);
    } else {
        throw std::runtime_error("Unsupported MUX TRUE branch for reset: " + mux_true->str());
    }

    functionalLogic = mux_else;
    return has_reset;
}

FlopInfo extractFlopClockAndReset(
    DFG& graph,
    ResolvedModule& resolved,
    const std::string& flop_name,
    const FlopInfo& flopIn,
    DFGNode*& functionalLogic)
{
    auto flop = flopIn;
    const std::string dName = flop_name + ".d";
    const auto& dNode = graph.signals.at(dName);
    auto* dNodeDriver = dNode->in[0].node;
    const auto& triggers = resolved.flopsTriggers.at(flopIn.name);

    if (dNode->in.size() != 1) {
        throw std::runtime_error(std::format(
            "Flop must have single driver: {} has {}", dName, dNode->in.size()));
    }

    asyncTrigger_t clock;
    asyncTrigger_t reset;
    bool has_reset;
    int reset_value;

    if (triggers.size() == 1) {
        clock = triggers[0];
        functionalLogic = dNodeDriver;
        has_reset = false;
    } else if (triggers.size() == 2) {
        has_reset = extract_reset(
            dNodeDriver, triggers, flopIn.name,
            reset, clock, reset_value, functionalLogic);
    } else {
        throw std::runtime_error(std::format(
            "Trigger size not supported: {}", triggers.size()));
    }

    flop.clock = clock;
    if (has_reset) {
        flop.reset = reset;
        flop.reset_value = reset_value;
    }
    flop.name = flop_name;
    flop.type.name = flop_name;
    return flop;
}

void check_functional_logic_no_clock_reset(
    DFGNode* functional_logic,
    asyncTrigger_t clock,
    std::optional<asyncTrigger_t> reset)
{
    std::set<DFGNode*> visited;
    std::vector<DFGNode*> to_visit = {functional_logic};

    while (!to_visit.empty()) {
        DFGNode* current = to_visit.back();
        to_visit.pop_back();

        if (visited.contains(current)) {
            continue;
        }
        visited.insert(current);

        if (current->op == DFGOp::INPUT) {
            if (current->name == clock.name) {
                throw std::runtime_error(
                    "Functional logic uses clock signal: " + clock.name);
            }
            if (reset && current->name == reset->name) {
                throw std::runtime_error(
                    "Functional logic uses reset signal: " + reset->name);
            }
        }

        for (const auto& inp : current->in) {
            to_visit.push_back(inp.node);
        }
    }
}

} // anonymous namespace

void resolveFlops(ResolvedModule& resolved) {
    if (!resolved.dfg || resolved.flops.empty()) {
        return;
    }

    DFG& graph = *resolved.dfg;

    // Connect flop outputs to their .q signals
    for (const auto& output : resolved.outputs) {
        if (resolved.flopsTriggers.contains(output.name)) {
            if (output.dimensions.empty()) {
                std::string qName = output.name + ".q";
                DFGNode* qNode = graph.lookupSignal(qName);
                if (qNode) {
                    graph.connectOutput(output.name, qNode);
                }
            } else {
                for (const auto& suffix : generateIndexSuffixes(output.dimensions)) {
                    std::string elemOutput = output.name + suffix;
                    std::string elemQ = output.name + suffix + ".q";
                    DFGNode* qNode = graph.lookupSignal(elemQ);
                    if (qNode && graph.outputs.contains(elemOutput)) {
                        graph.connectOutput(elemOutput, qNode);
                    }
                }
            }
        }
    }

    // Extract clock/reset info and validate functional logic
    std::vector<FlopInfo> resolved_flops;
    for (const auto& flop : resolved.flops) {
        for (const auto& name : allElements(flop.type)) {
            DFGNode* functional_logic;
            resolved_flops.push_back(
                extractFlopClockAndReset(graph, resolved, name, flop, functional_logic));
            auto& output = graph.signals.at(name + ".d");
            check_functional_logic_no_clock_reset(
                functional_logic,
                resolved_flops.back().clock,
                resolved_flops.back().reset);
            output->in = {functional_logic};
        }
    }
    resolved.flops = resolved_flops;
}

} // namespace custom_hdl
