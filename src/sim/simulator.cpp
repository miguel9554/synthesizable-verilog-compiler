#include "sim/simulator.h"

#include <algorithm>
#include <bit>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <queue>
#include <random>
#include <set>
#include <sstream>

#include "yaml-cpp/yaml.h"

namespace custom_hdl {

// ============================================================================
// Config parser
// ============================================================================

SimConfig parseSimConfig(const std::string& yaml_path) {
    YAML::Node root = YAML::LoadFile(yaml_path);

    SimConfig config;

    if (!root["top_module"])
        throw CompilerError("Sim config missing 'top_module'");
    config.top_module = root["top_module"].as<std::string>();

    if (!root["inputs"])
        throw CompilerError("Sim config missing 'inputs'");
    for (auto it = root["inputs"].begin(); it != root["inputs"].end(); ++it) {
        config.input_files[it->first.as<std::string>()] = it->second.as<std::string>();
    }

    if (!root["output_dir"])
        throw CompilerError("Sim config missing 'output_dir'");
    config.output_dir = root["output_dir"].as<std::string>();

    return config;
}

// ============================================================================
// Bit mask helper
// ============================================================================

int64_t Simulator::maskToWidth(int64_t val, const DFGNode* node) {
    if (!node->type.has_value() || node->type->width <= 0)
        return val;

    int w = node->type->width;
    // Width > 64 is rejected in constructor, so w is in [1, 64]
    uint64_t mask = (w == 64) ? ~0ULL : (1ULL << w) - 1;
    val &= static_cast<int64_t>(mask);

    // Sign-extend if signed
    if (node->type->isSigned() && w < 64 && (val & (1LL << (w - 1)))) {
        val |= ~static_cast<int64_t>(mask);
    }
    return val;
}

// ============================================================================
// Topological sort (Kahn's algorithm)
// ============================================================================

void Simulator::buildTopology() {
    const auto& nodes = module_.dfg->nodes;

    // Build flop .q node -> FlopInfo map (needed by run loop, not by topo sort)
    for (const auto& flop : module_.flops) {
        std::string qname = flop.name + ".q";
        if (auto it = module_.dfg->signals.find(qname); it != module_.dfg->signals.end()) {
            flop_q_nodes_[it->second] = &flop;
        }
    }

    // The DFG is purely combinational (no cycles). Kahn's algorithm gives eval order.
    std::map<const DFGNode*, int> in_degree;
    std::map<const DFGNode*, std::vector<const DFGNode*>> successors;

    for (const auto& node : nodes) {
        in_degree[node.get()] = 0;
    }

    for (const auto& node : nodes) {
        for (const auto& input : node->in) {
            in_degree[node.get()]++;
            successors[input.node].push_back(node.get());
        }
    }

    std::queue<const DFGNode*> q;
    for (const auto& [node, deg] : in_degree) {
        if (deg == 0) q.push(node);
    }

    topo_order_.clear();
    while (!q.empty()) {
        const DFGNode* curr = q.front();
        q.pop();
        topo_order_.push_back(curr);

        for (const DFGNode* succ : successors[curr]) {
            if (--in_degree[succ] == 0) {
                q.push(succ);
            }
        }
    }

    if (topo_order_.size() != nodes.size()) {
        throw CompilerError(std::format(
            "Simulator: topological sort failed — {} of {} nodes sorted (cycle in DFG?)",
            topo_order_.size(), nodes.size()));
    }
}

// ============================================================================
// Build async event timeline from clock/reset input files
// ============================================================================

void Simulator::buildTimeline() {
    // Determine which inputs are async (clocks and resets)
    for (const auto& flop : module_.flops) {
        async_inputs_.insert(flop.clock.name);
        if (flop.reset.has_value()) {
            async_inputs_.insert(flop.reset->name);
        }
    }

    // Parse async input files: "time value" format per line
    for (const auto& name : async_inputs_) {
        auto it = config_.input_files.find(name);
        if (it == config_.input_files.end()) {
            throw CompilerError(std::format(
                "Simulator: async input '{}' has no file in config", name));
        }

        std::ifstream file(it->second);
        if (!file.is_open()) {
            throw CompilerError(std::format(
                "Simulator: cannot open async input file '{}'", it->second));
        }

        std::string line;
        while (std::getline(file, line)) {
            if (line.empty() || line[0] == '#') continue;
            std::istringstream iss(line);
            int64_t time, value;
            if (!(iss >> time >> value)) {
                throw CompilerError(std::format(
                    "Simulator: bad line in async file '{}': {}", it->second, line));
            }
            timeline_.push_back({time, name, value});
        }
    }

    // Sort by time (stable sort preserves file order for same-time events)
    std::stable_sort(timeline_.begin(), timeline_.end(),
        [](const AsyncEvent& a, const AsyncEvent& b) { return a.time < b.time; });
}

// ============================================================================
// Build flop lookup maps
// ============================================================================

void Simulator::buildFlopMaps() {
    for (const auto& flop : module_.flops) {
        flops_by_clock_[flop.clock.name].push_back(&flop);
        if (flop.reset.has_value()) {
            flops_by_reset_[flop.reset->name].push_back(&flop);
        }
    }
}

// ============================================================================
// Load sync input files (one value per line)
// ============================================================================

void Simulator::loadSyncInputs() {
    for (const auto& [name, path] : config_.input_files) {
        if (async_inputs_.count(name)) continue;  // skip async inputs

        std::ifstream file(path);
        if (!file.is_open()) {
            throw CompilerError(std::format(
                "Simulator: cannot open sync input file '{}'", path));
        }

        std::vector<int64_t> values;
        std::string line;
        while (std::getline(file, line)) {
            if (line.empty() || line[0] == '#') continue;
            values.push_back(std::stoll(line));
        }

        if (values.empty()) {
            throw CompilerError(std::format(
                "Simulator: sync input file '{}' is empty", path));
        }

        sync_input_data_[name] = std::move(values);
        sync_input_pos_[name] = 0;
    }
}

// ============================================================================
// Node evaluation
// ============================================================================

int64_t Simulator::evaluateNode(const DFGNode* node) {
    auto getVal = [&](int idx) -> int64_t {
        return values_.at(node->in[idx].node);
    };

    switch (node->op) {
        case DFGOp::INPUT:
        case DFGOp::CONST:
            // Pre-set, should already be in values_
            return values_.at(node);

        case DFGOp::SIGNAL:
        case DFGOp::OUTPUT:
            if (node->in.empty()) return values_.at(node);
            return getVal(0);

        case DFGOp::ADD:   return getVal(0) + getVal(1);
        case DFGOp::SUB:   return getVal(0) - getVal(1);
        case DFGOp::MUL:   return getVal(0) * getVal(1);
        case DFGOp::DIV: {
            int64_t divisor = getVal(1);
            if (divisor == 0)
                throw CompilerError("Simulator: division by zero", node);
            return getVal(0) / divisor;
        }
        case DFGOp::POWER: {
            int64_t base = getVal(0);
            int64_t exp = getVal(1);
            if (exp < 0) return 0;
            int64_t result = 1;
            for (int64_t i = 0; i < exp; i++) result *= base;
            return result;
        }

        case DFGOp::EQ:  return getVal(0) == getVal(1) ? 1 : 0;
        case DFGOp::LT:  return getVal(0) <  getVal(1) ? 1 : 0;
        case DFGOp::LE:  return getVal(0) <= getVal(1) ? 1 : 0;
        case DFGOp::GT:  return getVal(0) >  getVal(1) ? 1 : 0;
        case DFGOp::GE:  return getVal(0) >= getVal(1) ? 1 : 0;

        case DFGOp::SHL:  return getVal(0) << getVal(1);
        case DFGOp::ASR:  return getVal(0) >> getVal(1);  // arithmetic if signed (C++ guarantees for signed)

        case DFGOp::MUX:
            return getVal(0) ? getVal(1) : getVal(2);

        case DFGOp::MUX_N: {
            int n = static_cast<int>(node->in.size()) / 2;
            // First active one-hot selector picks its data value
            for (int i = 0; i < n; i++) {
                if (values_.at(node->in[i].node) != 0) {
                    return values_.at(node->in[n + i].node);
                }
            }
            // No selector active — return last data value as default
            return values_.at(node->in[2 * n - 1].node);
        }

        case DFGOp::UNARY_PLUS:    return getVal(0);
        case DFGOp::UNARY_NEGATE:  return -getVal(0);
        case DFGOp::BITWISE_NOT:   return ~getVal(0);
        case DFGOp::LOGICAL_NOT:   return getVal(0) == 0 ? 1 : 0;

        case DFGOp::REDUCTION_AND: {
            int w = node->in[0].node->type.has_value() ? node->in[0].node->type->width : 64;
            uint64_t mask = (w == 64) ? ~0ULL : (1ULL << w) - 1;
            return (static_cast<uint64_t>(getVal(0)) & mask) == mask ? 1 : 0;
        }
        case DFGOp::REDUCTION_NAND: {
            int w = node->in[0].node->type.has_value() ? node->in[0].node->type->width : 64;
            uint64_t mask = (w == 64) ? ~0ULL : (1ULL << w) - 1;
            return (static_cast<uint64_t>(getVal(0)) & mask) == mask ? 0 : 1;
        }
        case DFGOp::REDUCTION_OR:
            return getVal(0) != 0 ? 1 : 0;
        case DFGOp::REDUCTION_NOR:
            return getVal(0) == 0 ? 1 : 0;
        case DFGOp::REDUCTION_XOR: {
            uint64_t v = static_cast<uint64_t>(getVal(0));
            int w = node->in[0].node->type.has_value() ? node->in[0].node->type->width : 64;
            if (w < 64) v &= (1ULL << w) - 1;
            return std::popcount(v) & 1;
        }
        case DFGOp::REDUCTION_XNOR: {
            uint64_t v = static_cast<uint64_t>(getVal(0));
            int w = node->in[0].node->type.has_value() ? node->in[0].node->type->width : 64;
            if (w < 64) v &= (1ULL << w) - 1;
            return (std::popcount(v) & 1) ^ 1;
        }

        case DFGOp::INDEX: {
            int64_t array_val = getVal(0);
            int64_t index = getVal(1);
            int elem_width = 1;  // default to 1-bit
            if (node->type.has_value()) {
                elem_width = node->type->width;
            }
            uint64_t elem_mask = (elem_width >= 64) ? ~0ULL : (1ULL << elem_width) - 1;
            return static_cast<int64_t>((static_cast<uint64_t>(array_val) >> (index * elem_width)) & elem_mask);
        }

        case DFGOp::MODULE:
            throw CompilerError("Simulator: MODULE nodes not supported (no hierarchy support yet)", node);
    }

    throw CompilerError(std::format("Simulator: unhandled op {}", to_string(node->op)), node);
}

void Simulator::evaluateCombinational() {
    for (const DFGNode* node : topo_order_) {
        // Skip pre-set nodes
        if (node->op == DFGOp::INPUT || node->op == DFGOp::CONST) continue;
        // Skip flop .q nodes (they hold register values)
        if (flop_q_nodes_.count(node)) continue;

        int64_t val = evaluateNode(node);
        values_[node] = maskToWidth(val, node);
    }
}

// ============================================================================
// Sync input advancement
// ============================================================================

void Simulator::advanceSyncInputs() {
    for (auto& [name, pos] : sync_input_pos_) {
        if (pos + 1 < sync_input_data_[name].size()) {
            pos++;
        }
        // Set the value in the DFG input node
        auto it = module_.dfg->inputs.find(name);
        if (it != module_.dfg->inputs.end()) {
            values_[it->second] = sync_input_data_[name][pos];
        }
    }
}

// ============================================================================
// Output recording
// ============================================================================

void Simulator::recordOutputs() {
    // Record all non-async inputs
    /*
    for (const auto& [name, node] : module_.dfg->inputs) {
        if (!async_inputs_.count(name)) {
            recorded_values_[name].push_back(values_.at(node));
        }
    }
    */

    // Record all signals (except flop .q which are internal)
    /*
    for (const auto& [name, node] : module_.dfg->signals) {
        recorded_values_[name].push_back(values_.at(node));
    }
    */

    // Record all outputs
    for (const auto& [name, node] : module_.dfg->outputs) {
        recorded_values_[name].push_back(values_.at(node));
    }
}

void Simulator::writeOutputFiles() {
    std::filesystem::create_directories(config_.output_dir);

    for (const auto& [name, values] : recorded_values_) {
        std::string filepath = config_.output_dir + "/" + name + ".txt";
        std::ofstream out(filepath);
        if (!out.is_open()) {
            throw CompilerError(std::format(
                "Simulator: cannot open output file '{}'", filepath));
        }
        for (int64_t v : values) {
            out << v << "\n";
        }
    }
}

// ============================================================================
// VCD tracing
// ============================================================================

void Simulator::setupVcd(std::ofstream& vcd_out) {
    vcd_top_ = std::make_unique<vcd_tracer::top>(module_.name);

    // Helper: get the bit width from a DFG node's type
    auto getWidth = [](const DFGNode* node) -> unsigned int {
        if (node->type.has_value() && node->type->width > 0)
            return static_cast<unsigned int>(node->type->width);
        return 64;
    };

    // Helper: elaborate a value<uint64_t> into a module with the correct runtime bit width.
    // We wrap the module's add_fn to override the bit_size parameter that
    // value<uint64_t>::elaborate() hardcodes to 64.
    auto elaborateWithWidth = [](vcd_tracer::module& mod, vcd_tracer::value_base& var,
                                 std::string_view name, unsigned int width) {
        auto original_add_fn = mod.get_add_fn();
        auto width_fn = [original_add_fn, width](
                std::string_view var_name, std::string_view var_type,
                unsigned int /*bit_size*/, vcd_tracer::scope_fn::dumper_fn fn)
                -> vcd_tracer::value_context {
            return original_add_fn(var_name, var_type, width, fn);
        };
        var.elaborate(width_fn, name);
    };

    {
        vcd_tracer::module inputs_mod(vcd_top_->root, "inputs");
        vcd_tracer::module signals_mod(vcd_top_->root, "signals");
        vcd_tracer::module outputs_mod(vcd_top_->root, "outputs");

        for (const auto& [name, node] : module_.dfg->inputs) {
            auto v = std::make_unique<vcd_tracer::value<uint64_t>>();
            elaborateWithWidth(inputs_mod, *v, name, getWidth(node));
            vcd_values_[node] = std::move(v);
        }

        // Async inputs (clocks/resets) that don't have DFG input nodes
        for (const auto& name : async_inputs_) {
            if (module_.dfg->inputs.count(name)) continue;  // already added above
            auto v = std::make_unique<vcd_tracer::value<uint64_t>>();
            elaborateWithWidth(inputs_mod, *v, name, 1);
            vcd_async_values_[name] = std::move(v);
        }

        for (const auto& [name, node] : module_.dfg->signals) {
            auto v = std::make_unique<vcd_tracer::value<uint64_t>>();
            elaborateWithWidth(signals_mod, *v, name, getWidth(node));
            vcd_values_[node] = std::move(v);
        }

        for (const auto& [name, node] : module_.dfg->outputs) {
            auto v = std::make_unique<vcd_tracer::value<uint64_t>>();
            elaborateWithWidth(outputs_mod, *v, name, getWidth(node));
            vcd_values_[node] = std::move(v);
        }
    }

    vcd_top_->finalize_header(vcd_out,
                              std::chrono::system_clock::from_time_t(0));
}

void Simulator::updateVcdValues(std::ofstream& vcd_out, int64_t time_ns,
                                const std::map<std::string, int64_t>& async_values) {
    for (auto& [node, vcd_val] : vcd_values_) {
        vcd_val->set_uint64(static_cast<uint64_t>(values_.at(node)));
    }
    for (auto& [name, vcd_val] : vcd_async_values_) {
        auto it = async_values.find(name);
        if (it != async_values.end()) {
            vcd_val->set_uint64(static_cast<uint64_t>(it->second));
        }
    }
    vcd_top_->time_update_abs(vcd_out, std::chrono::nanoseconds{time_ns});
}

// ============================================================================
// Constructor
// ============================================================================

Simulator::Simulator(const ResolvedModule& module, const SimConfig& config)
    : module_(module), config_(config)
{
    if (!module_.dfg) {
        throw CompilerError("Simulator: module has no DFG");
    }

    // Check for MODULE nodes and width limits
    for (const auto& node : module_.dfg->nodes) {
        if (node->op == DFGOp::MODULE) {
            throw CompilerError("Simulator: MODULE nodes not supported (no hierarchy support yet)", node.get());
        }
        if (node->type.has_value() && node->type->width > 64) {
            throw CompilerError(std::format(
                "Simulator: node '{}' has width {} (max 64 supported)",
                node->str(), node->type->width), node.get());
        }
    }

    // Validate all module inputs have config entries
    for (const auto& [name, node] : module_.dfg->inputs) {
        if (config_.input_files.find(name) == config_.input_files.end()) {
            throw CompilerError(std::format(
                "Simulator: module input '{}' has no corresponding file in config", name));
        }
    }

    buildFlopMaps();
    buildTimeline();
    loadSyncInputs();
    buildTopology();
}

// ============================================================================
// Main simulation loop
// ============================================================================

void Simulator::run() {
    std::cout << "Simulator: starting simulation for module '" << module_.name << "'" << std::endl;

    // === VCD Setup ===
    std::filesystem::create_directories(config_.output_dir);
    std::string vcd_path = config_.output_dir + "/" + module_.name + ".vcd";
    std::ofstream vcd_out(vcd_path);
    if (!vcd_out.is_open()) {
        throw CompilerError(std::format(
            "Simulator: cannot open VCD output file '{}'", vcd_path));
    }
    setupVcd(vcd_out);

    // === Initialization (time 0) ===

    // 1. Set all CONST node values
    for (const auto& node : module_.dfg->nodes) {
        if (node->op == DFGOp::CONST) {
            values_[node.get()] = std::get<int64_t>(node->data);
        }
    }

    // 2. Set flop .q to random values (deterministic seed=42)
    std::mt19937_64 rng(42);
    for (const auto& [qnode, flop] : flop_q_nodes_) {
        int w = flop->type.type.width;
        uint64_t mask = (w == 64) ? ~0ULL : (1ULL << w) - 1;
        values_[qnode] = static_cast<int64_t>(rng() & mask);
    }

    // 3. Set async signal initial values to 0
    for (const auto& name : async_inputs_) {
        auto it = module_.dfg->inputs.find(name);
        if (it != module_.dfg->inputs.end()) {
            values_[it->second] = 0;
        }
    }

    // 4. Set sync input values from first line of their files
    for (const auto& [name, data] : sync_input_data_) {
        auto it = module_.dfg->inputs.find(name);
        if (it != module_.dfg->inputs.end()) {
            values_[it->second] = data[0];
        }
    }

    // 5. Evaluate all combinational logic
    evaluateCombinational();

    // Track async values for edge detection and VCD tracing
    std::map<std::string, int64_t> async_prev;
    for (const auto& name : async_inputs_) {
        async_prev[name] = 0;
    }

    // VCD: trace initial state at time 0
    updateVcdValues(vcd_out, 0, async_prev);

    std::cout << "Simulator: initialization complete, processing "
              << timeline_.size() << " async events" << std::endl;

    // Log outputs before first clock edge
    recordOutputs();

    // === Main loop: process timeline in time-batches ===

    size_t idx = 0;
    while (idx < timeline_.size()) {
        int64_t batch_time = timeline_[idx].time;

        // Collect all events at this time
        std::vector<const AsyncEvent*> batch;
        while (idx < timeline_.size() && timeline_[idx].time == batch_time) {
            batch.push_back(&timeline_[idx]);
            idx++;
        }

        // Apply new async values and detect edges
        std::map<std::string, int64_t> new_async;
        for (const auto* evt : batch) {
            new_async[evt->signal_name] = evt->value;
        }

        // Detect active edges
        bool any_clock_edge = false;
        bool any_reset_edge = false;
        std::set<std::string> active_clock_edges;
        std::set<std::string> active_reset_edges;

        for (const auto& [name, new_val] : new_async) {
            int64_t old_val = async_prev[name];

            // Update the input node value
            auto it = module_.dfg->inputs.find(name);
            if (it != module_.dfg->inputs.end()) {
                values_[it->second] = new_val;
            }

            // Check for edges
            bool posedge = (old_val == 0 && new_val == 1);
            bool negedge = (old_val == 1 && new_val == 0);

            // Is this a clock?
            if (flops_by_clock_.count(name)) {
                // Check edge type matches what the flops expect
                const auto& flops = flops_by_clock_[name];
                for (const auto* flop : flops) {
                    if ((flop->clock.edge == POSEDGE && posedge) ||
                        (flop->clock.edge == NEGEDGE && negedge)) {
                        active_clock_edges.insert(name);
                        any_clock_edge = true;
                    }
                }
            }

            // Is this a reset?
            if (flops_by_reset_.count(name)) {
                const auto& flops = flops_by_reset_[name];
                for (const auto* flop : flops) {
                    if ((flop->reset->edge == POSEDGE && posedge) ||
                        (flop->reset->edge == NEGEDGE && negedge)) {
                        active_reset_edges.insert(name);
                        any_reset_edge = true;
                    }
                }
            }

            async_prev[name] = new_val;
        }

        // 3. On reset active edge: set affected flop .q = reset_value
        if (any_reset_edge) {
            for (const auto& rst_name : active_reset_edges) {
                for (const auto* flop : flops_by_reset_[rst_name]) {
                    std::string qname = flop->name + ".q";
                    auto qit = module_.dfg->signals.find(qname);
                    if (qit != module_.dfg->signals.end() && flop->reset_value.has_value()) {
                        values_[qit->second] = flop->reset_value.value();
                    }
                }
            }
        }

        // 4. On clock active edge: set affected flop .q = .d value, advance sync inputs
        if (any_clock_edge) {
            for (const auto& clk_name : active_clock_edges) {
                for (const auto* flop : flops_by_clock_[clk_name]) {
                    std::string qname = flop->name + ".q";
                    std::string dname = flop->name + ".d";
                    auto qit = module_.dfg->signals.find(qname);
                    auto dit = module_.dfg->signals.find(dname);
                    if (qit != module_.dfg->signals.end() && dit != module_.dfg->signals.end()) {
                        values_[qit->second] = values_.at(dit->second);
                    }
                }
            }
            advanceSyncInputs();
        }

        // 5. Re-evaluate combinational logic
        evaluateCombinational();

        // 6. VCD: trace all values at every time step
        updateVcdValues(vcd_out, batch_time, async_prev);

        // 7. Record output values only on clock edges (for text output)
        if (any_clock_edge) {
            recordOutputs();
        }
    }

    // Finalize VCD trace
    vcd_top_->finalize_trace(vcd_out);
    vcd_out.close();

    // Write text output files
    writeOutputFiles();

    std::cout << "Simulator: simulation complete. "
              << recorded_values_.begin()->second.size() << " cycles recorded."
              << std::endl;
    std::cout << "Simulator: output written to '" << config_.output_dir << "/'" << std::endl;
    std::cout << "Simulator: VCD trace written to '" << vcd_path << "'" << std::endl;
}

} // namespace custom_hdl
