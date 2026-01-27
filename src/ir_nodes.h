#pragma once

#include "types.h"
#include <memory>
#include <vector>

namespace custom_hdl {

// Base class for all IR nodes
struct IRNode {
    virtual ~IRNode() = default;
    virtual void print(int indent = 0) const = 0;
};

// Module definition
struct IRModule : IRNode {
    std::string name;
    std::vector<SignalInfo> parameters;
    std::vector<SignalInfo> inputs;
    std::vector<SignalInfo> outputs;
    std::vector<SignalInfo> signals;
    std::vector<SignalInfo> flops;
    std::vector<std::unique_ptr<IRNode>> body;

    void print(int indent = 0) const override;
};

// Always block (procedural logic)
struct IRAlways : IRNode {
    std::string sensitivity;  // e.g., "posedge clk"
    std::unique_ptr<IRNode> body;

    void print(int indent = 0) const override;
};

// Variable declaration
struct IRVariable : IRNode {
    std::string name;
    std::string type;  // e.g., "reg", "wire"
    int width;         // bit width

    void print(int indent = 0) const override;
};

// Assignment statement
struct IRAssignment : IRNode {
    std::string target;
    std::string value;  // Simplified for now
    bool blocking;      // true for =, false for <=

    void print(int indent = 0) const override;
};

// If statement
struct IRIf : IRNode {
    std::string condition;
    std::unique_ptr<IRNode> thenBranch;
    std::unique_ptr<IRNode> elseBranch;

    void print(int indent = 0) const override;
};

// Statement block
struct IRBlock : IRNode {
    std::vector<std::unique_ptr<IRNode>> statements;

    void print(int indent = 0) const override;
};

} // namespace custom_hdl
