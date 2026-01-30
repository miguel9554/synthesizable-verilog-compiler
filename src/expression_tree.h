#pragma once

#include <memory>
#include <string>

namespace custom_hdl {

// Forward declaration to break circular dependency
struct ResolvedSignal;

    // TODO should change double...

    class ExprNode {
    public:
            virtual double evaluate() const = 0;
            virtual ~ExprNode() = default;
    };

    class LiteralNode : public ExprNode{
        double value;
    public:
        explicit LiteralNode(double v): value(v) {}
        double evaluate() const override { return value; }
    };

    class ReferenceNode : public ExprNode{
        ResolvedSignal * reference;
    public:
        explicit ReferenceNode(ResolvedSignal * ref): reference(ref) {}
        double evaluate() const override;
    };

    enum class UnaryOp { NEGATE, PLUS };

    class UnaryNode : public ExprNode {
        std::unique_ptr<ExprNode> operand;
        UnaryOp op;
    public:
        UnaryNode(std::unique_ptr<ExprNode> operand, UnaryOp op)
            : operand(std::move(operand)), op(op) {}
        double evaluate() const override {
            double val = operand->evaluate();
            switch (op) {
                case UnaryOp::NEGATE: return -val;
                case UnaryOp::PLUS: return val;
            }
            return val;
        }
    };

    // Reference by name (for signals not yet resolved to pointers)
    class NamedReferenceNode : public ExprNode {
        std::string name;
    public:
        explicit NamedReferenceNode(std::string n) : name(std::move(n)) {}
        const std::string& getName() const { return name; }
        double evaluate() const override {
            // Cannot evaluate without resolved reference
            return 0.0;
        }
    };

    enum class BinaryOp { SUM, MINUS, MULTIPLY, DIVIDE };

    class BinaryNode : public ExprNode {
        std::unique_ptr<ExprNode> left;
        std::unique_ptr<ExprNode> right;
        BinaryOp op;
    public:
        BinaryNode(std::unique_ptr<ExprNode> l, std::unique_ptr<ExprNode> r, BinaryOp op)
            : left(std::move(l)), right(std::move(r)), op(op) {}
        double evaluate() const override {
            const double leftResult = left->evaluate();
            const double rightResult = right->evaluate();
            switch (op) {
                case BinaryOp::SUM: return leftResult + rightResult;
                case BinaryOp::MINUS: return leftResult - rightResult;
                case BinaryOp::MULTIPLY: return leftResult * rightResult;
                case BinaryOp::DIVIDE: return leftResult / rightResult;
            }
            return 0.0;
        }
    };

} // namespace custom_hdl
