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
            virtual std::string toJson(int indent = 0) const = 0;
            virtual ~ExprNode() = default;
    protected:
            static std::string indentStr(int n) { return std::string(n * 2, ' '); }
    };

    class LiteralNode : public ExprNode{
        double value;
    public:
        explicit LiteralNode(double v): value(v) {}
        double evaluate() const override { return value; }
        std::string toJson(int indent = 0) const override {
            return indentStr(indent) + R"({"type": "Literal", "value": )" + std::to_string(value) + "}";
        }
    };

    class ReferenceNode : public ExprNode{
        ResolvedSignal * reference;
    public:
        explicit ReferenceNode(ResolvedSignal * ref): reference(ref) {}
        double evaluate() const override;
        std::string toJson(int indent = 0) const override {
            return indentStr(indent) + R"({"type": "Reference", "resolved": true})";
        }
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
        std::string toJson(int indent = 0) const override {
            std::string opStr = (op == UnaryOp::NEGATE) ? "negate" : "plus";
            return indentStr(indent) + "{\n" +
                   indentStr(indent + 1) + R"("type": "Unary",)" + "\n" +
                   indentStr(indent + 1) + R"("op": ")" + opStr + "\",\n" +
                   indentStr(indent + 1) + "\"operand\":\n" + operand->toJson(indent + 1) + "\n" +
                   indentStr(indent) + "}";
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
        std::string toJson(int indent = 0) const override {
            return indentStr(indent) + R"({"type": "NamedReference", "name": ")" + name + R"("})";
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
        std::string toJson(int indent = 0) const override {
            const char* opStr;
            switch (op) {
                case BinaryOp::SUM: opStr = "add"; break;
                case BinaryOp::MINUS: opStr = "subtract"; break;
                case BinaryOp::MULTIPLY: opStr = "multiply"; break;
                case BinaryOp::DIVIDE: opStr = "divide"; break;
            }
            return indentStr(indent) + "{\n" +
                   indentStr(indent + 1) + R"("type": "Binary",)" + "\n" +
                   indentStr(indent + 1) + R"("op": ")" + opStr + "\",\n" +
                   indentStr(indent + 1) + "\"left\":\n" + left->toJson(indent + 1) + ",\n" +
                   indentStr(indent + 1) + "\"right\":\n" + right->toJson(indent + 1) + "\n" +
                   indentStr(indent) + "}";
        }
    };

} // namespace custom_hdl
