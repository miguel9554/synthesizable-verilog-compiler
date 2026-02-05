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

    enum class UnaryOp {
        PLUS,
        NEGATE,
        BITWISE_AND,      // reduction &
        BITWISE_NAND,     // reduction ~&
        BITWISE_OR,       // reduction |
        BITWISE_NOR,      // reduction ~|
        BITWISE_XOR,      // reduction ^
        BITWISE_XNOR,     // reduction ~^ or ^~
        LOGICAL_NOT,      // !x
        BITWISE_NOT       // ~x
    };

    class UnaryNode : public ExprNode {
    public:
        std::unique_ptr<ExprNode> operand;
        UnaryOp op;
        UnaryNode(std::unique_ptr<ExprNode> operand, UnaryOp op)
            : operand(std::move(operand)), op(op) {}
        double evaluate() const override {
            double val = operand->evaluate();
            int64_t ival = static_cast<int64_t>(val);
            switch (op) {
                case UnaryOp::PLUS: return val;
                case UnaryOp::NEGATE: return -val;
                case UnaryOp::BITWISE_NOT: return static_cast<double>(~ival);
                case UnaryOp::LOGICAL_NOT: return (ival == 0) ? 1.0 : 0.0;
                // Reduction ops - simplified evaluation (not bit-accurate)
                case UnaryOp::BITWISE_AND: return (ival != 0) ? 1.0 : 0.0;
                case UnaryOp::BITWISE_NAND: return (ival != 0) ? 0.0 : 1.0;
                case UnaryOp::BITWISE_OR: return (ival != 0) ? 1.0 : 0.0;
                case UnaryOp::BITWISE_NOR: return (ival != 0) ? 0.0 : 1.0;
                case UnaryOp::BITWISE_XOR: return static_cast<double>(__builtin_parityll(ival));
                case UnaryOp::BITWISE_XNOR: return static_cast<double>(!__builtin_parityll(ival));
            }
            return val;
        }
        std::string toJson(int indent = 0) const override {
            const char* opStr;
            switch (op) {
                case UnaryOp::PLUS: opStr = "plus"; break;
                case UnaryOp::NEGATE: opStr = "negate"; break;
                case UnaryOp::BITWISE_AND: opStr = "reduction_and"; break;
                case UnaryOp::BITWISE_NAND: opStr = "reduction_nand"; break;
                case UnaryOp::BITWISE_OR: opStr = "reduction_or"; break;
                case UnaryOp::BITWISE_NOR: opStr = "reduction_nor"; break;
                case UnaryOp::BITWISE_XOR: opStr = "reduction_xor"; break;
                case UnaryOp::BITWISE_XNOR: opStr = "reduction_xnor"; break;
                case UnaryOp::LOGICAL_NOT: opStr = "logical_not"; break;
                case UnaryOp::BITWISE_NOT: opStr = "bitwise_not"; break;
            }
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

    enum class BinaryOp { SUM, MINUS, MULTIPLY, DIVIDE, EQ, LT, LE, GT, GE };

    class BinaryNode : public ExprNode {
    public:
        std::unique_ptr<ExprNode> left;
        std::unique_ptr<ExprNode> right;
        BinaryOp op;
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
                case BinaryOp::EQ: return leftResult == rightResult ? 1.0 : 0.0;
                case BinaryOp::LT: return leftResult < rightResult ? 1.0 : 0.0;
                case BinaryOp::LE: return leftResult <= rightResult ? 1.0 : 0.0;
                case BinaryOp::GT: return leftResult > rightResult ? 1.0 : 0.0;
                case BinaryOp::GE: return leftResult >= rightResult ? 1.0 : 0.0;
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
                case BinaryOp::EQ: opStr = "eq"; break;
                case BinaryOp::LT: opStr = "lt"; break;
                case BinaryOp::LE: opStr = "le"; break;
                case BinaryOp::GT: opStr = "gt"; break;
                case BinaryOp::GE: opStr = "ge"; break;
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
