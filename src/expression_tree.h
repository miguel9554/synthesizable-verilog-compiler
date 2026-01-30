#pragma once

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

    typedef enum { SUM, MINUS } BINARY_OP;

    class BinaryNode: public ExprNode{
        ExprNode * left;
        ExprNode * right;
        BINARY_OP op;
    public:
        explicit BinaryNode(ExprNode * l, ExprNode * r, BINARY_OP op): left(l), right(r), op(op) {}
        double evaluate() const override {
            const double leftResult = left->evaluate();
            const double rightResult = right->evaluate();
            switch (op){
                case SUM:{
                    return leftResult+rightResult;
                }
                case MINUS:{
                    return leftResult-rightResult;
                }
            }
        }
    };

} // namespace custom_hdl
