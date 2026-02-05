#include "resolver.h"
#include "dfg.h"
#include "expression_tree.h"
#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxKind.h"
#include "slang/syntax/SyntaxNode.h"
#include "types.h"

#include <algorithm>
#include <atomic>
#include <iostream>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <vector>

using namespace slang::syntax;

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

void ResolvedSignal::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
    for (const auto& dim : dimensions) {
        os << "[" << dim.left << ":" << dim.right << "]";
    }
}

void ResolvedParam::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
    for (const auto& dim : dimensions) {
        os << "[" << dim.left << ":" << dim.right << "]";
    }
    os << " = " << value;
}


// ============================================================================
// Resolution functions (STUB implementations)
// ============================================================================

namespace {

// Forward declaration
ResolvedTypes::ProceduralCombo resolveStatement(
        const slang::syntax::StatementSyntax* statement,
        std::unique_ptr<DFG> graph,
        bool is_sequential
);


// Evaluate a constant expression given a parameter context
// Throws if a referenced parameter is not in the context
int64_t evaluateConstantExpr(const ExpressionSyntax* expr, const ParameterContext& ctx) {
    if (!expr) {
        throw std::runtime_error("Cannot evaluate null expression");
    }

    switch (expr->kind) {
        case SyntaxKind::IntegerLiteralExpression: {
            auto& literal = expr->as<LiteralExpressionSyntax>();
            // Parse the integer literal token
            auto text = literal.literal.rawText();
            // Handle simple decimal integers for now
            // TODO: handle other bases (hex, octal, binary) and sized literals
            return std::stoll(std::string(text));
        }

        case SyntaxKind::IdentifierName: {
            auto& name = expr->as<IdentifierNameSyntax>();
            std::string paramName(name.identifier.valueText());
            auto it = ctx.values.find(paramName);
            if (it == ctx.values.end()) {
                throw std::runtime_error(
                    "Parameter '" + paramName + "' not found in context");
            }
            return it->second;
        }

        case SyntaxKind::ParenthesizedExpression: {
            auto& paren = expr->as<ParenthesizedExpressionSyntax>();
            return evaluateConstantExpr(paren.expression, ctx);
        }

        case SyntaxKind::UnaryPlusExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return evaluateConstantExpr(unary.operand, ctx);
        }

        case SyntaxKind::UnaryMinusExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return -evaluateConstantExpr(unary.operand, ctx);
        }

        case SyntaxKind::AddExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return evaluateConstantExpr(binary.left, ctx) +
                   evaluateConstantExpr(binary.right, ctx);
        }

        case SyntaxKind::SubtractExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return evaluateConstantExpr(binary.left, ctx) -
                   evaluateConstantExpr(binary.right, ctx);
        }

        case SyntaxKind::MultiplyExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return evaluateConstantExpr(binary.left, ctx) *
                   evaluateConstantExpr(binary.right, ctx);
        }

        case SyntaxKind::DivideExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            auto divisor = evaluateConstantExpr(binary.right, ctx);
            if (divisor == 0) {
                throw std::runtime_error("Division by zero in constant expression");
            }
            return evaluateConstantExpr(binary.left, ctx) / divisor;
        }

        default:
            throw std::runtime_error(
                "Unsupported expression kind in constant evaluation: " +
                std::string(toString(expr->kind)));
    }
}

// IntegerType
// KeywordType
// NamedType
// StructUnionType
// EnumType
// TypeReference
// VirtualInterfaceType
// ImplicitType

// Resolve an UnresolvedParam to ResolvedParam
// TODO: Actually evaluate the type syntax and dimension expressions
ResolvedParam resolveParameter(const UnresolvedParam& param, const ParameterContext& topCtx, ParameterContext& localCtx) {
    ResolvedParam resolved;
    resolved.name = param.name;

    // TODO: currently only support for implicit type
    if (param.type.syntax->isKind(SyntaxKind::ImplicitType)){
        resolved.type = ResolvedType::makeInteger(32, false);
    } else{
        throw std::runtime_error("Only implicit param type supported");
    }

    // TODO only support for scalar params
    if (param.dimensions.syntax) {
        throw std::runtime_error("Param with dimensions not supported.");
    }

    // First check if param value is provided in context (override)
    auto itTop = topCtx.values.find(param.name);
    auto itLocal = localCtx.values.find(param.name);
    if (itTop != topCtx.values.end()) {
        resolved.value = itTop->second;
    } else if (itLocal != localCtx.values.end()) {
        resolved.value = itLocal->second;
    } else if (param.defaultValue) {
        // Evaluate the default value expression
        // First merge contexts
        ParameterContext mergedCtx = topCtx;
        for (const auto& [k, v] : localCtx.values) {
            mergedCtx.values[k] = v;
        }

        resolved.value = evaluateConstantExpr(param.defaultValue, mergedCtx);
        localCtx.values[param.name] = resolved.value;
    } else {
        std::ostringstream oss;

        oss << "Parameter '" << param.name
            << "' has no value in context and no default value.\n\n";

        oss << "Top context values:\n";
        for (const auto& [k, v] : topCtx.values) {
            oss << "  '" << k << "' -> '" << v << "'\n";
        }

        oss << "\nLocal context values:\n";
        for (const auto& [k, v] : localCtx.values) {
            oss << "  '" << k << "' -> '" << v << "'\n";
        }

        throw std::runtime_error(oss.str());
    }

    return resolved;
}

// IntegerType
// KeywordType
// NamedType
// StructUnionType
// EnumType
// TypeReference
// VirtualInterfaceType
// ImplicitType

std::vector<ResolvedDimension> ResolveDimensions(
        const SyntaxList<VariableDimensionSyntax>& dimensionsSyntaxList,
        const ParameterContext& ctx){
    std::vector<ResolvedDimension> resolvedDimensions;
    // Parse dimensionsSyntax from syntax
    if (!dimensionsSyntaxList.empty()) {
        for (const auto* dimSyntax : dimensionsSyntaxList) {
            if (!dimSyntax->specifier) {
                throw std::runtime_error("Dimension specifier is null");
            }

            if (!dimSyntax->specifier->isKind(SyntaxKind::RangeDimensionSpecifier)) {
                throw std::runtime_error(
                    "Only range dimension specifier supported, got: " +
                    std::string(toString(dimSyntax->specifier->kind)));
            }

            auto& rangeSpec = dimSyntax->specifier->as<RangeDimensionSpecifierSyntax>();

            if (!rangeSpec.selector->isKind(SyntaxKind::SimpleRangeSelect)) {
                throw std::runtime_error(
                    "Only simple range select supported, got: " +
                    std::string(toString(rangeSpec.selector->kind)));
            }

            auto& rangeSelect = rangeSpec.selector->as<RangeSelectSyntax>();

            int64_t left = evaluateConstantExpr(rangeSelect.left, ctx);
            int64_t right = evaluateConstantExpr(rangeSelect.right, ctx);

            resolvedDimensions.push_back(ResolvedDimension{
                .left = static_cast<int>(left),
                .right = static_cast<int>(right)
            });
        }
    } else {
        // No dimension syntax - default to single bit [0:0]
        resolvedDimensions.push_back(ResolvedDimension{.left = 0, .right = 0});
    }
    return resolvedDimensions;
}

// Resolve type and dimensions from syntax
// Populates the dimensions vector and returns the ResolvedType with computed width
ResolvedType resolveType(
    const DataTypeSyntax& syntax,
    // const UnresolvedDimension& dimension,
    const ParameterContext& ctx)
{

    SyntaxList<VariableDimensionSyntax> packedDimensionsSyntax = nullptr;

    bool is_signed;

    switch (syntax.kind){
        case SyntaxKind::ImplicitType: {
            packedDimensionsSyntax = (syntax.as<ImplicitTypeSyntax>()).dimensions;
            is_signed = false;
            break;
        }
        case SyntaxKind::LogicType: {
            packedDimensionsSyntax = (syntax.as<IntegerTypeSyntax>()).dimensions;
            is_signed = (syntax.as<IntegerTypeSyntax>()).signing.rawText() == "signed";
            break;
        }
        case SyntaxKind::RegType: {
            packedDimensionsSyntax = (syntax.as<IntegerTypeSyntax>()).dimensions;
            is_signed = (syntax.as<IntegerTypeSyntax>()).signing.rawText() == "signed";
            break;
        }
        default:
            throw std::runtime_error(
                "Unsupported type: " +
                std::string(toString(syntax.kind)));
    }


    // TODO we could store this in the struct also if needed
    const auto packedDimensions = ResolveDimensions(packedDimensionsSyntax, ctx);

    // Compute total width as product of all dimension sizes
    int width = 1;
    for (const auto& dim : packedDimensions) {
        width *= dim.size();
    }

    return ResolvedType::makeInteger(width, is_signed);
}

std::unique_ptr<ExprNode> buildExprTree(const ExpressionSyntax* expr) {
    if (!expr) {
        throw std::runtime_error("Cannot build expression tree from null expression");
    }

    switch (expr->kind) {
        case SyntaxKind::IntegerLiteralExpression: {
            auto& literal = expr->as<LiteralExpressionSyntax>();
            auto text = literal.literal.rawText();
            double value = std::stod(std::string(text));
            return std::make_unique<LiteralNode>(value);
        }

        case SyntaxKind::IntegerVectorExpression: {
            auto& vecExpr = expr->as<IntegerVectorExpressionSyntax>();
            // size is optional (e.g., 'hFF vs 8'hFF)
            // base is required (e.g., 'h, 'b, 'd, 'o)
            // value is the actual digits

            std::string sizeText(vecExpr.size.rawText());
            std::string baseText(vecExpr.base.rawText());
            std::string valueText(vecExpr.value.rawText());
            std::string literal = sizeText + baseText + valueText;

            // Remove underscores from value (Verilog allows 8'hFF_FF)
            valueText.erase(std::remove(valueText.begin(), valueText.end(), '_'), valueText.end());

            int base = 10;
            if (baseText.find('h') != std::string::npos || baseText.find('H') != std::string::npos) {
                base = 16;
            } else if (baseText.find('b') != std::string::npos || baseText.find('B') != std::string::npos) {
                base = 2;
            } else if (baseText.find('o') != std::string::npos || baseText.find('O') != std::string::npos) {
                base = 8;
            } else if (baseText.find('d') != std::string::npos || baseText.find('D') != std::string::npos) {
                base = 10;
            }

            int64_t value = std::stoll(valueText, nullptr, base);
            std::cout << "IntegerVectorExpression: " << literal << " -> " << value << std::endl;
            return std::make_unique<LiteralNode>(static_cast<double>(value));
        }

        case SyntaxKind::IdentifierName: {
            auto& name = expr->as<IdentifierNameSyntax>();
            std::string signalName(name.identifier.valueText());
            return std::make_unique<NamedReferenceNode>(signalName);
        }

        case SyntaxKind::ParenthesizedExpression: {
            auto& paren = expr->as<ParenthesizedExpressionSyntax>();
            return buildExprTree(paren.expression);
        }

        case SyntaxKind::UnaryPlusExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::PLUS);
        }

        case SyntaxKind::UnaryMinusExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::NEGATE);
        }

        case SyntaxKind::UnaryBitwiseAndExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::BITWISE_AND);
        }

        case SyntaxKind::UnaryBitwiseNandExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::BITWISE_NAND);
        }

        case SyntaxKind::UnaryBitwiseOrExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::BITWISE_OR);
        }

        case SyntaxKind::UnaryBitwiseNorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::BITWISE_NOR);
        }

        case SyntaxKind::UnaryBitwiseXorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::BITWISE_XOR);
        }

        case SyntaxKind::UnaryBitwiseXnorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::BITWISE_XNOR);
        }

        case SyntaxKind::UnaryLogicalNotExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::LOGICAL_NOT);
        }

        case SyntaxKind::UnaryBitwiseNotExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return std::make_unique<UnaryNode>(buildExprTree(unary.operand), UnaryOp::BITWISE_NOT);
        }

        case SyntaxKind::AddExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::SUM);
        }

        case SyntaxKind::SubtractExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::MINUS);
        }

        case SyntaxKind::MultiplyExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::MULTIPLY);
        }

        case SyntaxKind::DivideExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::DIVIDE);
        }

        case SyntaxKind::EqualityExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::EQ);
        }

        case SyntaxKind::LessThanExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::LT);
        }

        case SyntaxKind::LessThanEqualExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::LE);
        }

        case SyntaxKind::GreaterThanExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::GT);
        }

        case SyntaxKind::GreaterThanEqualExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return std::make_unique<BinaryNode>(
                buildExprTree(binary.left),
                buildExprTree(binary.right),
                BinaryOp::GE);
        }

        default:
            throw std::runtime_error(
                "Unsupported expression kind in expression tree: " +
                std::string(toString(expr->kind)));
    }
}

std::vector<ResolvedTypes::Assign> resolveAssign(
        const ContinuousAssignSyntax* syntax,
        const ParameterContext& /*ctx*/
    ){
    if (!syntax) throw std::runtime_error("Null pointer");
    if (syntax->strength) throw std::runtime_error("Strength statement not valid.");
    if (syntax->delay) throw std::runtime_error("Delay statement not valid.");

    std::vector<ResolvedTypes::Assign> result;

    for (const auto* assignExpr : syntax->assignments) {
        if (!assignExpr->isKind(SyntaxKind::AssignmentExpression)) {
            throw std::runtime_error(
                "Expected assignment expression, got: " +
                std::string(toString(assignExpr->kind)));
        }

        auto& binaryAssign = assignExpr->as<BinaryExpressionSyntax>();
        result.push_back(buildExprTree(binaryAssign.right));
    }

    return result;
}

DFGNode* exprTreeToDFGNode(DFG& graph, const ExprNode* node){
    if (auto* lit = dynamic_cast<const LiteralNode*>(node)){
        return graph.constant(static_cast<int64_t>(lit->evaluate()));
    }
    else if (auto* namedRef = dynamic_cast<const NamedReferenceNode*>(node)){
        // If it's an input, return the node
        // If it's an output, also?
        // "internal" nodes to the DFG are not allowed *yet*.
        // They would require parsing an internal signal declaration.
        const auto name = namedRef->getName();
        if (graph.inputs.contains(name)) return graph.inputs[name];
        if (graph.outputs.contains(name)) return graph.outputs[name]->in[0];
        // TODO should we always create the input?
        return graph.input(name);
    }
    else if (auto* binary = dynamic_cast<const BinaryNode*>(node)){
        auto* left = exprTreeToDFGNode(graph, binary->left.get());
        auto* right = exprTreeToDFGNode(graph, binary->right.get());
        switch(binary->op) {
            case BinaryOp::SUM:
                return graph.add(left, right);
            case BinaryOp::MINUS:
                return graph.sub(left, right);
            case BinaryOp::MULTIPLY:
                return graph.mul(left, right);
            case BinaryOp::DIVIDE:
                throw std::runtime_error("DIV operation not yet supported in DFG");
            case BinaryOp::EQ:
                return graph.eq(left, right);
            case BinaryOp::LT:
                return graph.lt(left, right);
            case BinaryOp::LE:
                return graph.le(left, right);
            case BinaryOp::GT:
                return graph.gt(left, right);
            case BinaryOp::GE:
                return graph.ge(left, right);
        }
        throw std::runtime_error("Unknown BinaryOp");
    }
    else if (auto* unary = dynamic_cast<const UnaryNode*>(node)){
        auto* operand = exprTreeToDFGNode(graph, unary->operand.get());
        switch(unary->op) {
            case UnaryOp::PLUS:
                return graph.unaryPlus(operand);
            case UnaryOp::NEGATE:
                return graph.unaryNegate(operand);
            case UnaryOp::BITWISE_NOT:
                return graph.bitwiseNot(operand);
            case UnaryOp::LOGICAL_NOT:
                return graph.logicalNot(operand);
            case UnaryOp::BITWISE_AND:
                return graph.reductionAnd(operand);
            case UnaryOp::BITWISE_NAND:
                return graph.reductionNand(operand);
            case UnaryOp::BITWISE_OR:
                return graph.reductionOr(operand);
            case UnaryOp::BITWISE_NOR:
                return graph.reductionNor(operand);
            case UnaryOp::BITWISE_XOR:
                return graph.reductionXor(operand);
            case UnaryOp::BITWISE_XNOR:
                return graph.reductionXnor(operand);
        }
        throw std::runtime_error("Unknown UnaryOp");
    }
    else {
        // Catches ReferenceNode and any other unsupported types
        throw std::runtime_error("Unsupported ExprNode type in DFG conversion");
    }
}

std::unique_ptr<DFG> exprTreeToDFG(const ExprNode* node, const std::string name, std::unique_ptr<DFG> graph){
    if (!graph){
        graph = std::make_unique<DFG>();
    }
    auto outputNode = exprTreeToDFGNode(*graph, node);
    graph->output(outputNode, name, true);
    return graph;
}


std::unique_ptr<DFG> resolveExpressionStatement(
        const ExpressionStatementSyntax* exprStatement,
        std::unique_ptr<DFG> graph,
        bool is_sequential){
    auto& expr = exprStatement->expr;
    const auto expectedKind = is_sequential ? SyntaxKind::NonblockingAssignmentExpression :
                                              SyntaxKind::AssignmentExpression;
    if (expr->kind != expectedKind){
        throw std::runtime_error(
        "Can only process assign expression. Current: " + std::string(toString(expr->kind)));
    }
    const auto& assignExpr = expr->as<slang::syntax::BinaryExpressionSyntax>();
    const auto& left = assignExpr.left;
    const auto& right = assignExpr.right;
    if (left->kind != SyntaxKind::IdentifierName) {
        throw std::runtime_error(
        "Left can only be variable name: " + std::string(toString(left->kind)));
    }
    const auto& identifier = left->as<slang::syntax::IdentifierNameSyntax>();
    const std::string name(identifier.identifier.valueText());
    const auto exprTree = buildExprTree(right);
    return exprTreeToDFG(exprTree.get(), name, std::move(graph));
}

std::unique_ptr<DFG> resolveConditionalStatement(
        const ConditionalStatementSyntax* conditionalStatement,
        std::unique_ptr<DFG> graph,
        bool is_sequential){
    const auto& predicate = conditionalStatement->predicate;
    if (conditionalStatement->uniqueOrPriority){
        throw std::runtime_error("Unique/priority not supported on if");
    }
    if (predicate->conditions.size()>1){
        throw std::runtime_error("Support for single predicate on if");
    }
    const auto& predicateExpr = predicate->conditions[0]->expr;

    // construct the signal node for the predicate expression
    auto predicateExprTree = buildExprTree(predicateExpr);
    auto conditionNode = exprTreeToDFGNode(*graph, predicateExprTree.get());

    // Extract driver nodes from graph outputs
    // (output nodes get modified in place when replaced, so we save the actual drivers)
    auto getDrivers = [](const DFG& g) {
        std::unordered_map<std::string, DFGNode*> drivers;
        for (const auto& [outName, outNode] : g.outputs) {
            if (!outNode->in.empty()) {
                drivers[outName] = outNode->in[0];
            }
        }
        return drivers;
    };

    const auto oldDrivers = getDrivers(*graph);

    if (conditionalStatement->elseClause) {
        // TODO should check if this is a statement.
        const auto& elseClause = conditionalStatement->elseClause->clause;
        const auto& elseStatement = elseClause->as<StatementSyntax>();
        graph = resolveStatement(&elseStatement, std::move(graph), is_sequential);
    }

    const auto elseDrivers = getDrivers(*graph);

    graph = resolveStatement(conditionalStatement->statement, std::move(graph), is_sequential);

    // Assign MUXes for signals that ARE assigned on IF branch
    for (const auto& [outName, outNode] : graph->outputs) {
        DFGNode* oldDriver;
        DFGNode* newDriver = outNode->in[0];

        // First, check if signal is asisgned in ELSE clause
        auto it = elseDrivers.find(outName);
        if (it != elseDrivers.end() && !outNode->in.empty()) {
            oldDriver = it->second;
        } else {
            auto it = oldDrivers.find(outName);
            if (it != oldDrivers.end() && !outNode->in.empty()) {
                oldDriver = it->second;
            } else {
            throw std::runtime_error(
                "Signal is not assigned in IF branch but not other: " + outName);
            }
        }

        if (oldDriver != newDriver) {
            // Add the mux!
            const auto& muxOut = graph->mux(conditionNode, newDriver, oldDriver);
            graph->output(muxOut, outName, true);
        }
    }

    // Handle signals that are assigned in ELSE branch but NOT in IF branch
    for (const auto& [elseName, elseDriver] : elseDrivers) {
        auto oldIt = oldDrivers.find(elseName);
        DFGNode* oldDriver = (oldIt != oldDrivers.end()) ? oldIt->second : nullptr;

        // Skip if ELSE didn't actually modify this signal
        if (elseDriver == oldDriver) {
            continue;
        }

        // Check if IF modified this signal
        auto ifIt = graph->outputs.find(elseName);
        bool if_modified = (ifIt != graph->outputs.end() && !ifIt->second->in.empty()
                            && ifIt->second->in[0] != elseDriver);

        // IF already handled this signal in the loop above
        if (if_modified) {
            continue;
        }

        // Signal is only in ELSE, not in OLD and not in IF → error
        if (!oldDriver) {
            throw std::runtime_error(
                "Signal '" + elseName + "' is only assigned in ELSE branch, not supported");
        }

        // Signal is in ELSE and OLD but not IF → add MUX
        // TRUE (condition true, IF branch): use oldDriver (IF didn't change it)
        // FALSE (condition false, ELSE branch): use elseDriver
        auto muxOut = graph->mux(conditionNode, oldDriver, elseDriver);
        graph->output(muxOut, elseName, true);
    }

    // TODO this graph MAY contain dangling/orphan nodes.
    return graph;
}

ResolvedTypes::ProceduralCombo resolveSequentialBlockStatement(
        const slang::syntax::BlockStatementSyntax* seqStatement,
        std::unique_ptr<DFG> graph,
        bool is_sequential
){
    for (const auto* item: seqStatement->items){
        // TODO should catch if this fails...
        const auto& statement = item->as<StatementSyntax>();
        graph = resolveStatement(&statement, std::move(graph), is_sequential);
    }
    return graph;
}

ResolvedTypes::ProceduralCombo resolveStatement(
        const slang::syntax::StatementSyntax* statement,
        std::unique_ptr<DFG> graph,
        bool is_sequential
){
    switch (statement->kind){
        case SyntaxKind::SequentialBlockStatement:{
            const auto& seqStatement = statement->as<BlockStatementSyntax>();
            graph = resolveSequentialBlockStatement(&seqStatement, std::move(graph), is_sequential);
            break;
        }
        case SyntaxKind::ExpressionStatement:{
            const auto& exprStatement = statement->as<ExpressionStatementSyntax>();
            graph = resolveExpressionStatement(&exprStatement, std::move(graph), is_sequential);
            break;
        }
        case SyntaxKind::ConditionalStatement:{
            const auto& conditionalStatement = statement->as<ConditionalStatementSyntax>();
            graph = resolveConditionalStatement(&conditionalStatement, std::move(graph), is_sequential);
            break;
          }

        default:
            throw std::runtime_error(
                "We expect all statements to be expressions. Current: " + std::string(toString(statement->kind)));
    }
    return graph;
}

ResolvedTypes::ProceduralCombo resolveProceduralCombo(
        const UnresolvedTypes::ProceduralCombo& statement,
        const ParameterContext& /*ctx*/
){
    if (statement->kind != SyntaxKind::SequentialBlockStatement){
        throw std::runtime_error(
        "Statement not synthesizable: " + std::string(toString(statement->kind)));
    }
    auto graph = std::make_unique<DFG>();
    graph = resolveStatement(statement, std::move(graph), false);
    return graph;
}

std::string make_id() {
    static std::atomic<uint64_t> counter{0};
    return "id_" + std::to_string(++counter);
}

typedef enum {
    POSEDGE, NEGEDGE
} edge_t;

typedef struct {
    edge_t edge;
    std::string name;
} asyncTrigger_t;

const char* edgeToStr(edge_t e) {
    switch (e) {
        case POSEDGE: return "POSEDGE";
        case NEGEDGE: return "NEGEDGE";
    }
}
std::vector<asyncTrigger_t> extractSignalEventExpression(
        const SignalEventExpressionSyntax& sigEventExpr,
        std::vector<asyncTrigger_t> triggers
){
    if (sigEventExpr.expr->kind != SyntaxKind::IdentifierName) {
        throw std::runtime_error(
                "Expression not supported on sensitibility list");
    }
    const auto& idExpr = sigEventExpr.expr->as<IdentifierNameSyntax>();
    const std::string name (idExpr.identifier.valueText());
    edge_t edge;
    if (sigEventExpr.edge.valueText() == "posedge"){
        edge = edge_t::POSEDGE;
    } else if (sigEventExpr.edge.valueText() == "negedge"){
        edge = edge_t::NEGEDGE;
    } else{
        throw std::runtime_error(
                "Edge must be posedge or negedge.");
    }
    triggers.push_back({edge, name});
    return triggers;
}

std::vector<asyncTrigger_t> extractAsyncTriggers(
        const EventExpressionSyntax* expr,
        std::vector<asyncTrigger_t> triggers){
    switch (expr->kind){
        case SyntaxKind::SignalEventExpression:
            return extractSignalEventExpression(expr->as<SignalEventExpressionSyntax>(), triggers);
        case SyntaxKind::ParenthesizedEventExpression:{
            const auto& eventExpr = expr->as<ParenthesizedEventExpressionSyntax>().expr;
            return extractAsyncTriggers(eventExpr, triggers);
         }
        case SyntaxKind::BinaryEventExpression:{
            const auto& binaryEventExpr = expr->as<BinaryEventExpressionSyntax>();
            const auto& leftExpr = binaryEventExpr.left;
            const auto& rightExpr = binaryEventExpr.right;
            const auto& token = binaryEventExpr.operatorToken;
            if (token.valueText() != "or"){
                throw std::runtime_error("Only OR supported in event list.");
            }
            triggers = extractAsyncTriggers(leftExpr, triggers);
            triggers = extractAsyncTriggers(rightExpr, triggers);
            return triggers;
        }
        default:
            throw std::runtime_error("Reached invalid code region.");
    }
}
ResolvedTypes::ProceduralTiming resolveProceduralTiming(
        const UnresolvedTypes::ProceduralTiming& timingStatement,
        const ParameterContext& /*ctx*/
){
    const auto& timingControl = timingStatement->timingControl;
    const auto& statement = timingStatement->statement;
    bool is_sequential;
    std::vector<asyncTrigger_t> triggers;

    switch (timingControl->kind){
        case SyntaxKind::ImplicitEventControl:
            std::cout << "We are on combo procedural" << std::endl;
            is_sequential = false;
            break;
        case SyntaxKind::EventControlWithExpression:{
            std::cout << "We are on flop procedural" << std::endl;
            const auto& eventControl = timingControl->as<EventControlWithExpressionSyntax>();
            triggers = extractAsyncTriggers((eventControl.expr), triggers);
            std::cout << "Triggers:\n";
            /*
            for (const auto& t : triggers) {
                std::cout << "  { edge: " << edgeToStr(t.edge)
                          << ", name: " << t.name << " }\n";
            }
            */
            is_sequential = true;
            break;
        }
        default:
            throw std::runtime_error(
                "Not supported timing control: " + std::string(toString(timingControl->kind)));

    }
    auto graph = std::make_unique<DFG>();
    graph = resolveStatement(statement, std::move(graph), is_sequential);
    return graph;
}

ResolvedSignal resolveSignal(const UnresolvedSignal& signal, const ParameterContext& ctx) {
    ResolvedSignal resolved;
    resolved.name = signal.name;

    resolved.type = resolveType(
        *signal.type.syntax,
        ctx);


    if (signal.dimensions.syntax) resolved.dimensions = ResolveDimensions(*signal.dimensions.syntax, ctx);

    return resolved;
}

} // anonymous namespace

ResolvedModule resolveModule(const UnresolvedModule& unresolved, const ParameterContext& topCtx) {
    ResolvedModule resolved;
    resolved.name = unresolved.name;
    auto localCtx = std::make_unique<ParameterContext>(topCtx);

    // Resolve parameters
    for (const auto& param : unresolved.parameters) {
        resolved.parameters.push_back(resolveParameter(param, topCtx, *localCtx));
    }

    auto mergedCtx = std::make_unique<ParameterContext>(topCtx);
    for (const auto& [k, v] : (*localCtx).values) {
        (*mergedCtx).values[k] = v;
    }

    // Resolve inputs
    for (const auto& input : unresolved.inputs) {
        resolved.inputs.push_back(resolveSignal(input, *mergedCtx));
    }

    // Resolve outputs
    for (const auto& output : unresolved.outputs) {
        resolved.outputs.push_back(resolveSignal(output, *mergedCtx));
    }

    // Resolve signals
    for (const auto& signal : unresolved.signals) {
        resolved.signals.push_back(resolveSignal(signal, *mergedCtx));
    }

    // Resolve flops
    for (const auto& flop : unresolved.flops) {
        resolved.flops.push_back(resolveSignal(flop, *mergedCtx));
    }

    // Resolve procedural combo
    for (const auto& block : unresolved.proceduralComboBlocks) {
        resolved.proceduralComboBlocks.push_back(resolveProceduralCombo(block, *mergedCtx));
    }

    // Resolve procedural timing
    for (const auto& block : unresolved.proceduralTimingBlocks) {
        resolved.proceduralTimingBlocks.push_back(resolveProceduralTiming(block, *mergedCtx));
    }

    for (const auto& assign : unresolved.assignStatements) {
        auto assignments = resolveAssign(assign, *mergedCtx);
        resolved.assignStatements.insert(
            resolved.assignStatements.end(),
            std::make_move_iterator(assignments.begin()),
            std::make_move_iterator(assignments.end()));
    }

    return resolved;
}

std::vector<ResolvedModule> resolveModules(
    const std::vector<std::unique_ptr<UnresolvedModule>>& modules) {

    std::vector<ResolvedModule> resolved;
    ParameterContext emptyCtx;  // Use default/empty context for now

    for (const auto& module : modules) {
        resolved.push_back(resolveModule(*module, emptyCtx));
    }

    return resolved;
}

} // namespace custom_hdl
