#include "resolver.h"
#include "dfg.h"
#include "expression_tree.h"
#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxKind.h"
#include "slang/syntax/SyntaxNode.h"
#include "types.h"

#include <iostream>
#include <memory>
#include <ostream>
#include <stdexcept>

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
    else if (dynamic_cast<const UnaryNode*>(node)){
        throw std::runtime_error("Unary operations not yet supported in DFG");
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
        std::unique_ptr<DFG> graph){
    auto& expr = exprStatement->expr;
    if (expr->kind != SyntaxKind::AssignmentExpression){
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
        std::unique_ptr<DFG> graph){
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
    // const auto expressionTakenBranch = conditionalStatement->statement->as<>;
    // auto graphTakenBranch = resolveExpressionStatement();

    return graph;
}
ResolvedTypes::ProceduralCombo resolveStatement(
        const slang::syntax::StatementSyntax* statement,
        std::unique_ptr<DFG> graph
){
    if (statement->kind != SyntaxKind::SequentialBlockStatement){
        throw std::runtime_error(
        "Statement not synthesizable: " + std::string(toString(statement->kind)));
    }
    auto& seqStatement = statement->as<BlockStatementSyntax>();
    for (const auto& item: seqStatement.items){
        switch (item->kind){
            case SyntaxKind::ExpressionStatement:{
                const auto& exprStatement = item->as<ExpressionStatementSyntax>();
                graph = resolveExpressionStatement(&exprStatement, std::move(graph));
                break;
            }
            case SyntaxKind::ConditionalStatement:{
                const auto& conditionalStatement = item->as<ConditionalStatementSyntax>();
                graph = resolveConditionalStatement( &conditionalStatement, std::move(graph));
                break;
              }

            default:
                throw std::runtime_error(
                    "We expect all statements to be expressions. Current: " + std::string(toString(item->kind)));
        }
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
    graph = resolveStatement(statement, std::move(graph));
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
