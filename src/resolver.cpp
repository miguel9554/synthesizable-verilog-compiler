#include "resolver.h"
#include "dfg.h"
#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxKind.h"
#include "slang/syntax/SyntaxNode.h"
#include "types.h"

#include <algorithm>
#include <atomic>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <stdexcept>
#include <unordered_map>
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
        bool is_sequential,
        const std::set<std::string>& flopNames
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

// Build DFG node directly from slang expression syntax
// For sequential blocks (is_sequential=true), flop references on RHS use .q suffix
DFGNode* buildExprDFG(DFG& graph, const ExpressionSyntax* expr,
                      bool is_sequential, const std::set<std::string>& flopNames) {
    if (!expr) {
        throw std::runtime_error("Cannot build DFG from null expression");
    }

    switch (expr->kind) {
        case SyntaxKind::IntegerLiteralExpression: {
            auto& literal = expr->as<LiteralExpressionSyntax>();
            auto text = literal.literal.rawText();
            int64_t value = std::stoll(std::string(text));
            return graph.constant(value);
        }

        case SyntaxKind::IntegerVectorExpression: {
            auto& vecExpr = expr->as<IntegerVectorExpressionSyntax>();
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
            return graph.constant(value);
        }

        case SyntaxKind::IdentifierName: {
            auto& name = expr->as<IdentifierNameSyntax>();
            std::string baseName(name.identifier.valueText());
            std::string signalName = baseName;
            // In sequential blocks, flops on RHS use .q suffix
            if (is_sequential && flopNames.contains(baseName)) {
                signalName = baseName + ".q";
            }
            // Check if signal already exists in graph
            if (graph.inputs.contains(signalName)) return graph.inputs[signalName];
            if (graph.outputs.contains(signalName)) return graph.outputs[signalName]->in[0];
            if (graph.signals.contains(signalName)) return graph.signals[signalName];
            // Create new input
            return graph.input(signalName);
        }

        case SyntaxKind::IdentifierSelectName: {
            auto& idSelect = expr->as<IdentifierSelectNameSyntax>();
            std::string baseName(idSelect.identifier.valueText());
            std::string selectors(idSelect.selectors.toString());
            std::string signalName;
            // In sequential blocks, flops on RHS use .q suffix
            if (is_sequential && flopNames.contains(baseName)) {
                signalName = baseName + ".q" + selectors;
            } else {
                signalName = baseName + selectors;
            }
            // Check if signal already exists in graph
            if (graph.inputs.contains(signalName)) return graph.inputs[signalName];
            if (graph.outputs.contains(signalName)) return graph.outputs[signalName]->in[0];
            if (graph.signals.contains(signalName)) return graph.signals[signalName];
            // Create new input
            return graph.input(signalName);
        }

        case SyntaxKind::ParenthesizedExpression: {
            auto& paren = expr->as<ParenthesizedExpressionSyntax>();
            return buildExprDFG(graph, paren.expression, is_sequential, flopNames);
        }

        // Unary operations
        case SyntaxKind::UnaryPlusExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.unaryPlus(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryMinusExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.unaryNegate(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryBitwiseAndExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.reductionAnd(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryBitwiseNandExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.reductionNand(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryBitwiseOrExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.reductionOr(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryBitwiseNorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.reductionNor(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryBitwiseXorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.reductionXor(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryBitwiseXnorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.reductionXnor(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryLogicalNotExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.logicalNot(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        case SyntaxKind::UnaryBitwiseNotExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return graph.bitwiseNot(buildExprDFG(graph, unary.operand, is_sequential, flopNames));
        }

        // Binary operations
        case SyntaxKind::AddExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.add(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                           buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::SubtractExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.sub(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                           buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::MultiplyExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.mul(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                           buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::DivideExpression: {
            throw std::runtime_error("DIV operation not yet supported in DFG");
        }

        case SyntaxKind::EqualityExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.eq(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                          buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::LessThanExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.lt(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                          buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::LessThanEqualExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.le(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                          buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::GreaterThanExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.gt(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                          buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::GreaterThanEqualExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.ge(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                          buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::ArithmeticShiftLeftExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.shl(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                           buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::ArithmeticShiftRightExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return graph.asr(buildExprDFG(graph, binary.left, is_sequential, flopNames),
                           buildExprDFG(graph, binary.right, is_sequential, flopNames));
        }

        case SyntaxKind::ConditionalExpression: {
            auto& cond = expr->as<ConditionalExpressionSyntax>();
            if (cond.predicate->conditions.size() != 1) {
                throw std::runtime_error("Only single condition supported in ternary expression");
            }
            if (cond.predicate->conditions[0]->matchesClause) {
                throw std::runtime_error("matches clause not supported in ternary expression");
            }
            auto* condNode = buildExprDFG(graph, cond.predicate->conditions[0]->expr, is_sequential, flopNames);
            auto* trueNode = buildExprDFG(graph, cond.left, is_sequential, flopNames);
            auto* falseNode = buildExprDFG(graph, cond.right, is_sequential, flopNames);
            return graph.mux(condNode, trueNode, falseNode);
        }

        default:
            throw std::runtime_error(
                "Unsupported expression kind in DFG building: " +
                std::string(toString(expr->kind)));
    }
}

std::vector<ResolvedTypes::Assign> resolveAssign(
        const ContinuousAssignSyntax* syntax,
        const ParameterContext& /*ctx*/,
        const std::set<std::string>& flopNames
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

        // Get output name from left side
        std::string outputName;
        if (binaryAssign.left->kind == SyntaxKind::IdentifierName) {
            const auto& identifier = binaryAssign.left->as<IdentifierNameSyntax>();
            outputName = identifier.identifier.valueText();
        } else if (binaryAssign.left->kind == SyntaxKind::IdentifierSelectName) {
            const auto& identifier = binaryAssign.left->as<IdentifierSelectNameSyntax>();
            outputName = std::string(identifier.identifier.valueText()) + std::string(identifier.selectors.toString());
        } else {
            throw std::runtime_error(
                "Left side must be identifier: " + std::string(toString(binaryAssign.left->kind)));
        }

        // Build DFG directly from expression
        // is_sequential=false (no .d suffix on LHS), but flopNames needed for .q on RHS
        auto graph = std::make_unique<DFG>();
        auto* exprNode = buildExprDFG(*graph, binaryAssign.right, false, flopNames);
        graph->output(exprNode, outputName, true);
        result.push_back(std::move(graph));
    }

    return result;
}

std::unique_ptr<DFG> resolveExpressionStatement(
        const ExpressionStatementSyntax* exprStatement,
        std::unique_ptr<DFG> graph,
        bool is_sequential,
        const std::set<std::string>& flopNames){
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

    // Get the base name and full name for LHS
    std::string baseName;
    std::string selectors;
    if (left->kind == SyntaxKind::IdentifierName) {
        const auto& identifier = left->as<slang::syntax::IdentifierNameSyntax>();
        baseName = identifier.identifier.valueText();
    } else if (left->kind == SyntaxKind::IdentifierSelectName) {
        const auto& identifier = left->as<IdentifierSelectNameSyntax>();
        baseName = identifier.identifier.valueText();
        selectors = identifier.selectors.toString();
    } else {
        throw std::runtime_error(
        "Left can only be variable name: " + std::string(toString(left->kind)));
    }

    // In sequential blocks, flops on LHS use .d suffix
    std::string outputName;
    if (is_sequential && flopNames.contains(baseName)) {
        outputName = baseName + ".d" + selectors;
    } else {
        outputName = baseName + selectors;
    }

    auto* exprNode = buildExprDFG(*graph, right, is_sequential, flopNames);
    graph->output(exprNode, outputName, true);
    return graph;
}

std::unique_ptr<DFG> resolveConditionalStatement(
        const ConditionalStatementSyntax* conditionalStatement,
        std::unique_ptr<DFG> graph,
        bool is_sequential,
        const std::set<std::string>& flopNames){
    const auto& predicate = conditionalStatement->predicate;
    if (conditionalStatement->uniqueOrPriority){
        throw std::runtime_error("Unique/priority not supported on if");
    }
    if (predicate->conditions.size()>1){
        throw std::runtime_error("Support for single predicate on if");
    }
    const auto& predicateExpr = predicate->conditions[0]->expr;

    // construct the signal node for the predicate expression
    auto conditionNode = buildExprDFG(*graph, predicateExpr, is_sequential, flopNames);

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
        graph = resolveStatement(&elseStatement, std::move(graph), is_sequential, flopNames);
    }

    const auto elseDrivers = getDrivers(*graph);

    graph = resolveStatement(conditionalStatement->statement, std::move(graph), is_sequential, flopNames);

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
            } else if (is_sequential) {
                // In sequential blocks, use .q as fallback (flop retains value)
                std::string qName = outName;
                size_t dPos = qName.find(".d");
                if (dPos != std::string::npos && (dPos + 2 == qName.size() || qName[dPos + 2] == '[')) {
                    qName.replace(dPos, 2, ".q");
                }
                // Look up the .q signal node
                if (graph->inputs.contains(qName)) {
                    oldDriver = graph->inputs[qName];
                } else if (graph->outputs.contains(qName)) {
                    oldDriver = graph->outputs[qName]->in[0];
                } else if (graph->signals.contains(qName)) {
                    oldDriver = graph->signals[qName];
                } else {
                    throw std::runtime_error("Could not find .q signal: " + qName);
                }
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

        // Signal is only in ELSE, not in OLD and not in IF
        if (!oldDriver) {
            if (is_sequential) {
                // In sequential blocks, use .q as fallback (flop retains value)
                std::string qName = elseName;
                size_t dPos = qName.find(".d");
                if (dPos != std::string::npos && (dPos + 2 == qName.size() || qName[dPos + 2] == '[')) {
                    qName.replace(dPos, 2, ".q");
                }
                // Look up the .q signal node
                if (graph->inputs.contains(qName)) {
                    oldDriver = graph->inputs[qName];
                } else if (graph->outputs.contains(qName)) {
                    oldDriver = graph->outputs[qName]->in[0];
                } else if (graph->signals.contains(qName)) {
                    oldDriver = graph->signals[qName];
                } else {
                    throw std::runtime_error("Could not find .q signal: " + qName);
                }
            } else {
                throw std::runtime_error(
                    "Signal '" + elseName + "' is only assigned in ELSE branch, not supported");
            }
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

std::unique_ptr<DFG> resolveCaseStatement(
        const CaseStatementSyntax* caseStatement,
        std::unique_ptr<DFG> graph,
        bool is_sequential,
        const std::set<std::string>& flopNames) {

    if (caseStatement->uniqueOrPriority) {
        throw std::runtime_error("unique/priority case not supported");
    }

    // Only support basic 'case', not casez/casex
    auto caseKeyword = caseStatement->caseKeyword.kind;
    if (caseKeyword == slang::parsing::TokenKind::CaseZKeyword) {
        throw std::runtime_error("casez not supported");
    }
    if (caseKeyword == slang::parsing::TokenKind::CaseXKeyword) {
        throw std::runtime_error("casex not supported");
    }

    // Build the selector expression node
    auto selectorNode = buildExprDFG(*graph, caseStatement->expr, is_sequential, flopNames);

    auto getDrivers = [](const DFG& g) {
        std::unordered_map<std::string, DFGNode*> drivers;
        for (const auto& [outName, outNode] : g.outputs) {
            if (!outNode->in.empty()) {
                drivers[outName] = outNode->in[0];
            }
        }
        return drivers;
    };

    const auto fallbackDrivers = getDrivers(*graph);

    // Collect info for each case
    struct CaseInfo {
        DFGNode* condition;  // selector == value
        std::unordered_map<std::string, DFGNode*> drivers;
    };
    std::vector<CaseInfo> normalCases;
    std::optional<std::unordered_map<std::string, DFGNode*>> defaultDrivers;

    for (const auto* item : caseStatement->items) {
        if (item->kind == SyntaxKind::DefaultCaseItem) {
            const auto& defaultItem = item->as<DefaultCaseItemSyntax>();
            // Reset to fallback state before processing
            for (const auto& [name, driver] : fallbackDrivers) {
                graph->output(driver, name, true);
            }
            graph = resolveStatement(&defaultItem.clause->as<StatementSyntax>(), std::move(graph), is_sequential, flopNames);
            defaultDrivers = getDrivers(*graph);
        } else if (item->kind == SyntaxKind::StandardCaseItem) {
            const auto& caseItem = item->as<StandardCaseItemSyntax>();

            if (caseItem.expressions.size() != 1) {
                throw std::runtime_error("Multiple expressions per case item not yet supported");
            }

            // Build condition: selector == case_value
            auto caseValueNode = buildExprDFG(*graph, caseItem.expressions[0], is_sequential, flopNames);
            auto conditionNode = graph->eq(selectorNode, caseValueNode);

            // Reset to fallback state before processing
            for (const auto& [name, driver] : fallbackDrivers) {
                graph->output(driver, name, true);
            }

            // Process case body
            graph = resolveStatement(&caseItem.clause->as<StatementSyntax>(), std::move(graph), is_sequential, flopNames);
            normalCases.push_back({conditionNode, getDrivers(*graph)});
        } else {
            throw std::runtime_error(
                "Unsupported case item kind: " + std::string(toString(item->kind)));
        }
    }

    // Collect all signals that were assigned in any case
    std::set<std::string> assignedSignals;
    for (const auto& c : normalCases) {
        for (const auto& [name, driver] : c.drivers) {
            auto it = fallbackDrivers.find(name);
            if (it == fallbackDrivers.end() || driver != it->second) {
                assignedSignals.insert(name);
            }
        }
    }
    if (defaultDrivers) {
        for (const auto& [name, driver] : *defaultDrivers) {
            auto it = fallbackDrivers.find(name);
            if (it == fallbackDrivers.end() || driver != it->second) {
                assignedSignals.insert(name);
            }
        }
    }

    // Build MUX for each assigned signal
    for (const auto& signalName : assignedSignals) {
        std::vector<DFGNode*> selectors;
        std::vector<DFGNode*> dataValues;

        // Determine default/fallback value for this signal
        DFGNode* defaultValue = nullptr;
        if (defaultDrivers) {
            auto it = defaultDrivers->find(signalName);
            if (it != defaultDrivers->end()) {
                defaultValue = it->second;
            }
        }
        if (!defaultValue) {
            auto it = fallbackDrivers.find(signalName);
            if (it != fallbackDrivers.end()) {
                defaultValue = it->second;
            }
        }

        // Collect selectors and data values from each case
        for (const auto& c : normalCases) {
            auto it = c.drivers.find(signalName);
            DFGNode* caseValue;
            if (it != c.drivers.end()) {
                caseValue = it->second;
            } else if (defaultValue) {
                caseValue = defaultValue;
            } else if (is_sequential) {
                // In sequential blocks, use .q as fallback (flop retains value)
                std::string qName = signalName;
                size_t dPos = qName.find(".d");
                if (dPos != std::string::npos && (dPos + 2 == qName.size() || qName[dPos + 2] == '[')) {
                    qName.replace(dPos, 2, ".q");
                }
                // Look up the .q signal node
                if (graph->inputs.contains(qName)) {
                    caseValue = graph->inputs[qName];
                } else if (graph->outputs.contains(qName)) {
                    caseValue = graph->outputs[qName]->in[0];
                } else if (graph->signals.contains(qName)) {
                    caseValue = graph->signals[qName];
                } else {
                    throw std::runtime_error("Could not find .q signal: " + qName);
                }
                // Also set defaultValue so subsequent branches use the same node
                defaultValue = caseValue;
            } else {
                throw std::runtime_error(
                    "Signal '" + signalName + "' not assigned in all case branches and has no default/fallback");
            }

            selectors.push_back(c.condition);
            dataValues.push_back(caseValue);
        }

        DFGNode* result;
        if (defaultValue) {
            // Has default: use chain of 2:1 MUXes to handle "no match" case
            // Each MUX: if selector[i] matches, use dataValues[i], else use previous result
            result = defaultValue;
            for (size_t i = 0; i < selectors.size(); ++i) {
                result = graph->mux(selectors[i], dataValues[i], result);
            }
        } else {
            // No default: use MUX_N directly (assumes cases are exhaustive/one-hot)
            if (selectors.size() == 1) {
                result = dataValues[0];
            } else {
                result = graph->muxN(selectors, dataValues);
            }
        }

        graph->output(result, signalName, true);
    }

    return graph;
}

ResolvedTypes::ProceduralCombo resolveSequentialBlockStatement(
        const slang::syntax::BlockStatementSyntax* seqStatement,
        std::unique_ptr<DFG> graph,
        bool is_sequential,
        const std::set<std::string>& flopNames
){
    for (const auto* item: seqStatement->items){
        // TODO should catch if this fails...
        const auto& statement = item->as<StatementSyntax>();
        graph = resolveStatement(&statement, std::move(graph), is_sequential, flopNames);
    }
    return graph;
}

ResolvedTypes::ProceduralCombo resolveStatement(
        const slang::syntax::StatementSyntax* statement,
        std::unique_ptr<DFG> graph,
        bool is_sequential,
        const std::set<std::string>& flopNames
){
    switch (statement->kind){
        case SyntaxKind::SequentialBlockStatement:{
            const auto& seqStatement = statement->as<BlockStatementSyntax>();
            graph = resolveSequentialBlockStatement(&seqStatement, std::move(graph), is_sequential, flopNames);
            break;
        }
        case SyntaxKind::ExpressionStatement:{
            const auto& exprStatement = statement->as<ExpressionStatementSyntax>();
            graph = resolveExpressionStatement(&exprStatement, std::move(graph), is_sequential, flopNames);
            break;
        }
        case SyntaxKind::ConditionalStatement:{
            const auto& conditionalStatement = statement->as<ConditionalStatementSyntax>();
            graph = resolveConditionalStatement(&conditionalStatement, std::move(graph), is_sequential, flopNames);
            break;
        }
        case SyntaxKind::CaseStatement:{
            const auto& caseStmt = statement->as<CaseStatementSyntax>();
            graph = resolveCaseStatement(&caseStmt, std::move(graph), is_sequential, flopNames);
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
        const ParameterContext& /*ctx*/,
        const std::set<std::string>& flopNames
){
    if (statement->kind != SyntaxKind::SequentialBlockStatement){
        throw std::runtime_error(
        "Statement not synthesizable: " + std::string(toString(statement->kind)));
    }
    auto graph = std::make_unique<DFG>();
    // always_comb is not sequential, but we still pass flopNames for reference lookups
    graph = resolveStatement(statement, std::move(graph), false, flopNames);
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
        const ParameterContext& /*ctx*/,
        const std::set<std::string>& flopNames
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
    graph = resolveStatement(statement, std::move(graph), is_sequential, flopNames);
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

    // Resolve flops and build flopNames set
    std::set<std::string> flopNames;
    for (const auto& flop : unresolved.flops) {
        resolved.flops.push_back(resolveSignal(flop, *mergedCtx));
        flopNames.insert(flop.name);
    }

    // Resolve procedural combo
    for (const auto& block : unresolved.proceduralComboBlocks) {
        resolved.proceduralComboBlocks.push_back(resolveProceduralCombo(block, *mergedCtx, flopNames));
    }

    // Resolve procedural timing
    for (const auto& block : unresolved.proceduralTimingBlocks) {
        resolved.proceduralTimingBlocks.push_back(resolveProceduralTiming(block, *mergedCtx, flopNames));
    }

    for (const auto& assign : unresolved.assignStatements) {
        auto assignments = resolveAssign(assign, *mergedCtx, flopNames);
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
