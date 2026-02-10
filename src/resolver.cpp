#include "resolver.h"
#include "dfg.h"
#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxKind.h"
#include "slang/syntax/SyntaxNode.h"
#include "types.h"

#include <algorithm>
#include <atomic>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

using namespace slang::syntax;

namespace custom_hdl {

// Context struct for resolution - bundles all parameters needed during DFG building
struct ResolutionContext {
    DFG& graph;
    ResolvedModule* thisModule;
    const std::set<std::string>& flopNames;
    const ParameterContext& params;
    bool is_sequential;
    std::vector<asyncTrigger_t> triggers;
    // TODO: Add ResolvedModule reference when needed
};

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

std::ostream& operator<<(std::ostream& os, const asyncTrigger_t& t) {
    os << (t.edge == POSEDGE ? "posedge" : "negedge")
       << ":" << t.name;
    return os;
}

void FlopInfo::print(std::ostream& os, int indent) const {
    auto indent_str = [](int n) { return std::string(n * 2, ' '); };

    os << indent_str(indent) << "Flop: " << name << std::endl;
    os << indent_str(indent + 1) << "type: ";
    type.print(os);
    os << std::endl;
    os << indent_str(indent + 1) << "flop_type: ";
    switch (flop_type) {
        case FLOP_D: os << "FLOP_D"; break;
    }
    os << std::endl;
    os << indent_str(indent + 1) << "clock: " << clock << std::endl;
    if (reset) {
        os << indent_str(indent + 1) << "reset: " << *reset << std::endl;
    }
    if (reset_value) {
        os << indent_str(indent + 1) << "reset_value: " << *reset_value << std::endl;
    }
}

void ResolvedSignal::print(std::ostream& os) const {
    os << name << ": ";
    type.print(os);
    for (const auto& dim : dimensions) {
        os << "[" << dim.left << ":" << dim.right << "]";
    }
}



void ResolvedModule::print(int indent) const {
    auto indent_str = [](int n) { return std::string(n * 2, ' '); };

    std::cout << indent_str(indent) << "Module: " << this->name << std::endl;

    std::cout << indent_str(indent + 1) << "Parameters:" << std::endl;
    for (const auto& param : this->parameters) {
        std::cout << indent_str(indent + 2);
        param.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Inputs:" << std::endl;
    for (const auto& in : this->inputs) {
        std::cout << indent_str(indent + 2);
        in.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Outputs:" << std::endl;
    for (const auto& out : this->outputs) {
        std::cout << indent_str(indent + 2);
        out.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Signals:" << std::endl;
    for (const auto& signal : this->signals) {
        std::cout << indent_str(indent + 2);
        signal.print(std::cout);
        std::cout << std::endl;
    }

    std::cout << indent_str(indent + 1) << "Flops:" << std::endl;
    for (const auto& flop : this->flops) {
        flop.print(std::cout, indent + 2);
    }

    std::cout << indent_str(indent + 1) << "No of hier. inst.: " << this->hierarchyInstantiation.size() << std::endl;

    // Write DFG to files
    if (this->dfg) {
        ensureDebugOutputDir();
        std::string graphName = this->name + "_dfg";

        // Write DOT file
        std::string dotFilename = DEBUG_OUTPUT_DIR + "/" + graphName + ".dot";
        std::ofstream dotOut(dotFilename);
        if (dotOut) {
            dotOut << this->dfg->toDot(graphName);
            std::cout << indent_str(indent + 1) << "Wrote DFG to: " << dotFilename << std::endl;
        }

        // Write JSON file
        std::string jsonFilename = DEBUG_OUTPUT_DIR + "/" + graphName + ".json";
        std::ofstream jsonOut(jsonFilename);
        if (jsonOut) {
            jsonOut << this->dfg->toJson();
            std::cout << indent_str(indent + 1) << "Wrote DFG JSON to: " << jsonFilename << std::endl;
        }
    }
}

// ============================================================================
// Resolution functions (STUB implementations)
// ============================================================================

namespace {

// TODO should be double? or parametrized by type.
int64_t parseIntegerVectorExpression(const IntegerVectorExpressionSyntax& vecExpr){
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
    return value;
}

// Forward declaration - in-place statement resolver
void resolveStatementInPlace(
        const slang::syntax::StatementSyntax* statement,
        ResolutionContext& ctx
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

        case SyntaxKind::IntegerVectorExpression: {
            auto& vecExpr = expr->as<IntegerVectorExpressionSyntax>();
            return parseIntegerVectorExpression(vecExpr);
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
    // TODO currently just storing the reduction of this, the width.
    const auto packedDimensions = ResolveDimensions(packedDimensionsSyntax, ctx);

    // Compute total width as product of all dimension sizes
    int width = 1;
    for (const auto& dim : packedDimensions) {
        width *= dim.size();
    }

    return ResolvedType::makeInteger(width, is_signed);
}

DFGNode* resolveIdentifier(
        const std::string baseName,
        DFG& graph,
        bool throw_on_not_found,
        const std::set<std::string>& flopNames
){
    std::string signalName = baseName;

    // In sequential blocks, flops on RHS use .q suffix
    if (flopNames.contains(baseName)) {
        signalName = baseName + ".q";
    }
    // Use lookupSignal helper - DO NOT CREATE
    DFGNode* node = graph.lookupSignal(signalName);
    if (throw_on_not_found && node == nullptr) {
        throw std::runtime_error("Undeclared signal: '" + signalName + "'");
    }
    return node;
}

// Build DFG node directly from slang expression syntax
// For sequential blocks (is_sequential=true), flop references on RHS use .q suffix
DFGNode* buildExprDFG(
        const ExpressionSyntax* expr,
        ResolutionContext& ctx
) {
    if (!expr) {
        throw std::runtime_error("Cannot build DFG from null expression");
    }

    switch (expr->kind) {
        case SyntaxKind::IntegerLiteralExpression: {
            auto& literal = expr->as<LiteralExpressionSyntax>();
            auto text = literal.literal.rawText();
            int64_t value = std::stoll(std::string(text));
            return ctx.graph.constant(value);
        }

        case SyntaxKind::IntegerVectorExpression: {
            auto& vecExpr = expr->as<IntegerVectorExpressionSyntax>();
            const auto value = parseIntegerVectorExpression(vecExpr);
            return ctx.graph.constant(value);
        }

        case SyntaxKind::IdentifierName: {
            auto& name = expr->as<IdentifierNameSyntax>();
            std::string baseName(name.identifier.valueText());
            const auto node = resolveIdentifier(
                    baseName,
                    ctx.graph,
                    true,
                    ctx.flopNames
            );
            return node;
        }

        case SyntaxKind::IdentifierSelectName: {
            auto& name = expr->as<IdentifierSelectNameSyntax>();
            std::string baseName(name.identifier.valueText());
            const auto node = resolveIdentifier(
                    baseName,
                    ctx.graph,
                    true,
                    ctx.flopNames
            );
            const auto selectors = &name.selectors;
            auto indexedSignalNode = node;

            if (selectors){
                for (const auto& elemSelect: *selectors){
                    if (!elemSelect->selector){
                        throw std::runtime_error(
                            "Empty selector not allowed.");
                    }
                    if (elemSelect->selector->kind != SyntaxKind::BitSelect){
                        throw std::runtime_error(
                            "Currently only single element select supported.");
                    }
                    const auto& bitSelect = elemSelect->selector->as<BitSelectSyntax>();
                    const auto& selectorExpr = bitSelect.expr;
                    auto* selectorExprNode = buildExprDFG(selectorExpr, ctx);
                    indexedSignalNode = ctx.graph.index(indexedSignalNode, selectorExprNode);
                }
            }

            return indexedSignalNode;
        }

        case SyntaxKind::ParenthesizedExpression: {
            auto& paren = expr->as<ParenthesizedExpressionSyntax>();
            return buildExprDFG(paren.expression, ctx);
        }

        // Unary operations
        case SyntaxKind::UnaryPlusExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.unaryPlus(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryMinusExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.unaryNegate(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryBitwiseAndExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.reductionAnd(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryBitwiseNandExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.reductionNand(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryBitwiseOrExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.reductionOr(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryBitwiseNorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.reductionNor(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryBitwiseXorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.reductionXor(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryBitwiseXnorExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.reductionXnor(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryLogicalNotExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.logicalNot(buildExprDFG(unary.operand, ctx));
        }

        case SyntaxKind::UnaryBitwiseNotExpression: {
            auto& unary = expr->as<PrefixUnaryExpressionSyntax>();
            return ctx.graph.bitwiseNot(buildExprDFG(unary.operand, ctx));
        }

        // Binary operations
        case SyntaxKind::AddExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.add(buildExprDFG(binary.left, ctx),
                                 buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::SubtractExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.sub(buildExprDFG(binary.left, ctx),
                                 buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::MultiplyExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.mul(buildExprDFG(binary.left, ctx),
                                 buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::DivideExpression: {
            throw std::runtime_error("DIV operation not yet supported in DFG");
        }

        case SyntaxKind::EqualityExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.eq(buildExprDFG(binary.left, ctx),
                                buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::LessThanExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.lt(buildExprDFG(binary.left, ctx),
                                buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::LessThanEqualExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.le(buildExprDFG(binary.left, ctx),
                                buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::GreaterThanExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.gt(buildExprDFG(binary.left, ctx),
                                buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::GreaterThanEqualExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.ge(buildExprDFG(binary.left, ctx),
                                buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::ArithmeticShiftLeftExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.shl(buildExprDFG(binary.left, ctx),
                                 buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::ArithmeticShiftRightExpression: {
            auto& binary = expr->as<BinaryExpressionSyntax>();
            return ctx.graph.asr(buildExprDFG(binary.left, ctx),
                                 buildExprDFG(binary.right, ctx));
        }

        case SyntaxKind::ConditionalExpression: {
            auto& cond = expr->as<ConditionalExpressionSyntax>();
            if (cond.predicate->conditions.size() != 1) {
                throw std::runtime_error("Only single condition supported in ternary expression");
            }
            if (cond.predicate->conditions[0]->matchesClause) {
                throw std::runtime_error("matches clause not supported in ternary expression");
            }
            auto* condNode = buildExprDFG(cond.predicate->conditions[0]->expr, ctx);
            auto* trueNode = buildExprDFG(cond.left, ctx);
            auto* falseNode = buildExprDFG(cond.right, ctx);
            return ctx.graph.mux(condNode, trueNode, falseNode);
        }

        default:
            throw std::runtime_error(
                "Unsupported expression kind in DFG building: " +
                std::string(toString(expr->kind)));
    }
}

void resolveAssignExpression(const BinaryExpressionSyntax& assignExpr,
        ResolutionContext& ctx){
    const auto& left = assignExpr.left;
    const auto& right = assignExpr.right;

    // Build the Expr graph of the RHS
    auto* RHSexprNode = buildExprDFG(right, ctx);

    // Get the base name and selectors for LHS
    std::string baseName;
    const SyntaxList<ElementSelectSyntax>* selectors = nullptr;
    if (left->kind == SyntaxKind::IdentifierName) {
        const auto& identifier = left->as<slang::syntax::IdentifierNameSyntax>();
        baseName = identifier.identifier.valueText();
    } else if (left->kind == SyntaxKind::IdentifierSelectName) {
        const auto& identifier = left->as<IdentifierSelectNameSyntax>();
        baseName = identifier.identifier.valueText();
        selectors = &identifier.selectors;
    } else {
        throw std::runtime_error(
        "Left can only be variable name: " + std::string(toString(left->kind)));
    }

    // Build the full element name for LHS by evaluating selectors statically
    std::string indexSuffix;
    if (selectors) {
        for (const auto& elemSelect : *selectors) {
            if (!elemSelect->selector) {
                throw std::runtime_error("Empty selector not allowed.");
            }
            if (elemSelect->selector->kind != SyntaxKind::BitSelect) {
                throw std::runtime_error(
                    "Currently only single element select supported.");
            }
            const auto& bitSelect = elemSelect->selector->as<BitSelectSyntax>();
            const auto& selectorExpr = bitSelect.expr;

            // Try to evaluate the index statically
            try {
                int64_t idx = evaluateConstantExpr(selectorExpr, ctx.params);
                indexSuffix += "[" + std::to_string(idx) + "]";
            } catch (const std::runtime_error&) {
                throw std::runtime_error(
                    "Dynamic index on LHS not supported for: " + baseName);
            }
        }
    }

    // Build the full output name
    // For sequential blocks with flops: base[idx].d
    // For combinational: base[idx] or just base
    std::string outputName;
    if (ctx.is_sequential) {
        if (ctx.flopNames.contains(baseName)) {
            outputName = baseName + indexSuffix + ".d";
        } else {
            throw std::runtime_error(
                std::format("{} NOT a flop and assigned on seq. block", baseName));
        }
    } else {
        outputName = baseName + indexSuffix;
    }

    // Connect driver to existing output or signal node
    if (ctx.graph.outputs.contains(outputName)) {
        ctx.graph.connectOutput(outputName, RHSexprNode);
    } else if (ctx.graph.signals.contains(outputName)) {
        ctx.graph.connectSignal(outputName, RHSexprNode);
    } else {
        throw std::runtime_error("Cannot assign to undeclared: " + outputName);
    }

    // If the assign is sequential, set the triggers of the signal
    if (ctx.is_sequential) {
        ctx.thisModule->flopsTriggers[baseName] = ctx.triggers;
    }
}

// Resolve a continuous assignment in-place on the shared DFG
void resolveAssignInPlace(
        const ContinuousAssignSyntax* syntax,
        ResolutionContext& ctx
    ){
    if (!syntax) throw std::runtime_error("Null pointer");
    if (syntax->strength) throw std::runtime_error("Strength statement not valid.");
    if (syntax->delay) throw std::runtime_error("Delay statement not valid.");

    ctx.is_sequential = false;

    for (const auto* assignExpr : syntax->assignments) {
        if (!assignExpr->isKind(SyntaxKind::AssignmentExpression)) {
            throw std::runtime_error(
                "Expected assignment expression, got: " +
                std::string(toString(assignExpr->kind)));
        }

        const auto& binaryAssign = assignExpr->as<BinaryExpressionSyntax>();
        resolveAssignExpression(binaryAssign, ctx);
    }
}

void resolveExpressionStatementInPlace(
        const ExpressionStatementSyntax* exprStatement,
        ResolutionContext& ctx){
    auto& expr = exprStatement->expr;
    const auto expectedKind = ctx.is_sequential ? SyntaxKind::NonblockingAssignmentExpression :
                                                  SyntaxKind::AssignmentExpression;
    if (expr->kind != expectedKind){
        throw std::runtime_error(
        "Can only process assign expression. Current: " + std::string(toString(expr->kind)));
    }
    const auto& assignExpr = expr->as<slang::syntax::BinaryExpressionSyntax>();
    resolveAssignExpression(assignExpr, ctx);
}

void resolveConditionalStatementInPlace(
        const ConditionalStatementSyntax* conditionalStatement,
        ResolutionContext& ctx){
    const auto& predicate = conditionalStatement->predicate;
    if (conditionalStatement->uniqueOrPriority){
        throw std::runtime_error("Unique/priority not supported on if");
    }
    if (predicate->conditions.size()>1){
        throw std::runtime_error("Support for single predicate on if");
    }
    const auto& predicateExpr = predicate->conditions[0]->expr;

    // construct the signal node for the predicate expression
    auto conditionNode = buildExprDFG(predicateExpr, ctx);

    // Extract driver nodes from outputs and signals
    // (we need to track what's connected before modifications)
    // Skip aggregate nodes (in.size() > 1) which represent structural decomposition, not driven signals
    auto getDrivers = [](const DFG& g) {
        std::unordered_map<std::string, DFGNode*> drivers;
        for (const auto& [outName, outNode] : g.outputs) {
            if (outNode->in.size() == 1) {
                drivers[outName] = outNode->in[0];
            }
        }
        for (const auto& [sigName, sigNode] : g.signals) {
            if (sigNode->in.size() == 1) {
                drivers[sigName] = sigNode->in[0];
            }
        }
        return drivers;
    };

    const auto oldDrivers = getDrivers(ctx.graph);

    if (conditionalStatement->elseClause) {
        const auto& elseClause = conditionalStatement->elseClause->clause;
        const auto& elseStatement = elseClause->as<StatementSyntax>();
        resolveStatementInPlace(&elseStatement, ctx);
    }

    const auto elseDrivers = getDrivers(ctx.graph);

    resolveStatementInPlace(conditionalStatement->statement, ctx);

    // Helper to connect a signal (output or signal type)
    auto connectSignalOrOutput = [&ctx](const std::string& name, DFGNode* driver) {
        if (ctx.graph.outputs.contains(name)) {
            ctx.graph.connectOutput(name, driver);
        } else if (ctx.graph.signals.contains(name)) {
            ctx.graph.connectSignal(name, driver);
        }
    };

    // Helper to get current driver for a signal
    // Skip aggregate nodes (in.size() > 1) which represent structural decomposition
    auto getCurrentDriver = [&ctx](const std::string& name) -> DFGNode* {
        if (auto it = ctx.graph.outputs.find(name); it != ctx.graph.outputs.end()) {
            return it->second->in.size() == 1 ? it->second->in[0] : nullptr;
        }
        if (auto it = ctx.graph.signals.find(name); it != ctx.graph.signals.end()) {
            return it->second->in.size() == 1 ? it->second->in[0] : nullptr;
        }
        return nullptr;
    };

    // Collect all signals that were assigned
    std::set<std::string> assignedSignals;
    for (const auto& [name, _] : ctx.graph.outputs) {
        if (getCurrentDriver(name) != nullptr) {
            assignedSignals.insert(name);
        }
    }
    for (const auto& [name, _] : ctx.graph.signals) {
        if (getCurrentDriver(name) != nullptr) {
            assignedSignals.insert(name);
        }
    }

    // Assign MUXes for signals that ARE assigned on IF branch
    for (const auto& outName : assignedSignals) {
        DFGNode* oldDriver;
        DFGNode* newDriver = getCurrentDriver(outName);
        if (!newDriver) continue;

        // First, check if signal is assigned in ELSE clause
        auto it = elseDrivers.find(outName);
        if (it != elseDrivers.end()) {
            oldDriver = it->second;
        } else {
            auto it2 = oldDrivers.find(outName);
            if (it2 != oldDrivers.end()) {
                oldDriver = it2->second;
            } else if (ctx.is_sequential) {
                // In sequential blocks, use .q as fallback (flop retains value)
                std::string qName = outName;
                if (qName.ends_with(".d")) {
                    qName = qName.substr(0, qName.length() - 2) + ".q";
                }
                // Look up the .q signal node
                DFGNode* qNode = ctx.graph.lookupSignal(qName);
                if (!qNode) {
                    throw std::runtime_error("Could not find .q signal: " + qName);
                }
                oldDriver = qNode;
            } else {
                throw std::runtime_error(
                    "Signal is not assigned in IF branch but not other: " + outName);
            }
        }

        if (oldDriver != newDriver) {
            // Add the mux!
            const auto& muxOut = ctx.graph.mux(conditionNode, newDriver, oldDriver);
            connectSignalOrOutput(outName, muxOut);
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
        DFGNode* currentDriver = getCurrentDriver(elseName);
        bool if_modified = (currentDriver != nullptr && currentDriver != elseDriver);

        // IF already handled this signal in the loop above
        if (if_modified) {
            continue;
        }

        // Signal is only in ELSE, not in OLD and not in IF
        if (!oldDriver) {
            if (ctx.is_sequential) {
                // In sequential blocks, use .q as fallback (flop retains value)
                std::string qName = elseName;
                if (qName.ends_with(".d")) {
                    qName = qName.substr(0, qName.length() - 2) + ".q";
                }
                // Look up the .q signal node
                DFGNode* qNode = ctx.graph.lookupSignal(qName);
                if (!qNode) {
                    throw std::runtime_error("Could not find .q signal: " + qName);
                }
                oldDriver = qNode;
            } else {
                throw std::runtime_error(
                    "Signal '" + elseName + "' is only assigned in ELSE branch, not supported");
            }
        }

        // Signal is in ELSE and OLD but not IF → add MUX
        // TRUE (condition true, IF branch): use oldDriver (IF didn't change it)
        // FALSE (condition false, ELSE branch): use elseDriver
        auto muxOut = ctx.graph.mux(conditionNode, oldDriver, elseDriver);
        connectSignalOrOutput(elseName, muxOut);
    }
}

void resolveCaseStatementInPlace(
        const CaseStatementSyntax* caseStatement,
        ResolutionContext& ctx) {

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
    auto selectorNode = buildExprDFG(caseStatement->expr, ctx);

    // Extract driver nodes from outputs and signals
    // Skip aggregate nodes (in.size() > 1) which represent structural decomposition, not driven signals
    auto getDrivers = [](const DFG& g) {
        std::unordered_map<std::string, DFGNode*> drivers;
        for (const auto& [outName, outNode] : g.outputs) {
            if (outNode->in.size() == 1) {
                drivers[outName] = outNode->in[0];
            }
        }
        for (const auto& [sigName, sigNode] : g.signals) {
            if (sigNode->in.size() == 1) {
                drivers[sigName] = sigNode->in[0];
            }
        }
        return drivers;
    };

    // Helper to connect a signal (output or signal type)
    auto connectSignalOrOutput = [&ctx](const std::string& name, DFGNode* driver) {
        if (ctx.graph.outputs.contains(name)) {
            ctx.graph.connectOutput(name, driver);
        } else if (ctx.graph.signals.contains(name)) {
            ctx.graph.connectSignal(name, driver);
        }
    };

    // Helper to restore drivers to fallback state
    auto restoreDrivers = [&connectSignalOrOutput](const std::unordered_map<std::string, DFGNode*>& drivers) {
        for (const auto& [name, driver] : drivers) {
            connectSignalOrOutput(name, driver);
        }
    };

    const auto fallbackDrivers = getDrivers(ctx.graph);

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
            restoreDrivers(fallbackDrivers);
            resolveStatementInPlace(&defaultItem.clause->as<StatementSyntax>(), ctx);
            defaultDrivers = getDrivers(ctx.graph);
        } else if (item->kind == SyntaxKind::StandardCaseItem) {
            const auto& caseItem = item->as<StandardCaseItemSyntax>();

            if (caseItem.expressions.size() != 1) {
                throw std::runtime_error("Multiple expressions per case item not yet supported");
            }

            // Build condition: selector == case_value
            auto caseValueNode = buildExprDFG(caseItem.expressions[0], ctx);
            auto conditionNode = ctx.graph.eq(selectorNode, caseValueNode);

            // Reset to fallback state before processing
            restoreDrivers(fallbackDrivers);

            // Process case body
            resolveStatementInPlace(&caseItem.clause->as<StatementSyntax>(), ctx);
            normalCases.push_back({conditionNode, getDrivers(ctx.graph)});
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
            } else if (ctx.is_sequential) {
                // In sequential blocks, use .q as fallback (flop retains value)
                std::string qName = signalName;
                if (qName.ends_with(".d")) {
                    qName = qName.substr(0, qName.length() - 2) + ".q";
                }
                // Look up the .q signal node
                DFGNode* qNode = ctx.graph.lookupSignal(qName);
                if (!qNode) {
                    throw std::runtime_error("Could not find .q signal: " + qName);
                }
                caseValue = qNode;
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
            // Compute default selector: none of the normal cases matched
            DFGNode* selectorSum = selectors[0];
            for (size_t i = 1; i < selectors.size(); ++i) {
                selectorSum = ctx.graph.add(selectorSum, selectors[i]);
            }
            DFGNode* defaultSel = ctx.graph.eq(selectorSum, ctx.graph.constant(0));
            selectors.push_back(defaultSel);
            dataValues.push_back(defaultValue);
        }

        if (selectors.size() == 1) {
            result = dataValues[0];
        } else {
            result = ctx.graph.muxN(selectors, dataValues);
        }

        connectSignalOrOutput(signalName, result);
    }
}

void resolveSequentialBlockStatementInPlace(
        const slang::syntax::BlockStatementSyntax* seqStatement,
        ResolutionContext& ctx
){
    for (const auto* item: seqStatement->items){
        const auto& statement = item->as<StatementSyntax>();
        resolveStatementInPlace(&statement, ctx);
    }
}

void resolveStatementInPlace(
        const slang::syntax::StatementSyntax* statement,
        ResolutionContext& ctx
){
    switch (statement->kind){
        case SyntaxKind::SequentialBlockStatement:{
            const auto& seqStatement = statement->as<BlockStatementSyntax>();
            resolveSequentialBlockStatementInPlace(&seqStatement, ctx);
            break;
        }
        case SyntaxKind::ExpressionStatement:{
            const auto& exprStatement = statement->as<ExpressionStatementSyntax>();
            resolveExpressionStatementInPlace(&exprStatement, ctx);
            break;
        }
        case SyntaxKind::ConditionalStatement:{
            const auto& conditionalStatement = statement->as<ConditionalStatementSyntax>();
            resolveConditionalStatementInPlace(&conditionalStatement, ctx);
            break;
        }
        case SyntaxKind::CaseStatement:{
            const auto& caseStmt = statement->as<CaseStatementSyntax>();
            resolveCaseStatementInPlace(&caseStmt, ctx);
            break;
        }

        default:
            throw std::runtime_error(
                "We expect all statements to be expressions. Current: " + std::string(toString(statement->kind)));
    }
}

// Resolve procedural combo block in-place on shared DFG
void resolveProceduralComboInPlace(
        const UnresolvedTypes::ProceduralCombo& statement,
        ResolutionContext& ctx
){
    if (statement->kind != SyntaxKind::SequentialBlockStatement){
        throw std::runtime_error(
        "Statement not synthesizable: " + std::string(toString(statement->kind)));
    }
    // always_comb is not sequential
    ctx.is_sequential = false;
    resolveStatementInPlace(statement, ctx);
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
// Resolve procedural timing block in-place on shared DFG
void resolveProceduralTimingInPlace(
        const UnresolvedTypes::ProceduralTiming& timingStatement,
        ResolutionContext& ctx
){
    const auto& timingControl = timingStatement->timingControl;
    const auto& statement = timingStatement->statement;
    std::vector<asyncTrigger_t> triggers;

    switch (timingControl->kind){
        case SyntaxKind::ImplicitEventControl:
            std::cout << "We are on combo procedural" << std::endl;
            ctx.is_sequential = false;
            break;
        case SyntaxKind::EventControlWithExpression:{
            std::cout << "We are on flop procedural" << std::endl;
            const auto& eventControl = timingControl->as<EventControlWithExpressionSyntax>();
            triggers = extractAsyncTriggers((eventControl.expr), triggers);
            std::cout << "Triggers:\n";
            ctx.is_sequential = true;
            break;
        }
        default:
            throw std::runtime_error(
                "Not supported timing control: " + std::string(toString(timingControl->kind)));

    }
    ctx.triggers = triggers;
    resolveStatementInPlace(statement, ctx);
}

ResolvedSignal resolveSignal(const UnresolvedSignal& signal, const ParameterContext& ctx) {
    ResolvedSignal resolved;
    resolved.name = signal.name;

    resolved.type = resolveType(
        *signal.type.syntax,
        ctx);


    if (signal.dimensions.syntax) resolved.dimensions = ResolveDimensions(*signal.dimensions.syntax, ctx);

    // For some reason getting 1 dimension of [0:0]
    if(resolved.dimensions.size() == 1 && resolved.dimensions[0].left == 0 && resolved.dimensions[0].right == 0){
        resolved.dimensions = {};
    }

    return resolved;
}

// ============================================================================
// Pre-population helpers for DFG
// ============================================================================

// Generate all index suffixes for multi-dimensional arrays
// For [0:1], returns ["[0]", "[1]"]
// For [0:1][0:1], returns ["[0][0]", "[0][1]", "[1][0]", "[1][1]"]
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

// Pre-populate module input (port) with all bit indices
// For vector inputs, creates base node + individual element nodes
void prePopulateInput(DFG& graph, const ResolvedSignal& sig) {
    if (sig.dimensions.empty()) {
        graph.input(sig.name);
    } else {
        // Create base node for dynamic read access
        graph.input(sig.name);
        // Create individual element nodes
        for (const auto& suffix : generateIndexSuffixes(sig.dimensions)) {
            graph.input(sig.name + suffix);
        }
    }
}

// Pre-populate module output (port) with all bit indices
// Creates OUTPUT nodes with no driver (->in empty)
// For vector outputs, creates base node + individual element nodes
void prePopulateOutput(DFG& graph, const ResolvedSignal& sig) {
    if (sig.dimensions.empty()) {
        auto n = std::make_unique<DFGNode>(DFGOp::OUTPUT, sig.name);
        graph.nodes.push_back(std::move(n));
        graph.outputs[sig.name] = graph.nodes.back().get();
    } else {
        // Create base node for dynamic read access
        auto n = std::make_unique<DFGNode>(DFGOp::OUTPUT, sig.name);
        graph.nodes.push_back(std::move(n));
        graph.outputs[sig.name] = graph.nodes.back().get();
        // Create individual element nodes
        for (const auto& suffix : generateIndexSuffixes(sig.dimensions)) {
            auto elemNode = std::make_unique<DFGNode>(DFGOp::OUTPUT, sig.name + suffix);
            graph.nodes.push_back(std::move(elemNode));
            graph.outputs[sig.name + suffix] = graph.nodes.back().get();
        }
    }
}

// Pre-populate internal signal with all bit indices
// For vector signals, creates individual element nodes
// For flop .d/.q signals, handles special naming (my_flop[idx].d)
void prePopulateSignal(DFG& graph, const ResolvedSignal& sig) {
    if (sig.dimensions.empty()) {
        graph.signal(sig.name);
        return;
    }

    // Extract base name and optional type suffix for flop signals (.d or .q)
    std::string baseName = sig.name;
    std::string typeSuffix;

    if (sig.name.ends_with(".d") || sig.name.ends_with(".q")) {
        baseName = sig.name.substr(0, sig.name.length() - 2);
        typeSuffix = sig.name.substr(sig.name.length() - 2);
    }

    // Create aggregate node for dynamic access
    const auto& aggregate = graph.signal(sig.name);

    // Create individual element nodes
    for (const auto& idxSuffix : generateIndexSuffixes(sig.dimensions)) {
        std::string elemName = baseName + idxSuffix + typeSuffix;
        const auto& individual = graph.signal(elemName);
        aggregate->in.push_back(individual);
    }
}

FlopInfo extractFlopClockAndreset(DFG& graph, ResolvedModule& resolved, const std::string flop_name, const FlopInfo flopIn, DFGNode*& functionalLogic){
        auto flop = flopIn;
        const std::string& dName = flop_name + ".d";
        const auto& dNode = graph.signals.at(dName);
        const auto& dNodeDriver = dNode->in[0];
        const auto& triggers = resolved.flopsTriggers.at(flopIn.name);
        if (dNode->in.size() != 1) {
            throw std::runtime_error(std::format(
                        "Flop must have single driver: {} has {}", dName, dNode->in.size()));
        }
        asyncTrigger_t clock;
        asyncTrigger_t reset;
        bool has_reset;
        int reset_value;
        if (triggers.size() == 1){
            // Only clock, no reset
            clock = triggers[0];
            functionalLogic = dNodeDriver;
            has_reset = false;
        } else if (triggers.size() == 2){
            // First op SHOULD be mux for reset
            if (dNodeDriver->op != DFGOp::MUX){
                throw std::runtime_error("Reset MUX not present.");
            }
            const auto& mux_sel = dNodeDriver->in[0];
            const auto& mux_true = dNodeDriver->in[1]; // TRUE branch
            const auto& mux_else = dNodeDriver->in[2]; // false branch
            bool is_negated = false;
            DFGNode* expectedResetNode;
            if (mux_sel->op == DFGOp::BITWISE_NOT ||
                    mux_sel->op ==DFGOp::LOGICAL_NOT){
                is_negated = true;
                expectedResetNode = mux_sel->in[0];
            } else{
                expectedResetNode = mux_sel;
            }
            if (expectedResetNode->op != DFGOp::INPUT){
                throw std::runtime_error("Reset MUX NOT driven by input.");
            }
            const std::string& reset_name = expectedResetNode->name;
            if (triggers[0].name == reset_name) {
                reset = triggers[0];
                clock = triggers[1];
            } else if (triggers[1].name == reset_name){
                reset = triggers[1];
                clock = triggers[0];
            } else {
                throw std::runtime_error("Reset not present in sensitivity list.");
            }

            // Check that the reset polarity is correct
            if (reset.edge == edge_t::NEGEDGE){
                if (is_negated == false){
                    throw std::runtime_error("NEGEDGE reset should have NEG polarity.");
                }
            }

            if (reset.edge == edge_t::POSEDGE){
                if (is_negated == true){
                    throw std::runtime_error("POSEDGE reset should have PLUS polarity.");
                }
            }

            has_reset = true;

            // Assign clocked logic
            functionalLogic = mux_else;

            // TODO recover (and check) reset value
            // should evaluate constant mux_true
            /*
            if (mux_true->op != DFGOp::CONST){
                throw std::runtime_error("Only support CONST for reset val.");
            }
            reset_value = std::get<int64_t>(mux_true->data);
            */
            // TODO fix this garbage.
            reset_value = 0;
        } else {
            throw std::runtime_error(std::format("Trigger size not supported: {}", triggers.size()));
        }

        // Check that functionalLogic has NO interaction with clock or reset
        // Meaning, they are not present in any node operation
        flop.clock = clock;
        if (has_reset) {
            flop.reset = reset;
            flop.reset_value = reset_value;
        }
        flop.name = flop_name;
        flop.type.name = flop_name;
        return flop;
}

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
        const auto& resolvedSignal = (resolveSignal(flop, *mergedCtx));
        resolved.flops.push_back(FlopInfo{
                .name = resolvedSignal.name,
                .type = resolvedSignal,
                .flop_type = FLOP_D,
                .clock = {},
                .reset = std::nullopt,
                .reset_value = std::nullopt,
                });
        flopNames.insert(flop.name);
    }

    // === Create single DFG and pre-populate ===
    resolved.dfg = std::make_unique<DFG>();
    DFG& graph = *resolved.dfg;

    // Pre-populate module PARAMETERS (ports only)
    for (const auto& parameter : resolved.parameters) {
        prePopulateInput(graph, parameter);
    }

    // Pre-populate module INPUTS (ports only)
    for (const auto& input : resolved.inputs) {
        prePopulateInput(graph, input);
    }

    // Pre-populate module OUTPUTS (ports only, no driver yet)
    for (const auto& output : resolved.outputs) {
        prePopulateOutput(graph, output);
    }

    // Pre-populate internal SIGNALS (not ports)
    for (const auto& signal : resolved.signals) {
        prePopulateSignal(graph, signal);
    }

    // === Resolve all blocks into the shared graph ===
    // Create resolution context
    ResolutionContext resCtx{graph, &resolved, flopNames, *mergedCtx, false, {}};

    for (const auto& block : unresolved.proceduralComboBlocks) {
        resolveProceduralComboInPlace(block, resCtx);
    }

    for (const auto& block : unresolved.proceduralTimingBlocks) {
        resolveProceduralTimingInPlace(block, resCtx);
    }

    for (const auto& assign : unresolved.assignStatements) {
        resolveAssignInPlace(assign, resCtx);
    }

    // Connect flop outputs to their .q signals
    for (const auto& output : resolved.outputs) {
        if (flopNames.contains(output.name)) {
            if (output.dimensions.empty()) {
                // Scalar flop output: connect output to name.q
                std::string qName = output.name + ".q";
                DFGNode* qNode = graph.lookupSignal(qName);
                if (qNode) {
                    graph.connectOutput(output.name, qNode);
                }
            } else {
                // Vector flop output: connect each element
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

    // Check flops is correct and assign them
    std::vector<FlopInfo> resolved_flops;
    for (const auto& flop: resolved.flops) {
        for (const auto& name: allElements(flop.type)){
            DFGNode* functional_logic;
            resolved_flops.push_back(extractFlopClockAndreset(graph, resolved, name, flop, functional_logic));
            // graph.outputs[name]
        }
    }
    resolved.flops = resolved_flops;

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
