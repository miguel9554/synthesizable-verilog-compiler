#include "ir_builder.h"
#include "syntax_extract.h"

#include <algorithm>
#include <memory>
#include <span>
#include <stdexcept>

#include "slang/syntax/AllSyntax.h"
#include "slang/syntax/SyntaxKind.h"
#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/SyntaxVisitor.h"
#include "types.h"

using namespace slang::syntax;
using namespace custom_hdl;

namespace {

// Visitor class that builds our custom IR from slang syntax tree
class IRBuilderVisitor : public SyntaxVisitor<IRBuilderVisitor> {
public:
    std::vector<std::unique_ptr<UnresolvedModule>> modules;
    UnresolvedModule* currentModule = nullptr;

    // Module members we support and will visit
    static constexpr SyntaxKind allowedMembers[] = {
        SyntaxKind::DataDeclaration,
        SyntaxKind::NetDeclaration,
        SyntaxKind::ParameterDeclarationStatement,
        // SyntaxKind::GenvarDeclaration,
        SyntaxKind::EmptyMember,
        // SyntaxKind::ProceduralBlock,
        // SyntaxKind::InitialBlock,
        // SyntaxKind::FinalBlock,
        SyntaxKind::AlwaysBlock,
        SyntaxKind::AlwaysCombBlock,
        SyntaxKind::AlwaysFFBlock,
        // SyntaxKind::AlwaysLatchBlock,
        // SyntaxKind::GenerateRegion,
        // SyntaxKind::LoopGenerate,
        // SyntaxKind::IfGenerate,
        // SyntaxKind::CaseGenerate,
        // SyntaxKind::GenerateBlock,
        // SyntaxKind::TimeUnitsDeclaration,
        SyntaxKind::HierarchyInstantiation,
        // SyntaxKind::FunctionDeclaration,
        SyntaxKind::ContinuousAssign,
        // SyntaxKind::DefParam,
        // SyntaxKind::ElabSystemTask,
        // SyntaxKind::LocalVariableDeclaration,
    };

    static bool isInList(SyntaxKind kind, std::span<const SyntaxKind> list) {
        return std::find(list.begin(), list.end(), kind) != list.end();
    }

    void handle(const ModuleDeclarationSyntax& node) {
        auto headerInfo = extractModuleHeader(*node.header);

        auto module = std::make_unique<UnresolvedModule>();
        module->name = std::move(headerInfo.name);
        module->parameters = std::move(headerInfo.parameters);
        module->inputs = std::move(headerInfo.inputs);
        module->outputs = std::move(headerInfo.outputs);

        if (node.blockName) throw std::runtime_error("Can't parse blockName");

        // Set current module context
        currentModule = module.get();

        // Visit module members explicitly
        for (auto* member : node.members) {
            if (isInList(member->kind, allowedMembers)) {
                member->visit(*this);
            } else {
                throw std::runtime_error(
                    "Disallowed module member: " + std::string(toString(member->kind))
                );
            }
        }

        // Store the completed module
        modules.push_back(std::move(module));
        currentModule = nullptr;
    }

    void handle(const DeclaratorSyntax& node) {
        auto signal = std::make_unique<UnresolvedSignal>();
        signal->name = std::string(node.name.valueText());
    }

    void handle(const DataDeclarationSyntax& node) {
        if (!currentModule) throw std::runtime_error(
                "Procedural block must be inside module.");
        const auto type = extractDataType(*node.type);
        std::vector<UnresolvedSignal> signals;
        for (auto declarator : node.declarators){
                signals.push_back(
                    UnresolvedSignal{
                    .name = std::string(declarator->name.valueText()),
                    .type = type,
                    .dimensions = {&(declarator->dimensions)},
                });
        }
        std::move(signals.begin(), signals.end(),
                  std::back_inserter(currentModule->signals));
    }

    void handle(const HierarchyInstantiationSyntax& node) {
        if (!currentModule) throw std::runtime_error(
                "Continuous assign must be inside module.");
        currentModule->hierarchyInstantiation.push_back(&node);
    }

    void handle(const ContinuousAssignSyntax& node) {
        if (!currentModule) throw std::runtime_error(
                "Continuous assign must be inside module.");
        currentModule->assignStatements.push_back(&node);
    }

    void handle(const ProceduralBlockSyntax& node) {
        if (!currentModule) throw std::runtime_error(
                "Procedural block must be inside module.");

        // Extract sensitivity list based on block kind
        switch (node.kind) {
            case SyntaxKind::AlwaysFFBlock:
            case SyntaxKind::AlwaysBlock:{
                auto& statement = node.statement;
                if (statement->isKind(SyntaxKind::TimingControlStatement)){
                    auto& timingControl = statement->as<TimingControlStatementSyntax>();
                    currentModule->proceduralTimingBlocks.push_back(&timingControl);
                } else {
                    throw std::runtime_error("Procedural block must have timing control.");
                }
                break;
             }
            case SyntaxKind::AlwaysCombBlock:{
                auto& statement = node.statement;
                if (isInList(statement->kind, synthesizableStatements)) {
                    currentModule->proceduralComboBlocks.push_back(statement);

                } else {
                    throw std::runtime_error(
                    "Not synthesizable statement: " + std::string(toString(statement->kind)));
                }
                break;
             }
            case SyntaxKind::AlwaysLatchBlock:
                throw std::runtime_error("Latch not allowed.");
            case SyntaxKind::InitialBlock:
                throw std::runtime_error("Initial block not synthesizable");
            default:
                throw std::runtime_error("Unknown procedural block kind");
        }

        visitDefault(node);
    }
};

} // anonymous namespace

namespace custom_hdl {

std::vector<std::unique_ptr<UnresolvedModule>> buildIR(const SyntaxTree& tree) {
    IRBuilderVisitor visitor;
    tree.root().visit(visitor);
    return std::move(visitor.modules);
}

} // namespace custom_hdl
