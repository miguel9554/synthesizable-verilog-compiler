#include "slang/syntax/SyntaxTree.h"
#include "slang/syntax/CSTSerializer.h"
#include "slang/text/Json.h"

#include <fstream>
#include <iostream>

using namespace slang;
using namespace slang::syntax;

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <verilog_file> <output_json>\n";
        return 1;
    }

    const char* inputFile = argv[1];
    const char* outputFile = argv[2];

    auto treeResult = SyntaxTree::fromFile(inputFile);
    if (!treeResult) {
        std::cerr << "Failed to parse: " << inputFile << "\n";
        return 1;
    }

    auto tree = std::move(*treeResult);

    JsonWriter writer;
    writer.setPrettyPrint(true);

    CSTSerializer serializer(writer);
    serializer.serialize(*tree);

    std::ofstream out(outputFile);
    if (!out) {
        std::cerr << "Failed to open output file: " << outputFile << "\n";
        return 1;
    }

    out << writer.view();
    std::cout << "Wrote AST to: " << outputFile << "\n";

    return 0;
}
