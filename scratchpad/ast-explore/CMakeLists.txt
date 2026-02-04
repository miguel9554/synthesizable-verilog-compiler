# AST exploration tool
add_executable(ast_to_json ast_to_json.cpp)

target_link_libraries(ast_to_json PRIVATE slang_slang)

target_include_directories(ast_to_json PRIVATE
    ${CMAKE_SOURCE_DIR}/external/slang/include
    ${CMAKE_SOURCE_DIR}/external/slang/external
    ${CMAKE_BINARY_DIR}/external/slang/source
)
