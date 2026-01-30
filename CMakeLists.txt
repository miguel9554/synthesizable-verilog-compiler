cmake_minimum_required(VERSION 3.15)
project(CustomHDL VERSION 0.1.0 LANGUAGES CXX)

# Set C++20 standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Add some useful compiler flags
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(-Wall -Wextra -pedantic)
endif()

# Check if slang submodule is initialized
if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/external/slang/CMakeLists.txt")
    message(FATAL_ERROR
        "Slang submodule not found. Please run:\n"
        "  git submodule update --init --recursive"
    )
endif()

# Add slang as a subdirectory
# This builds slang as part of your project
message(STATUS "Adding slang from external/slang")
add_subdirectory(external/slang)

# Collect all source files in src/
file(GLOB PROJECT_SOURCES src/*.cpp)

# Create the main executable
add_executable(custom_hdl_compiler ${PROJECT_SOURCES})

# Link against slang
target_link_libraries(custom_hdl_compiler PRIVATE
    slang_slang
    boost_unordered
)

# Include directories
target_include_directories(custom_hdl_compiler PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/external/slang/include
    ${CMAKE_CURRENT_SOURCE_DIR}/external/slang/external
    ${CMAKE_CURRENT_BINARY_DIR}/external/slang/source
)

# Optional: Add install target
install(TARGETS custom_hdl_compiler
    RUNTIME DESTINATION bin
)

# Custom target to clean only project artifacts (not slang)
add_custom_target(clean_project
    COMMAND ${CMAKE_COMMAND} -E rm -f ${CMAKE_BINARY_DIR}/custom_hdl_compiler
    COMMAND find ${CMAKE_BINARY_DIR}/CMakeFiles/custom_hdl_compiler.dir -name "*.o" -delete 2>/dev/null || true
    COMMENT "Cleaning project artifacts only (preserving slang)"
)

# Print build configuration
message(STATUS "")
message(STATUS "Custom HDL Compiler Configuration:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Compiler: ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "  C++ Standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "")
