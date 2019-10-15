cmake_minimum_required(VERSION 3.12)
project(mycc)

set(CMAKE_CXX_STANDARD 14)
#set (CMAKE_C_COMPILER clang)
#set (CMAKE_CXX_COMPILER clang++)

find_package(LLVM REQUIRED CONFIG)
find_package(Boost REQUIRED COMPONENTS program_options)

add_executable(mycc
        main.cpp
        src/lex/lex.cpp
        src/parser/parser.cpp
        src/sema/ast.cpp src/sema/symbol_tables.cpp src/sema/types.cpp includes/sema/qualifiedType.h src/sema/qualifiedType.cpp src/sema/value.cpp)
target_include_directories(mycc PUBLIC includes)

llvm_map_components_to_libnames(llvm_libs support core irreader mc mcjit native interpreter)
target_include_directories(mycc PUBLIC ${LLVM_INCLUDE_DIRS})
target_compile_definitions(mycc PUBLIC ${LLVM_DEFINITIONS})
target_link_libraries(mycc ${llvm_libs})
target_link_libraries(mycc Boost::program_options)

add_executable(first_sets src/tools/main.cpp src/tools/first_set_generator.cpp)
target_include_directories(first_sets PUBLIC includes)
