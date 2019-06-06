cmake_minimum_required(VERSION 3.12)
project(myccpiler)

set(CMAKE_CXX_STANDARD 14)

find_package(LLVM REQUIRED CONFIG)

add_executable(myccpiler
        main.cpp
        src/lex/lex.cpp
        src/parser/parser.cpp
        src/sema/ast.cpp src/sema/symbol_tables.cpp)
target_include_directories(myccpiler PUBLIC includes)

llvm_map_components_to_libnames(llvm_libs support core irreader)
target_include_directories(myccpiler PUBLIC ${LLVM_INCLUDE_DIRS})
target_compile_definitions(myccpiler PUBLIC ${LLVM_DEFINITIONS})
target_link_libraries(myccpiler ${llvm_libs})

add_executable(first_sets src/tools/main.cpp src/tools/first_set_generator.cpp includes/sema/operator.h includes/sema/types.h includes/sema/symbol_tables.h)
target_include_directories(first_sets PUBLIC includes)