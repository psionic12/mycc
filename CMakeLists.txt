cmake_minimum_required(VERSION 3.12)
project(myccpiler)

set(CMAKE_CXX_STANDARD 14)

add_executable(myccpiler
        main.cpp
        src/lex/lex.cpp
        src/parser/parser.cpp
        src/sema/SymbolTable.cpp src/sema/ast.cpp)
target_include_directories(myccpiler PUBLIC includes)

add_executable(first_sets src/tools/main.cpp src/tools/first_set_generator.cpp includes/sema/operator.h)
target_include_directories(first_sets PUBLIC includes)