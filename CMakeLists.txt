cmake_minimum_required(VERSION 3.12)
project(myccpiler)

set(CMAKE_CXX_STANDARD 14)

add_executable(myccpiler main.cpp src/lex/lex.cpp includes/parser/parser.h includes/ast/ast.h src/parser/parser.cpp)
target_include_directories(myccpiler PUBLIC includes)