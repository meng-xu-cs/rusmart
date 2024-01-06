cmake_minimum_required(VERSION 3.22)
project({:NAME:})

# dependencies
set(CFG_CVC5
    "" CACHE PATH
    "CVC5 installation directory")

find_package(cvc5 REQUIRED
             PATHS "{:ARTIFACT:}/lib/cmake/cvc5/"
             NO_DEFAULT_PATH)

# environment
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 23 CACHE STRING "")

# target
add_executable(main main.cpp)
target_link_libraries(main PUBLIC cvc5::cvc5)