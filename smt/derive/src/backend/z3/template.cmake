cmake_minimum_required(VERSION 3.22)
project({:NAME:})

# dependencies
set(CFG_Z3
    "" CACHE PATH
    "Z3 installation directory")

find_package(Z3 REQUIRED
             PATHS "{:ARTIFACT:}/lib/cmake/z3/"
             NO_DEFAULT_PATH)

# environment
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 23 CACHE STRING "")

# target
add_executable(main main.c)
target_include_directories(main PUBLIC ${Z3_C_INCLUDE_DIRS})
target_link_libraries(main PUBLIC ${Z3_LIBRARIES})