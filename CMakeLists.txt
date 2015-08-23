cmake_minimum_required(VERSION 3.3)

project(javascript)

add_subdirectory(3rdparty/docopt.cpp)

include_directories(3rdparty/docopt.cpp /usr/local/opt/icu4c/include)

add_definitions(-std=c++14 -DU_DISABLE_RENAMING -fcolor-diagnostics)

add_executable(js src/main.cpp)

target_link_libraries(js
  docopt_s
  icucore
)
