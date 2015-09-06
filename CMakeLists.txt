cmake_minimum_required(VERSION 3.3)

project(javascript)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/tools/cmake")

option(TESTING "Enable testing" OFF)

include(SetCXXSupport)

add_subdirectory(3rdparty/docopt.cpp)

include_directories(3rdparty/docopt.cpp /usr/local/opt/icu4c/include)

add_definitions(-DU_DISABLE_RENAMING)

if (APPLE)
  set(EXTRA_LIBS "icucore")
endif()

add_executable(js src/main.cpp)

target_link_libraries(js
  docopt_s
  icucore
)

if (${TESTING})

  include(Testing)

  add_library(testrunner src/unittest.cpp)

  add_tests(GLOB "src/lexer_unittest.cpp" LIBRARIES testrunner ${EXTRA_LIBS})
endif()
