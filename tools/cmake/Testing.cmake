enable_testing()

function(add_tests)
  cmake_parse_arguments(ARG "" "GLOB" "LIBRARIES" ${ARGN})



  foreach(glob_pattern ${ARG_GLOB})

    file(GLOB TEST_FILES "${glob_pattern}")

    foreach(TEST_FILE ${TEST_FILES})
      get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
      add_executable(${TEST_NAME} ${TEST_FILE})
      target_link_libraries(${TEST_NAME} ${ARG_LIBRARIES})
      add_test(${TEST_NAME} ${TEST_NAME})
    endforeach()

  endforeach()

endfunction()
