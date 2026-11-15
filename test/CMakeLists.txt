add_library(cdd_test_harness STATIC test_harness.f90)
target_compile_options(cdd_test_harness PUBLIC ${CDD_STRICT_FLAGS})
target_link_libraries(cdd_test_harness PUBLIC cdd_fortran_lib)

set(CDD_TEST_TARGETS
  doc_coverage_audit
  test_errors_and_strings
  test_standard_registry
  test_json_suite
  test_ir_and_namemap
  test_cst_roundtrip
  test_emit_udt_standards
  test_extract_udt_standards
  test_emit_codec_standards
  test_codec_runtime_compile
  test_patch_udt_trivia
  test_cli_suite
)

foreach(test_name ${CDD_TEST_TARGETS})
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${test_name}.f90")
    add_executable(${test_name} "${test_name}.f90")
    target_compile_options(${test_name} PRIVATE ${CDD_STRICT_FLAGS})
    target_link_libraries(${test_name} PRIVATE cdd_test_harness cdd_fortran_lib)
    add_test(NAME ${test_name} COMMAND ${test_name} WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
  endif()
endforeach()

if(TARGET cdd-fortran AND TARGET test_cli_suite)
  add_dependencies(test_cli_suite cdd-fortran)
endif()

find_package(Python3 COMPONENTS Interpreter REQUIRED)
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/test_precommit_hooks.py")
  add_test(
    NAME test_precommit_hooks
    COMMAND "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/test_precommit_hooks.py"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  )
endif()
