cmake_minimum_required(VERSION 3.20)
project(cdd_fortran VERSION 0.1.0 LANGUAGES Fortran)

include(CTest)
include(CheckFortranCompilerFlag)

option(ENABLE_COVERAGE "Compile with gcov branch/line/function coverage instrumentation" OFF)
option(ENABLE_BOUNDS_CHECK "Compile with runtime -fcheck=all bounds and pointer checks" OFF)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")

check_fortran_compiler_flag("-std=f2023" HAS_STD_F2023)
if(HAS_STD_F2023)
  set(CDD_FORTRAN_STD_FLAG "-std=f2023")
else()
  set(CDD_FORTRAN_STD_FLAG "-std=f2018")
endif()

set(CDD_STRICT_FLAGS
  "${CDD_FORTRAN_STD_FLAG}"
  "-frecursive"
  "-Wall"
  "-Wextra"
  "-Wpedantic"
  "-Werror"
  "-Wimplicit-interface"
  "-Wimplicit-procedure"
  "-fimplicit-none"
  "-Wuse-without-only"
  "-Wconversion"
  "-Wsurprising"
  "-Wunderflow"
  "-Wcharacter-truncation"
  "-Wline-truncation"
)

if(APPLE)
  add_link_options("-Wl,-stack_size,0x4000000")
endif()

if(ENABLE_COVERAGE)
  list(APPEND CDD_STRICT_FLAGS "-g" "-O0" "--coverage" "-fprofile-arcs" "-ftest-coverage")
  add_link_options("--coverage")
elseif(ENABLE_BOUNDS_CHECK)
  list(APPEND CDD_STRICT_FLAGS "-g" "-O0" "-fcheck=all" "-fbacktrace")
endif()

add_subdirectory(src)

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/app/main.f90")
  add_executable(cdd-fortran app/main.f90)
  target_compile_options(cdd-fortran PRIVATE ${CDD_STRICT_FLAGS})
  target_link_libraries(cdd-fortran PRIVATE cdd_fortran_lib)
endif()

if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(test)
endif()

find_package(Python3 COMPONENTS Interpreter REQUIRED)
find_program(DOXYGEN_EXECUTABLE doxygen)

find_program(LCOV_EXECUTABLE lcov)

add_custom_target(coverage
  COMMAND "${LCOV_EXECUTABLE}" --zerocounters --directory "${CMAKE_BINARY_DIR}" --quiet
  COMMAND "${CMAKE_CTEST_COMMAND}" --output-on-failure
  COMMAND "${Python3_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/scripts/check_coverage.py" "${CMAKE_BINARY_DIR}"
  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
  COMMENT "Running CTest and enforcing 100.0% function, line, and branch coverage"
)

if(DOXYGEN_EXECUTABLE)
  add_custom_target(docs
    COMMAND "${CMAKE_COMMAND}" -E make_directory "${CMAKE_SOURCE_DIR}/build/docs"
    COMMAND "${DOXYGEN_EXECUTABLE}" "${CMAKE_SOURCE_DIR}/Doxyfile"
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    COMMENT "Running strict Doxygen Fortran verification (WARN_AS_ERROR)"
  )
endif()
