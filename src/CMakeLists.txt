set(CDD_LIB_SOURCES
  core/cdd_kinds.f90
  core/cdd_errors.f90
  core/cdd_string.f90
  core/cdd_fs.f90
  core/cdd_standard.f90
)

foreach(optional_src
  json/cdd_json_tokens.f90
  json/cdd_json_lexer.f90
  json/cdd_json_dom.f90
  json/cdd_json_parser.f90
  json/cdd_json_writer.f90
  ir/cdd_ir_types.f90
  ir/cdd_ir_namemap.f90
  json/cdd_json_schema.f90
  json/cdd_json_infer.f90
  cst/cdd_cst_trivia.f90
  cst/cdd_cst_tokens.f90
  cst/cdd_cst_lexer.f90
  cst/cdd_cst_nodes.f90
  cst/cdd_cst_parser.f90
  cst/cdd_cst_printer.f90
  codegen/cdd_emit_udt.f90
  codegen/cdd_extract_udt.f90
  codegen/cdd_emit_codec.f90
  codegen/cdd_patch_udt.f90
  cdd_fortran.f90
  cli/cdd_cli_args.f90
  cli/cdd_cli_runner.f90
)
  if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${optional_src}")
    list(APPEND CDD_LIB_SOURCES "${optional_src}")
  endif()
endforeach()

add_library(cdd_fortran_lib STATIC ${CDD_LIB_SOURCES})
target_compile_options(cdd_fortran_lib PUBLIC ${CDD_STRICT_FLAGS})
target_include_directories(cdd_fortran_lib PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}")
