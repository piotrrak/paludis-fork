
_se_srcs = [
  'config_file.se',
  'fs_iterator.se',
  'fs_path.se',
  'is_file_with_extension.se',
  'log.se',
]

_hdrs_se_hh = []
_srcs_se_cc = []

foreach se : _se_srcs
  tgt_basename = fs.stem(se) + '-se'

  debug(f'SE custom target - will generate: @se@ -> [@tgt_basename@.hh, @tgt_basename@.cc]')

  _srcs_se_cc += custom_target(f'tgt_pu_@tgt_basename@_cc',
    command: [prog_bash, prog_make_se, '--source', '@INPUT@'],
    capture: true,
    depend_files: script_make_se,
    input: files(se),
    output: f'@tgt_basename@.cc',
  )

  _hdrs_se_hh += custom_target(f'tgt_pu_@tgt_basename@_hh',
    command: [prog_bash, prog_make_se, '--header', '@INPUT@'],
    capture: true,
    depend_files: script_make_se,
    input: files(se),
    output: f'@tgt_basename@.hh',
    depends: _srcs_se_cc[-1],
  )
endforeach

dep_paludis_util_se = declare_dependency(
  sources: _hdrs_se_hh,
  dependencies: dep_paludis_config_h
)

_hdr_util_hh = custom_target(
  command: [prog_m4, '-I', meson.project_source_root(), '-E', '@INPUT@'],
  capture: true,
  input: 'util.hh.m4',
  output: fs.stem('util.hh.m4'),
)

lib_paludis_util = both_libraries('paludisutil', [
  _hdrs_se_hh,
  _hdr_util_hh,
  'active_object_ptr.cc',
  'buffer_output_stream.cc',
  'channel.cc',
  'config_file.cc',
  'cookie.cc',
  'damerau_levenshtein.cc',
  'destringify.cc',
  'deferred_construction_ptr.cc',
  'digest_registry.cc',
  'discard_output_stream.cc',
  'elf.cc',
  'elf_dynamic_section.cc',
  'elf_relocation_section.cc',
  'elf_sections.cc',
  'elf_symbol_section.cc',
  'enum_iterator.cc',
  'env_var_names.cc',
  'exception.cc',
  'executor.cc',
  'extract_host_from_url.cc',
  'fs_iterator.cc',
  'fs_error.cc',
  'fs_path.cc',
  'fs_stat.cc',
  'graph.cc',
  'hashes.cc',
  'is_file_with_extension.cc',
  'log.cc',
  'make_named_values.cc',
  'map.cc',
  'md5.cc',
  'named_value.cc',
  'options.cc',
  'persona.cc',
  'pipe.cc',
  'pool.cc',
  'pretty_print.cc',
  'process.cc',
  'pty.cc',
  'realpath.cc',
  'return_literal_function.cc',
  'rmd160.cc',
  'safe_ifstream.cc',
  'safe_ofstream.cc',
  'sequence.cc',
  'set.cc',
  'sha1.cc',
  'sha256.cc',
  'sha512.cc',
  'simple_parser.cc',
  'string_list_stream.cc',
  'strip.cc',
  'system.cc',
  'tail_output_stream.cc',
  'tee_output_stream.cc',
  'thread_pool.cc',
  'timestamp.cc',
  'tokeniser.cc',
  'tribool.cc',
  'type_list.cc',
  'upper_lower.cc',
  'visitor.cc',
  'visitor_cast.cc',
  'whirlpool.cc',
  'wildcard_expander.cc', ],
  include_directories: inc_,
  dependencies: [dep_paludis_config_h, dl_dep],
)

dep_paludis_util = declare_dependency(
  link_with: lib_paludis_util,
  sources: _hdr_util_hh,
  include_directories: inc_,
  dependencies: dep_paludis_config_h,
)

outputwrapper = executable('outputwrapper', 'output_wrapper.cc')

configure_file(
  input: files('echo_functions.bash.in'),
  output: '@BASENAME@',
  format: 'cmake@',
  configuration: {
    'COLOURSCHEME' : 'notpink',
    'GENERATED_FILE' : generated_warning,
  },
)

_digest_tests = [
  'md5',
  'rmd160',
  'sha1',
  'sha256',
  'sha512',
  'whirlpool',
]

foreach dt : _digest_tests
  dt_src = f'@dt@_TEST.cc'
  dt_name = fs.stem(dt_src)
  dt_exe = executable(dt_name,
    dt_src,
    build_by_default: get_option('unit_tests'),
    dependencies: [dep_paludis_util, dep_gtest],
  )

  test(dt, dt_exe,
    protocol: 'gtest',
    suite: 'util',
  )

endforeach

_util_tests = [
  'active_object_ptr',
  'byte_swap',
  'create_iterator',
  'damerau_levenshtein',
  'destringify',
  'deferred_construction_ptr',
  'enum_iterator',
  'extract_host_from_url',
  'graph',
  'hashes',
  'iterator_range',
  'indirect_iterator',
  'join',
  'log',
  'member_iterator',
  'options',
  'pool',
  'pretty_print',
  'pty',
  'return_literal_function',
  'save',
  'simple_parser',
  'singleton',
  'stream_holder',
  'stringify',
  'strip',
  'system',
  'tail_output_stream',
  'thread_pool',
  'tokeniser',
  'tribool',
  'wrapped_forward_iterator',
  'wrapped_value',
]

foreach tt : _util_tests
  tt_src = f'@tt@_TEST.cc'
  tt_name = fs.stem(tt_src)
  tt_exe = executable(tt_name,
    tt_src,
    build_by_default: get_option('unit_tests'),
    dependencies: [dep_paludis_util, dep_gtest, dep_gmock],
  )

  test(tt, tt_exe,
    protocol: 'gtest',
    suite: 'util',
  )
endforeach

_system_tests = [
  'config_file',
  'fs_iterator',
  'fs_path',
  'fs_stat',
  'is_file_with_extension',
  'process',
  'realpath',
  'safe_ifstream',
  'safe_ofstream',
  'wildcard_expander',
]

foreach tt : _system_tests
  tt_src = f'@tt@_TEST.cc'
  tt_name = fs.stem(tt_src)
  tt_exe = executable(tt_name,
    tt_src,
    build_by_default: get_option('unit_tests'),
    dependencies: [dep_paludis_util, dep_gtest, dep_gmock],
  )

  command = [ 'tt_args=$*' ]
  setup_name = f'@tt_name@_setup.sh'
  if fs.exists(setup_name)
    setup = fs.copyfile(files(setup_name))
    command += setup.full_path()
  endif

  command += tt_exe.full_path() + ' ${tt_args}'
  command += 'tt_ret=$?'

  cleanup_name = f'@tt_name@_cleanup.sh'
  if fs.exists(cleanup_name)
    cleanup = fs.copyfile(files(cleanup_name))
    command += cleanup.full_path()
  endif

  command += 'exit ${tt_ret}'

  test(tt, prog_bash,
    args: ['-c', ';'.join(command), '--'],
    depends: [setup, cleanup],
    suite: ['util', 'util_system'],
    protocol: 'gtest',
  )
endforeach
