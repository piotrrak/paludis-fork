_se_srcs = [
  'args_handler.se',
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

_srcs = [
  'args.cc',
  'args_error.cc',
  'args_group.cc',
  'args_handler.cc',
  'args_option.cc',
  'args_section.cc',
  'bad_argument.cc',
  'args_visitor.cc',
  'args_dumper.cc',
  'escape.cc',
  'log_level_arg.cc',
  'man.cc',
]

libpaludisargs = both_libraries('paludisargs', [
   _srcs,
   _hdrs_se_hh,
  ],
  dependencies: dep_paludis_util,
)

dep_paludis_args = declare_dependency(
  link_with: libpaludisargs,
  sources: _hdrs_se_hh,
  dependencies: [dep_paludis_config_h, dep_paludis_util],
)

exe_args_TEST= executable('args_TEST', 'args_TEST.cc',
  dependencies: [dep_gtest, dep_paludis_args],
  build_by_default: get_option('unit_tests'),
)

test('args', exe_args_TEST, protocol: 'gtest', suite: 'args')
