
_se_srcs = [
  'action.se',
  'choice.se',
  'create_output_manager_info.se',
  'dep_spec_annotations.se',
  'elike_blocker.se',
  'elike_choices.se',
  'elike_dep_parser.se',
  'elike_package_dep_spec.se',
  'elike_use_requirement.se',
  'fs_merger.se',
  'hook.se',
  'mask.se',
  'match_package.se',
  'merger_entry_type.se',
  'merger.se',
  'metadata_key.se',
  'output_manager.se',
  'package_id.se',
  'partially_made_package_dep_spec.se',
  'pretty_print_options.se',
  'repository.se',
  'set_file.se',
  'tar_merger.se',
  'user_dep_spec.se',
  'version_operator.se',
  'version_spec.se',
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

dep_paludis_se = declare_dependency(
  sources: _hdrs_se_hh,
  dependencies: dep_paludis_config_h
)

configure_file(
  configuration: {
    'PACKAGE' : 'paludis',
    'PALUDIS_PC_SLOT': '0x2a',
    'VERSION_MAJOR': '0',
    'VERSION_MINOR': '0',
    'VERSION_MICRO': '0',
    'VERSION_SUFFIX': 'dendrodatio',
    'GENERATED_FILE': generated_warning,
    'CXXFLAGS': '-Osuperfast',
    'LDFLAGS': '-Wl,everything',
    'BUILDHOST': run_command('hostname').stdout().strip(),
    'BUILDUSER': 'nobody',
    'CXX': cpp.get_id(),
  },
  input:'about.hh.in',
  output: 'about.hh'
)

paludis_srcdir = meson.project_source_root()
paludis_builddir = meson.project_build_root()

test_env = environment({
    'HOME': meson.project_build_root(),
    'PALUDIS_BYPASS_USERPRIV_CHECKS': '1',
    'PALUDIS_DEFAULT_OUTPUT_CONF':
      paludis_srcdir / 'paludis/environments/paludis/tests_output.conf',
    'PALUDIS_DISTRIBUTION': 'gentoo',
    'PALUDIS_DISTRIBUTIONS_DIR':
      paludis_srcdir / 'paludis/distributions',
    'PALUDIS_EAPIS_DIR':
      paludis_srcdir / 'paludis/repositories/e/eapis',
    'PALUDIS_EBUILD_DIR':
      paludis_srcdir / 'paludis/repositories/e/ebuild',
    'PALUDIS_EBUILD_DIR_FALLBACK':
      paludis_builddir / 'paludis/repositories/e/ebuild',
    'PALUDIS_ECHO_FUNCTIONS_DIR':
      paludis_builddir / 'paludis/util',
    'PALUDIS_EXTRA_EBUILD_MODULES_DIRS': paludis_builddir / 'paludis/util',
    'PALUDIS_FETCHERS_DIR': paludis_srcdir / 'paludis/fetchers',
    'PALUDIS_HOOKER_DIR': paludis_srcdir / 'paludis',
    'PALUDIS_NO_CHOWN': '1',
    'PALUDIS_NO_GLOBAL_HOOKS': '1',
    'PALUDIS_NO_GLOBAL_SETS': '1',
    'PALUDIS_NO_XTERM_TITLES': '1',
    'PALUDIS_SUFFIXS_FILE':
      paludis_srcdir / 'paludis/repositories/e/ebuild_entries_suffixes.conf',
  },
)

meson.add_devenv(test_env)

subdir('util')
subdir('selinux')
subdir('environments')
subdir('repositories')

_srcs = [
  'about_metadata.cc',
  'action.cc',
  'action_names.cc',
  'additional_package_dep_spec_requirement.cc',
  'always_enabled_dependency_label.cc',
  'broken_linkage_configuration.cc',
  'broken_linkage_finder.cc',
  'buffer_output_manager.cc',
  'call_pretty_printer.cc',
  'changed_choices.cc',
  'choice.cc',
  'comma_separated_dep_parser.cc',
  'comma_separated_dep_pretty_printer.cc',
  'command_output_manager.cc',
  'common_sets.cc',
  'contents.cc',
  'create_output_manager_info.cc',
  'dep_label.cc',
  'dep_spec.cc',
  'dep_spec_annotations.cc',
  'dep_spec_data.cc',
  'dep_spec_flattener.cc',
  'distribution.cc',
  'elf_linkage_checker.cc',
  'elike_blocker.cc',
  'elike_choices.cc',
  'elike_dep_parser.cc',
  'elike_conditional_dep_spec.cc',
  'elike_package_dep_spec.cc',
  'elike_slot_requirement.cc',
  'elike_use_requirement.cc',
  'environment.cc',
  'environment_factory.cc',
  'environment_implementation.cc',
  'file_output_manager.cc',
  'filter.cc',
  'filter_handler.cc',
  'filtered_generator.cc',
  'format_messages_output_manager.cc',
  'formatted_pretty_printer.cc',
  'forward_at_finish_output_manager.cc',
  'fs_merger.cc',
  'fuzzy_finder.cc',
  'generator.cc',
  'generator_handler.cc',
  'hook.cc',
  'hooker.cc',
  'ipc_output_manager.cc',
  'libtool_linkage_checker.cc',
  'linkage_checker.cc',
  'literal_metadata_key.cc',
  'maintainer.cc',
  'mask.cc',
  'mask_utils.cc',
  'match_package.cc',
  'merger.cc',
  'merger_entry_type.cc',
  'metadata_key.cc',
  'metadata_key_holder.cc',
  'name.cc',
  'ndbam.cc',
  'ndbam_merger.cc',
  'ndbam_unmerger.cc',
  'notifier_callback.cc',
  'output_manager.cc',
  'output_manager_factory.cc',
  'output_manager_from_environment.cc',
  'package_dep_spec_collection.cc',
  'package_dep_spec_properties.cc',
  'package_id.cc',
  'paludislike_options_conf.cc',
  'partially_made_package_dep_spec.cc',
  'partitioning.cc',
  'permitted_choice_value_parameter_values.cc',
  'pretty_print_options.cc',
  'pretty_printer.cc',
  'repository.cc',
  'repository_factory.cc',
  'repository_name_cache.cc',
  'selection.cc',
  'selection_handler.cc',
  'serialise.cc',
  'set_file.cc',
  'slot.cc',
  'slot_requirement.cc',
  'spec_tree.cc',
  'standard_output_manager.cc',
  'stripper.cc',
  'syncer.cc',
  'tar_merger.cc',
  'tee_output_manager.cc',
  'unchoices_key.cc',
  'unformatted_pretty_printer.cc',
  'unmerger.cc',
  'user_dep_spec.cc',
  'version_operator.cc',
  'version_requirements.cc',
  'version_spec.cc',
]

deps_libpaludis = [dep_paludis_util]

deps_libpaludis += dep_libpaludis_selinux

foreach r : repositories
  deps_libpaludis += get_variable(f'dep_paludis_@r@_repository')
endforeach

foreach e : environments
  deps_libpaludis += get_variable(f'dep_paludis_@e@_environment') 
endforeach

libpaludis = both_libraries('paludis', _srcs,
  dependencies: deps_libpaludis,
)

dep_paludis = declare_dependency(
  link_with: libpaludis,
  dependencies: [dep_paludis_util],
)

subdir('args')
subdir('resolver')

_tests = [
  'about',
  'broken_linkage_configuration',
  'comma_separated_dep_parser',
  'dep_spec',
  'elike_dep_parser',
  'elike_use_requirement',
  'environment_implementation',
  'filter',
  'filtered_generator',
  'fs_merger',
  'fuzzy_finder',
  'generator',
  'hooker',
  'name',
  'partitioning',
  'repository_name_cache',
  'selection',
  'set_file',
  'tar_merger',
  'user_dep_spec',
  'version_operator',
  'version_spec',
]

tt_env = test_env

sohooks = shared_library('paludissohooks_TEST', [
    'sohooks_TEST.cc',
  ],
  dependencies: [
     dep_paludis,
  ]
)

tt_env.append('PALUDIS_HOOKERTEST_LIB_PATH', sohooks.full_path())

foreach tt: _tests
  tt_src = f'@tt@_TEST.cc'
  tt_name = fs.stem(tt_src)
  tt_exe = executable(tt_name,
    tt_src,
    build_by_default: get_option('unit_tests'),
    dependencies: [
      dep_paludis,
      dep_gtest,
      dep_gmock
    ],
  )

  command = [ 'tt_args=$*' ]
  setup_name = f'@tt_name@_setup.sh'
  if fs.exists(setup_name)
    setup = fs.copyfile(files(setup_name))
    tt_setups += [setup]
    command += setup.full_path()
  endif

  command += tt_exe.full_path() + ' ${tt_args}'
  command += 'tt_ret=$?'

  tt_setups = []
  cleanup_name = f'@tt_name@_cleanup.sh'
  if fs.exists(cleanup_name)
    cleanup = fs.copyfile(files(cleanup_name))
    tt_setups += [cleanup]
    command += cleanup.full_path()
  endif

  command += 'exit ${tt_ret}'

  test(f'@tt@'.replace('_TEST', ''), prog_bash,
    args: ['-c', ';'.join(command), '--'],
    depends: [setup, cleanup],
    env: tt_env,
    protocol: 'gtest',
  )
endforeach

foreach tt : [] # _e_repo_dep_tests
  tt_src = _e_repo_srcdir / f'@tt@_TEST.cc'
  tt_name = fs.stem(tt_src)
  tt_exe = executable(tt_name,
    tt_src,
    build_by_default: false, #get_option('unit_tests'),
    dependencies: [dep_paludis, dep_gtest, dep_gmock],
  )

  test(tt, tt_exe,
    env: test_env,
    protocol: 'gtest',
  )
endforeach

foreach _suite : _repositories_test_suites
  _tests = 'gtest_tests' in _suite? _suite['gtest_tests'] : []
  _tests_src_dir = _suite['source_dir']
  # some of tests can take even a minute
  _suite_test_tmout = _suite.get('timeout', 30)

  foreach tt : _tests

    if not tt.contains('TEST')
      tt = f'@tt@_TEST'
    endif

    tt_src = _tests_src_dir / f'@tt@.cc'
    tt_name = fs.stem(tt_src)
    tt_exe = executable(tt_name,
      tt_src,
      build_by_default: get_option('unit_tests'),
      dependencies: [
        dep_paludis,
        dep_gtest,
        dep_gmock
      ],
    )

    tt_setups = []
    command = [ 'tt_args=$*' ]
    setup_name = _tests_src_dir / f'@tt_name@_setup.sh'
    if fs.exists(setup_name)
      setup = fs.copyfile(files(setup_name))
      tt_setups += [setup]
      command += setup.full_path()
    endif

    command += tt_exe.full_path() + ' ${tt_args}'
    command += 'tt_ret=$?'

    cleanup_name = _tests_src_dir / f'@tt_name@_cleanup.sh'
    if fs.exists(cleanup_name)
      cleanup = fs.copyfile(files(cleanup_name))
      tt_setups += [cleanup]
      command += cleanup.full_path()
    endif

    command += 'exit ${tt_ret}'

    test(f'@tt@'.replace('_TEST', ''),
      prog_bash,
      args: ['-c', ';'.join(command), '--'],
      depends: [tt_setups, sohooks],
      env: test_env,
      suite: _suite['suite_name'],
      workdir: meson.current_build_dir(),
      protocol: 'gtest',
      timeout: _suite_test_tmout,
    )
  endforeach
endforeach
