_se_srcs = [
  'any_child_score.se',
  'change_type.se',
  'decision.se',
  'destination_types.se',
  'job_requirements.se',
  'nag.se',
  'promote_binaries.se',
  'resolver_functions.se',
  'use_existing.se',
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

dep_paludis_resolver_se = declare_dependency(
  sources: _hdrs_se_hh,
  dependencies: dep_paludis_config_h
)

_srcs = [
  'accumulate_deps.cc',
  'allow_choice_changes_helper.cc',
  'allowed_to_remove_helper.cc',
  'allowed_to_restart_helper.cc',
  'always_via_binary_helper.cc',
  'any_child_score.cc',
  'can_use_helper.cc',
  'change_by_resolvent.cc',
  'change_type.cc',
  'collect_depped_upon.cc',
  'collect_installed.cc',
  'collect_purges.cc',
  'collect_world.cc',
  'confirm_helper.cc',
  'constraint.cc',
  'decider.cc',
  'decision.cc',
  'decision_utils.cc',
  'decisions.cc',
  'destination.cc',
  'destination_types.cc',
  'destination_utils.cc',
  'find_replacing_helper.cc',
  'find_repository_for_helper.cc',
  'get_constraints_for_dependent_helper.cc',
  'get_constraints_for_purge_helper.cc',
  'get_constraints_for_via_binary_helper.cc',
  'get_destination_types_for_blocker_helper.cc',
  'get_destination_types_for_error_helper.cc',
  'get_initial_constraints_for_helper.cc',
  'get_resolvents_for_helper.cc',
  'get_sameness.cc',
  'get_use_existing_nothing_helper.cc',
  'has_behaviour.cc',
  'interest_in_spec_helper.cc',
  'job.cc',
  'job_list.cc',
  'job_lists.cc',
  'job_requirements.cc',
  'job_state.cc',
  'labels_classifier.cc',
  'make_destination_filtered_generator_helper.cc',
  'make_origin_filtered_generator_helper.cc',
  'make_uninstall_blocker.cc',
  'make_unmaskable_filter_helper.cc',
  'match_qpns.cc',
  'nag.cc',
  'order_early_helper.cc',
  'orderer.cc',
  'orderer_notes.cc',
  'package_id_comparator_with_promotion.cc',
  'package_or_block_dep_spec.cc',
  'prefer_or_avoid_helper.cc',
  'promote_binaries.cc',
  'promote_binaries_helper.cc',
  'reason.cc',
  'reason_utils.cc',
  'remove_hidden_helper.cc',
  'remove_if_dependent_helper.cc',
  'required_confirmations.cc',
  'resolution.cc',
  'resolutions_by_resolvent.cc',
  'resolved.cc',
  'resolvent.cc',
  'resolver.cc',
  'resolver_functions.cc',
  'same_slot.cc',
  'sanitised_dependencies.cc',
  'selection_with_promotion.cc',
  'slot_name_or_null.cc',
  'strongly_connected_component.cc',
  'suggest_restart.cc',
  'unsuitable_candidates.cc',
  'use_existing.cc',
  'why_changed_choices.cc',
]

libpaludisresolver = static_library('paludisresolver', [
    _srcs,
  ],
  dependencies: [
    dep_paludis_se,
    dep_paludis,
    dep_paludis_util,
    dep_paludis_util_se,
    dep_paludis_resolver_se,
  ],
)

dep_paludis_resolver = declare_dependency(
  link_with: libpaludisresolver,
  dependencies: [dep_paludis_resolver_se, dep_paludis_config_h],
)

libpaludisresolver_test = static_library('paludisresolvertest', [
    'resolver_test.cc',
  ],
  dependencies: [
    dep_paludis_resolver_se,
    dep_paludis_util_se,
    dep_paludis_fake_repository,
  ],
  build_by_default: get_option('unit_tests'),
)

dep_paludis_resolver_test = declare_dependency(
  link_with: libpaludisresolver_test,
)

_resolver_tests = [
  'any',
  'binaries',
  'continue_on_failure',
  'errors',
  'fetches',
  'purges',
  'blockers',
  'cycles',
  'serialisation',
  'simple',
  'subslots',
  'suggestions',
  'uninstalls',
]

foreach tt: _resolver_tests
  tt_src = f'resolver_TEST_@tt@.cc'
  tt_name = fs.stem(tt_src)
  tt_exe = executable(tt_name,
    tt_src,
    build_by_default: get_option('unit_tests'),
    dependencies: [
      dep_paludis_resolver,
      dep_paludis_resolver_test,
      dep_gtest,
      dep_gmock
    ],
  )

  tt_setups = []
  command = [ 'tt_args=$*' ]
  setup_name = f'@tt_name@_setup.sh'
  if fs.exists(setup_name)
    setup = fs.copyfile(files(setup_name))
    tt_setups += [setup]
    command += setup.full_path()
  endif

  command += tt_exe.full_path() + ' ${tt_args}'
  command += 'tt_ret=$?'

  cleanup_name = _e_repo_srcdir / f'@tt_name@_cleanup.sh'

  if fs.exists(cleanup_name)
    cleanup = fs.copyfile(files(cleanup_name))
    tt_setups += [cleanup]
    command += cleanup.full_path()
  endif

  command += 'exit ${tt_ret}'

  test(f'resolver_@tt@', prog_bash,
    args: ['-c', ';'.join(command), '--'],
    depends: [tt_setups],
    env: test_env,
    workdir: meson.current_build_dir(),
    protocol: 'gtest',
    suite: 'resolver',
  )
endforeach
