subdir('ebuild')

se_srcs = [
  'dep_parser.se',
  'iuse.se',
  'e_repository_params.se',
]

_hdrs_se_hh = []
_srcs_se_cc = []

foreach se : se_srcs
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
  'a_finder.cc',
  'aa_visitor.cc',
  'can_skip_phase.cc',
  'check_fetched_files_visitor.cc',
  'check_userpriv.cc',
  'dep_parser.cc',
  'do_fetch_action.cc',
  'do_info_action.cc',
  'do_install_action.cc',
  'do_pretend_action.cc',
  'do_pretend_fetch_action.cc',
  'e_choice_value.cc',
  'e_installed_repository.cc',
  'e_installed_repository_id.cc',
  'e_choices_key.cc',
  'e_key.cc',
  'e_keywords_key.cc',
  'e_mask.cc',
  'e_repository.cc',
  'e_repository_exceptions.cc',
  'e_repository_id.cc',
  'e_repository_news.cc',
  'e_repository_params.cc',
  'e_repository_sets.cc',
  'e_slot_key.cc',
  'e_string_set_key.cc',
  'e_stripper.cc',
  'eapi.cc',
  'eapi_phase.cc',
  'ebuild.cc',
  'ebuild_flat_metadata_cache.cc',
  'ebuild_id.cc',
  'eclass_mtimes.cc',
  'exndbam_id.cc',
  'exndbam_repository.cc',
  'exheres_layout.cc',
  'exheres_mask_store.cc',
  'exheres_profile.cc',
  'extra_distribution_data.cc',
  'fetch_visitor.cc',
  'file_suffixes.cc',
  'info_metadata_key.cc',
  'iuse.cc',
  'pretend_fetch_visitor.cc',
  'fix_locked_dependencies.cc',
  'glsa.cc',
  'layout.cc',
  'licence_groups.cc',
  'make_archive_strings.cc',
  'make_use.cc',
  'manifest2_reader.cc',
  'mask_info.cc',
  'memoised_hashes.cc',
  'metadata_xml.cc',
  'myoption.cc',
  'myoptions_requirements_verifier.cc',
  'parse_annotations.cc',
  'parse_dependency_label.cc',
  'parse_plain_text_label.cc',
  'parse_uri_label.cc',
  'pbin_merger.cc',
  'permitted_directories.cc',
  'pipe_command_handler.cc',
  'profile.cc',
  'registration.cc',
  'required_use_verifier.cc',
  'source_uri_finder.cc',
  'spec_tree_pretty_printer.cc',
  'traditional_layout.cc',
  'traditional_mask_file.cc',
  'traditional_mask_store.cc',
  'traditional_profile.cc',
  'traditional_profile_file.cc',
  'use_desc.cc',
  'xml_things_handle.cc',
  'vdb_id.cc',
  'vdb_merger.cc',
  'vdb_repository.cc',
  'vdb_unmerger.cc',
]

_ebuild_suffixes_conf = 'ebuild_entries_suffixes.conf'
assert(fs.exists(_ebuild_suffixes_conf))
test_env.set('PALUDIS_SUFFIXES_FILE', meson.current_source_dir() / _ebuild_suffixes_conf)

paludis_e_repository = static_library('paludiserepository', [
    _hdrs_se_hh,
    _srcs,
  ],
  dependencies: [dep_paludis_util],
  build_by_default: false,
)

dep_paludis_e_repository = declare_dependency(
  objects: paludis_e_repository.extract_all_objects(recursive: false),
  dependencies: [dep_paludis_util],
)

_e_repo_dep_tests = [
  'aa_visitor',
  'dep_parser',
  'fix_locked_dependencies',
  'source_uri_finder',
]

_e_repo_tests = [
  'vdb_repository',
  'vdb_repository_TEST_eapis',
  'vdb_repository_TEST_cache',
  'e_repository',
  'e_repository_TEST_0',
  'e_repository_TEST_1',
  'e_repository_TEST_2',
  'e_repository_TEST_3',
  'e_repository_TEST_4',
  'e_repository_TEST_5',
  'e_repository_TEST_6',
  'e_repository_TEST_ever',
  'e_repository_TEST_exheres_0',
  'e_repository_TEST_exlibs',
  'e_repository_TEST_phases',
  'e_repository_TEST_replacing',
  'e_repository_TEST_symlink_rewriting',
  'exndbam_repository',
  'depend_rdepend',
  'e_repository_sets',
  'ebuild_flat_metadata_cache',
  'fetch_visitor',
  'vdb_merger',
  'vdb_unmerger',
]

_e_repo_srcdir = meson.current_source_dir()

_e_repo_suite = {
  'source_dir' : _e_repo_srcdir,
  'gtest_tests' : _e_repo_tests,
  'suite_name' : 'e_repo',
  'timeout' : 90,
}

_e_repo_dep_suite = {
  'source_dir' : _e_repo_srcdir,
  'gtest_tests' : _e_repo_dep_tests,
  'suite_name' : ['e_repo' ,'e_repo_deps'],
}

_repositories_test_suites += [
  _e_repo_suite,
  _e_repo_dep_suite,
]
