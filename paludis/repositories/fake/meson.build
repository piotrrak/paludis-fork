
_srcs = [
  'dep_parser.cc',
  'fake_repository_base.cc',
  'fake_repository.cc',
  'fake_installed_repository.cc',
  'fake_package_id.cc',
  'registration.cc',
]

paludis_fake_repository = shared_module('paludisfakerepository', [
  _srcs,
  ],
  dependencies: [dep_paludis_util],
  build_by_default: false,
)

dep_paludis_fake_repository = declare_dependency(
  objects: paludis_fake_repository.extract_all_objects(recursive: false),
)

_fake_repo_tests = [
  'fake_repository',
  'fake_installed_repository',
]

_fake_repo_srcdir = meson.current_source_dir()

_fake_repo_suite = {
  'source_dir' : _fake_repo_srcdir,
  'gtest_tests' : _fake_repo_tests,
  'suite_name' : 'fake_repo',
}

_repositories_test_suites += [
  _fake_repo_suite,
]

