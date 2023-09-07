
_srcs = [
  'unavailable_repository.cc',
  'unavailable_package_id.cc',
  'unavailable_repository_id.cc',
  'unavailable_repository_dependencies_key.cc',
  'unavailable_mask.cc',
  'unavailable_repository_store.cc',
  'unavailable_repository_file.cc',
  'registration.cc',
]

paludis_unavailable_repository = shared_module('paludisunavailablerepository', [
    _srcs,
  ],
  dependencies: [dep_paludis_util],
  build_by_default: false,
)

dep_paludis_unavailable_repository = declare_dependency(
  objects: paludis_unavailable_repository.extract_all_objects(recursive: false),
)

_unavailable_repo_srcdir = meson.current_source_dir()

_unavailable_repo_tests = [
  'unavailable_repository'
]

_unavailable_repo_suite = {
  'source_dir' : _unavailable_repo_srcdir,
  'gtest_tests' : _unavailable_repo_tests,
  'suite_name' : 'unavailable_repo',
}

_repositories_test_suites += [
  _unavailable_repo_suite,
]

