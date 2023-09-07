
_srcs = [
  'unwritten_repository.cc',
  'unwritten_id.cc',
  'unwritten_mask.cc',
  'unwritten_repository_store.cc',
  'unwritten_repository_file.cc',
  'registration.cc',
]

paludis_unwritten_repository = shared_module('paludisunwrittenrepository', [
    _srcs,
  ],
  dependencies: [dep_paludis_util],
  build_by_default: false,
)

dep_paludis_unwritten_repository = declare_dependency(
  objects: paludis_unwritten_repository.extract_all_objects(recursive: false),
)

_unwritten_repo_tests = [
  'unwritten_repository',
]

_unwritten_repo_srcdir = meson.current_source_dir()

_unwritten_repo_suite = {
  'source_dir' : _unwritten_repo_srcdir,
  'gtest_tests' : _unwritten_repo_tests,
  'suite_name' : 'unwritten_repo',
}

_repositories_test_suites += [
  _unwritten_repo_suite,
]
