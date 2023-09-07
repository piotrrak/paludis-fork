
_srcs = [
  'exceptions.cc',
  'unpackaged_id.cc',
  'unpackaged_repository.cc',
  'unpackaged_key.cc',
  'unpackaged_stripper.cc',
  'installed_repository.cc',
  'installed_id.cc',
  'registration.cc',
]

paludis_unpackaged_repository = shared_module('paludisunpackagedrepository', [
    _srcs,
  ],
  dependencies: [dep_paludis_util],
  build_by_default: false,
)

dep_paludis_unpackaged_repository = declare_dependency(
  objects: paludis_unpackaged_repository.extract_all_objects(recursive: false),
)

_unpackaged_repo_tests = [
  'unpackaged_repository',
  'installed_repository',
]

_unpackaged_repo_srcdir = meson.current_source_dir()

_unpackaged_repo_suite = {
  'source_dir' : _unpackaged_repo_srcdir,
  'gtest_tests' : _unpackaged_repo_tests,
  'suite_name' : 'unpackaged_repo',
}

_repositories_test_suites += [
  _unpackaged_repo_suite,
]
