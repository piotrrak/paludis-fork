
paludis_accounts_repository = shared_module('paludisaccountsrepository', [
    'accounts_exceptions.cc',
    'accounts_handler.cc',
    'accounts_repository.cc',
    'accounts_repository_store.cc',
    'accounts_id.cc',
    'accounts_dep_key.cc',
    'accounts_installed_mask.cc',
    'installed_accounts_id.cc',
    'dummy_accounts_handler.cc',
    'passwd_accounts_handler.cc',
    'registration.cc',
  ],
  dependencies: [dep_paludis_util, dep_paludis_se],
  build_by_default: false,
)

dep_paludis_accounts_repository = declare_dependency(
  objects: paludis_accounts_repository.extract_all_objects(recursive: false),
)

_accounts_repo_tests = [
  'accounts_repository'
]

_accounts_repo_srcdir = meson.current_source_dir()

_accounts_repo_suite = {
  'source_dir' : _accounts_repo_srcdir,
  'gtest_tests' : _accounts_repo_tests,
  'suite_name' : 'accounts_repo',
}

_repositories_test_suites += [ _accounts_repo_suite, ]
