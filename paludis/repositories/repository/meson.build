
_srcs = [
  'repository_id.cc',
  'repository_repository.cc',
  'repository_repository_store.cc',
  'registration.cc',
]

paludis_repository_repository = shared_module('paludisrepositoryrepository', [
    _srcs,
  ],
  dependencies: [dep_paludis_util],
  build_by_default: false,
)

dep_paludis_repository_repository = declare_dependency(
  objects: paludis_repository_repository.extract_all_objects(recursive: false),
)
