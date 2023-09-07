
_srcs = [
  'test_environment.cc',
  'registration.cc',
]

paludis_test_environment = shared_module('paludistestenvironment', [
    _srcs,
  ],
  dependencies: [dep_paludis_util, dep_paludis_se ],
  build_by_default: false,
)

dep_paludis_test_environment = declare_dependency(
  objects: paludis_test_environment.extract_all_objects(recursive: false),
)
