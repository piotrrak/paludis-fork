_srcs = [
  'portage_environment.cc',
  'registration.cc',
]

paludis_portage_environment = shared_module('paludisportageenvironment', [
    _srcs,
  ],
  dependencies: [dep_paludis_util, dep_paludis_se],
  build_by_default: false,
)

dep_paludis_portage_environment = declare_dependency(
  objects: paludis_portage_environment.extract_all_objects(recursive: false),
)

