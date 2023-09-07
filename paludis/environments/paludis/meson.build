
_srcs = [
  'bashable_conf.cc',
  'extra_distribution_data.cc',
  'keywords_conf.cc',
  'licenses_conf.cc',
  'mirrors_conf.cc',
  'output_conf.cc',
  'package_mask_conf.cc',
  'paludis_config.cc',
  'paludis_environment.cc',
  'suggestions_conf.cc',
  'use_conf.cc',
  'world.cc',
  'registration.cc',
]

paludis_paludis_environment = shared_module('paludispaludisenvironment', [
    _srcs,
  ],
  dependencies: [dep_paludis_util, dep_paludis_se],
  cpp_args: [f'-DSHAREDIR="@paludis_datadir@"'],
  build_by_default: false,
)

dep_paludis_paludis_environment = declare_dependency(
  objects: paludis_paludis_environment.extract_all_objects(recursive: false),
)
