project('paludis', ['cpp'], default_options: ['cpp_std=gnu++23'])

fs = import('fs')

inc_ = include_directories('.', is_system: true)

script_make_se = files('misc/make_se.bash')
script_make_nn = files('misc/make_nn.bash')

prog_bash = find_program('bash', required: true)
prog_m4 = find_program('m4', 'gm4', required: true)

prog_make_se = find_program(script_make_se, required: true)
prog_make_nn = find_program(script_make_nn, required: true)

cpp = meson.get_compiler('cpp')

checks_defines = []

if cpp.compiles('#include <cxxabi.h>', name: 'cxxabi.h')
  checks_defines += '-DHAVE_CXXABI_H'
  if cpp.compiles('''
#include <typeinfo>
#include <cxxabi.h>
#include <stdlib.h>
int main(void) {
  const char *name = typeid(name).name();
  int status;
  char *demangled = abi::__cxa_demangle(name, 0, 0, &status);
  free(demangled);
  return 0;
}
''', name: '__cxa_demangle')
    checks_defines += '-DHAVE_CXA_DEMANGLE'
  endif
endif

check_fns = [
  'signal',
  'grantpt',
  'unlockpt',
  'pipe2',
  'ptsname',
  'lchflags',
  'utimensat',
  'canonicalize_file_name',
]

foreach fn : check_fns
  has_fn = cpp.has_function(fn)
  var_name = f'have_@fn@'
  set_variable(var_name, has_fn)
  if has_fn
    checks_defines += f'-D@var_name@'.to_upper()
  endif
endforeach

have_sys_xattr_h = cpp.check_header('<sys/xattr.h>')
if have_sys_xattr_h
  checks_defines += '-DHAVE_SYS_XATTR_H'
  if cpp.compiles('''
    #include <sys/xattr.h>
        int main(void) {
          flistxattr(0, 0, 0);
          fgetxattr(0, 0, 0, 0);
          fsetxattr(0, 0, 0, 0, 0);
          return 0;
        }
    ''',
    name: 'f*xattr() functions family')
    checks_defines += '-DHAVE_XATTRS'
  endif
endif

if cpp.has_member('struct dirent', 'd_type', prefix: '''
#include <sys/types.h>
#include <dirent.h>
''')
  checks_defines += '-DHAVE_DIRENT_DTYPE'
endif

if cpp.compiles('''
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <linux/falloc.h>
int main(void) {
  return fallocate(0, FALLOC_FL_KEEP_SIZE, 0, 100);
}
''',
  name:'fallocate')
  checks_defines += '-DHAVE_FALLOCATE'
endif

if not cpp.compiles('''
template <typename>
struct F;

template <typename T_>
struct T {
  void f();
};

template <typename T_>
void T<T_>::f() { F<T_>::g(); }

extern template class T<int>;

int main(void) {
  T<int> f;
  f.f();
  return 0;
}
''',
  name: '-O3/extern template failure')
  error(cpp.get_id() + ' inlines extern template (GCC bug 39242?); try -O2 or -fno-inline-functions')
endif

if not cpp.compiles('''
  template <typename T_>
  struct S;

  template <>
  struct S<int> { };

  template class S<int>;
''', name: 'does not need explicit instantiation', args: '-Werror')
endif

add_project_arguments(checks_defines, language: 'cpp')

if cpp.get_id() == 'clang'
  add_project_arguments('-stdlib=libc++', language: 'cpp')
  add_project_link_arguments('-stdlib=libc++', language: 'cpp')
  if get_option('b_sanitize').contains('address')
    add_project_link_arguments('-fsanitize=address', language: 'cpp')
    add_project_link_arguments('-shared-libasan', language: 'cpp')
    cpp_prog = find_program(cpp.cmd_array()[-1])
    asan_arch = host_machine.cpu_family()
    r = run_command(cpp_prog, '--print-file-name', f'libclang_rt.asan-@asan_arch@.so')
    asan_path = fs.parent(r.stdout().strip())
    add_project_link_arguments(f'-Wl,-rpath=@asan_path@', language: 'cpp')
  endif
  proj_gtest = subproject('gtest', default_options: [
    'cpp_args=-stdlib=libc++',
    'cpp_std=' + get_option('cpp_std'),
  ])
  checks_defines += '-DPALUDIS_NO_EXPLICIT_FULLY_SPECIALISED=1'
endif

# Never found dependency
NEVER_dep = dependency('', required: false)
assert(not NEVER_dep.found())

# Always found dependency
ALWAYS_dep = declare_dependency()
assert(ALWAYS_dep.found())

dl_dep = cpp.find_library('dl', required : false)
threads_dep = dependency('threads')

dep_gtest = dependency('gtest_main', main: true)
dep_gmock = dependency('gmock')

dep_libarchive = dependency('libarchive', required: get_option('pbins'))
dep_catch2 = dependency('catch2-with-main')

default_repositories = ['e', 'fake', 'repository', 'unavailable', 'unpackaged', 'unwritten']
all_repositories = default_repositories + ['accounts', 'gemcutter']

repositories = get_option('repositories')

wants_all_repos = ('all' in repositories)
wants_default_repos = ('default' in repositories)
repo_names = ', '.join(repositories)

if repositories.length() > 1 and (wants_default_repos or wants_all_repos)
  warning(f'Option "repositories:" (@repo_names@)  has "all" or "default" ' +
          'which will overridde other selections!')
endif

repositories = wants_default_repos? default_repositories : repositories
repositories = wants_all_repos? all_repositories : repositories

pp_semicolon_hack = 'semicolon_goes_where {}'

repo_decls = all_repositories + [pp_semicolon_hack]
repository_group_decls = 'struct ' + '; struct '.join(repo_decls)

cfg_repositories = configuration_data({
  'REPOSITORY_GROUPS_DECLS': f'@repository_group_decls@'
})

repositories_enabler_deps = {}

foreach r : all_repositories
  r_enabled = repositories.contains(r)
  repositories_enabler_deps += {
    r : (r_enabled? ALWAYS_dep : NEVER_dep)
  }
  cfg_repositories.set(f'REPOSITORY_GROUP_IF_@r@', r_enabled ? r : '')
endforeach

default_environments = [ 'paludis', 'test', ]
all_environments = default_environments + ['portage']

environments = get_option('environments')

want_all_envs = 'all' in environments
want_default_envs = 'default' in environments

if (want_default_envs or want_all_envs) and environments.length()
  assert(environments.size() == 1)
endif

environments = want_default_envs? default_environments : environments
environments = want_all_envs? all_environments : environments

env_decls = all_environments + [pp_semicolon_hack]
environment_group_decls = 'struct ' + '; struct '.join(env_decls)

cfg_environments = configuration_data({
  'ENVIRONMENT_GROUPS_DECLS': f'@environment_group_decls@'
})

foreach e : all_environments
  e_enabled = environments.contains(e)
  cfg_environments.set(f'ENVIRONMENT_GROUP_IF_@e@',
    e_enabled ? f'environment_groups::@e@' : '')
endforeach

generated_warning = fs.read(files('misc/generated-file.txt'))

cfg_paludis = configuration_data({
  'HAVE_CXA_DEMANGLE': 1,
  'PALUDIS_DEFAULT_DISTRIBUTION': 'gentoo',
  'GENERATED_FILE' : generated_warning,
})

cfg_paludis.merge_from(cfg_repositories)
cfg_paludis.merge_from(cfg_environments)

paludis_config_h = configure_file(
  configuration: cfg_paludis,
  input: files('cmake/config.h.in'),
  output: 'config.h',
  format: 'cmake@',
)

dir_defines = []
paludis_sysconfdir = get_option('sysconfdir')
dir_defines += [f'-DSYSCONFDIR="@paludis_sysconfdir@"']

paludis_prefix = get_option('prefix')

gnu_inst_dirs = ['datadir', 'libdir', 'libexecdir']

foreach d: gnu_inst_dirs
  p = paludis_prefix / get_option(d)
  def = d.to_upper()
  dir_defines += f'-D@def@="@p@"'
  set_variable(f'paludis_@d@', p)
endforeach

dep_paludis_config_h = declare_dependency(
  sources: paludis_config_h,
  include_directories: include_directories('.'),
  compile_args: [
    dir_defines,
  ]
)

subdir('paludis')
