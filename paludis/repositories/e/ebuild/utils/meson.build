
subdir('4')

executable('print_exports', ['print_exports.cc'])
executable('locked_pipe_command', ['locked_pipe_command.cc'])
executable('strip_tar_corruption', ['strip_tar_corruption.cc'])

prep_template = files('prep.in')
foreach prep_suffix : 'all;allstrip;allman;allinfo;strip;man;info;docs;alldocs'.split(';')
  fs.copyfile(prep_template, f'prep@prep_suffix@')
endforeach
