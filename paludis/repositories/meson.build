
_repositories_test_suites = []

_prio_repository_tests = 100
_old_len = 0

foreach _r : all_repositories
  _enabler =  repositories_enabler_deps[_r]
  subdir(_r, if_found: _enabler)
endforeach

_updated = []
foreach _si : range(_repositories_test_suites.length())
  _suite = _repositories_test_suites[_si]
  _suite += {'priority' : _prio_repository_tests}
  _name = 'suite_name' in _suite ? _suite['suite_name'] : []
  _suite += { 'suite_name': ['repo'] + _name }
  _updated += _suite
endforeach

_repositories_test_suites = _updated

