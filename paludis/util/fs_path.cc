/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2010, 2011 Ciaran McCreesh
 *
 * This file is part of the Paludis package manager. Paludis is free software;
 * you can redistribute it and/or modify it under the terms of the GNU General
 * Public License version 2, as published by the Free Software Foundation.
 *
 * Paludis is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <paludis/util/fs_path.hh>
#include <paludis/util/fs_error.hh>
#include <paludis/util/fs_stat.hh>
#include <paludis/util/exception.hh>
#include <paludis/util/stringify.hh>
#include <paludis/util/options.hh>
#include <paludis/util/timestamp.hh>
#include <paludis/util/log.hh>
#include <paludis/util/sequence-impl.hh>
#include <paludis/util/set-impl.hh>
#include <paludis/util/wrapped_output_iterator-impl.hh>
#include <paludis/util/wrapped_forward_iterator-impl.hh>

#include <cerrno>
#include <climits>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <filesystem>
#include <iomanip>
#include <ranges>
#include <string>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "config.h"

using namespace paludis;
using path_t = std::filesystem::path;

namespace rs = std::ranges;
namespace rv = rs::views;

namespace {

namespace paths {
const inline path_t dot{"."};
const inline path_t dotdot{".."};
const inline path_t root{"/"};
} // namespace paths

constexpr bool compat_starts_with(rs::input_range auto && r1, rs::input_range auto && r2)
{

#if defined(__cpp_lib_ranges_starts_ends_with) &&                              \
    __cpp_lib_ranges_starts_ends_with >= 202106L
  return rs::starts_with(r1, r2);
#else
  const auto mismatch = rs::mismatch(r1, r2);

  return mismatch.in2 == std::end(r2);
#endif
}

}

std::strong_ordering FSPath::compare(const FSPath &other) const noexcept {
  return _path <=> other._path;
}

bool paludis::operator==(const FSPath &me, const FSPath &other) noexcept {
  return std::strong_ordering::equal == (me <=> other);
}

bool paludis::operator!=(const FSPath &, const FSPath &) noexcept = default;

FSPath::FSPath(const std::string &path) : _path(_normalised(path)) {}

FSStat
FSPath::stat() const
{
    return FSStat(*this);
}

FSPath &
FSPath::operator/= (const FSPath & rhs)
{
  constexpr auto not_root_p = [](auto p) { return p != paths::root; };

  for (const auto &p : rhs._path | rv::filter(not_root_p))
    _path /= p;

  return *this;
}

FSPath &
FSPath::operator/= (const std::string & rhs)
{
    return operator/= (FSPath(rhs));
}

FSPath
FSPath::operator/ (const FSPath & rhs) const
{
    FSPath result(*this);
    result /= rhs;
    return result;
}

FSPath
FSPath::operator/ (const std::string & rhs) const
{
    return *this / FSPath(rhs);
}

FSPath::operator path_t() const { return _path; }

path_t FSPath::_normalised(path_t path) try {
  auto rev = rv::reverse(path);
  if (!rev.empty() && *rev.begin() == "")
    path = path.parent_path();
  return path;
} catch (const std::exception &e) {
  Context c("When normalising FSPath path '" + path.string() + "':");
  throw InternalError(PALUDIS_HERE,
                      "caught std::exception '" + stringify(e.what()) + "'");
}

void FSPath::_normalise() { _path = _normalised(_path); }

std::string FSPath::basename() const {
  if (not _path.has_filename())
    return _path.root_path();
  if (_path == paths::dot || _path == paths::dotdot)
    return _path.string();
  return _path.filename();
}

std::string FSPath::stem() const { return _path.stem(); }

std::string FSPath::extension() const { return _path.extension(); }

FSPath
FSPath::strip_leading(const FSPath & f) const
{
  auto mismatch = rs::mismatch(_path, f._path);

  if (mismatch.in2 != f._path.end())
    throw FSError("Can't strip leading '{}' from FSPath '{}'", stringify(f),
                  _path.string());

  auto r = paths::root;

  for (auto part : rs::subrange(mismatch.in1, _path.end()))
    r /= part;

  return FSPath{r};
}

bool
FSPath::starts_with(const FSPath & f) const
{
  return compat_starts_with(_path, f._path);
}

FSPath
FSPath::dirname() const
{
  if (_path == paths::dot || _path == paths::dotdot)
    return FSPath{_path};

  return FSPath{_path.parent_path()};
}

FSPath
FSPath::realpath() const
{
  Context context("When fetching realpath of '" + _path.string() + "':");

#ifdef HAVE_CANONICALIZE_FILE_NAME
  char *r(canonicalize_file_name(_path.c_str()));
  if (!r)
    throw FSError("Could not resolve path '{}'", _path.string());
  FSPath result(r);
  std::free(r);
  return result;
#else
    char r[PATH_MAX + 1];
    std::memset(r, 0, PATH_MAX + 1);
    if (! stat().exists())
      throw FSError("Could not resolve path '{}'", _path.string());
    if (!::realpath(_path.c_str(), r))
      throw FSError("Could not resolve path '{}'", _path.string());
    FSPath result(r);
    if (! result.stat().exists())
      throw FSError("Could not resolve path '{}'", _path.string());
    return result;
#endif
}

FSPath
FSPath::realpath_if_exists() const
{
  Context context("When fetching realpath of '" + _path.string() +
                  "', if it exists:");

#ifdef HAVE_CANONICALIZE_FILE_NAME
  char *r(canonicalize_file_name(_path.c_str()));
  if (!r)
    return *this;
  FSPath result(r);
  std::free(r);
  return result;
#else
    char r[PATH_MAX + 1];
    std::memset(r, 0, PATH_MAX + 1);
    if (! stat().exists())
        return *this;
    if (!::realpath(_path.c_str(), r))
      return *this;
    FSPath result(r);
    if (! result.stat().exists())
        return *this;
    return result;
#endif
}

FSPath
FSPath::cwd()
{
    char r[PATH_MAX + 1];
    std::memset(r, 0, PATH_MAX + 1);
    if (! ::getcwd(r, PATH_MAX))
        throw FSError("Could not get current working directory");
    return FSPath(r);
}

std::ostream &
paludis::operator<< (std::ostream & s, const FSPath & f)
{
  s << f._path.string();
  return s;
}

bool
FSPath::mkdir(const mode_t mode, const FSPathMkdirOptions & options) const
{
  if (0 == ::mkdir(_path.c_str(), mode))
    return true;

  int e(errno);
  if (e == EEXIST && options[fspmkdo_ok_if_exists]) {
    if (stat().is_directory())
      return false;
    throw FSError("mkdir '{}' failed: target exists and is not a directory",
                  _path.string());
    }
    else
      throw FSError(errno, "mkdir '{}' failed", _path.string());
}

bool
FSPath::symlink(const std::string & target) const
{
  if (0 == ::symlink(target.c_str(), _path.c_str()))
    return true;

  int e(errno);
  if (e == EEXIST) {
    if (stat().is_symlink() && target == readlink())
      return false;
    throw FSError("symlink '{}' to '{}' failed: target exists", _path.string(),
                  target);
    }
    else
      throw FSError(errno, "symlink '{}' to '{}' failed", _path.string(),
                    target);
}

bool
FSPath::unlink() const
{
#ifdef HAVE_LCHFLAGS
  if (0 != ::lchflags(_path.c_str(), 0)) {
    int e(errno);
    if (e != ENOENT)
      throw FSError(e, "lchflags for unlink '{}' failed", _path.string());
  }
#endif

  if (0 == ::unlink(_path.c_str()))
    return true;

  int e(errno);
  if (e == ENOENT)
    return false;
  else
    throw FSError(e, "unlink '{}' failed", _path.string());
}

bool
FSPath::rmdir() const
{
  if (0 == ::rmdir(_path.c_str()))
    return true;

  int e(errno);
  if (e == ENOENT)
    return false;
  else
    throw FSError(e, "rmdir '{}' failed", _path.string());
}

bool
FSPath::utime(const Timestamp & t) const
{
  Context context("When setting utime for '" + stringify(_path) + "':");

#ifdef HAVE_UTIMENSAT
    static bool utimensat_works(true);

    if (utimensat_works)
    {
        struct timespec ts[2] = { t.as_timespec(), t.as_timespec() };
        if (0 == ::utimensat(AT_FDCWD, _path.c_str(), ts, 0))
          return true;

        int e(errno);
        if (e == ENOENT)
            return false;
        else if (e == ENOSYS)
        {
            utimensat_works = false;
            Log::get_instance()->message("util.fs_entry.utime.utimensat_unimplemented", ll_debug, lc_context)
                << "utimensat(2) not implemented by this kernel, using utimes(2)";
        }
        else
          throw FSError(e, "utimensat '{}' failed", _path.string());
    }
#endif

    struct timeval tv[2] = { t.as_timeval(), t.as_timeval() };
    if (0 == ::utimes(_path.c_str(), tv))
      return true;

    int e(errno);
    if (e == ENOENT)
        return false;
    else
      throw FSError(e, "utimes '{}' failed", _path.string());
}

std::string
FSPath::readlink() const
{
    char buf[PATH_MAX + 1];
    std::memset(buf, 0, PATH_MAX + 1);
    if (-1 == ::readlink(_path.c_str(), buf, PATH_MAX))
      throw FSError(errno, "readlink '{}' failed", _path.string());
    return buf;
}

void
FSPath::chown(const uid_t new_owner, const gid_t new_group) const
{
  if (0 != ::chown(_path.c_str(), new_owner, new_group))
    throw FSError(errno, "chown '{}' to '{}', '{}' failed", _path.string(),
                  stringify(new_owner), stringify(new_group));
}

void
FSPath::lchown(const uid_t new_owner, const gid_t new_group) const
{
  if (0 != ::lchown(_path.c_str(), new_owner, new_group))
    throw FSError(errno, "lchown '{}' to '{}', '{}' failed", _path.string(),
                  stringify(new_owner), stringify(new_group));
}

void
FSPath::chmod(const mode_t mode) const
{
  if (0 != ::chmod(_path.c_str(), mode))
    throw FSError(errno, "chmod '{}' failed", _path.string());
}

void
FSPath::rename(const FSPath & new_name) const
{
  if (0 != std::rename(_path.c_str(), new_name._path.c_str())) {
    throw FSError(errno, "rename('{}', '{}') failed", stringify(_path),
                  stringify(new_name._path));
  }
}

bool FSPathComparator::operator()(const FSPath &a,
                                  const FSPath &b) const noexcept {
  return std::strong_ordering::less == (a <=> b);
}

template class PALUDIS_IF_GCC_VISIBLE paludis::Sequence<FSPath>;
template class PALUDIS_IF_GCC_VISIBLE paludis::WrappedForwardIterator<
    Sequence<FSPath>::ConstIteratorTag, const FSPath>;
template class PALUDIS_IF_GCC_VISIBLE paludis::WrappedForwardIterator<
    Sequence<FSPath>::ReverseConstIteratorTag, const FSPath>;
template class PALUDIS_IF_GCC_VISIBLE paludis::WrappedOutputIterator<Sequence<FSPath>::InserterTag,
                                              FSPath>;

template class PALUDIS_IF_GCC_VISIBLE paludis::Set<FSPath, FSPathComparator>;
template class PALUDIS_IF_GCC_VISIBLE paludis::WrappedForwardIterator<
    Set<FSPath, FSPathComparator>::ConstIteratorTag, const FSPath>;
