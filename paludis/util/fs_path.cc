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
#include <paludis/util/pimp-impl.hh>
#include <paludis/util/exception.hh>
#include <paludis/util/stringify.hh>
#include <paludis/util/options.hh>
#include <paludis/util/timestamp.hh>
#include <paludis/util/log.hh>
#include <paludis/util/sequence-impl.hh>
#include <paludis/util/set-impl.hh>
#include <paludis/util/wrapped_output_iterator-impl.hh>
#include <paludis/util/wrapped_forward_iterator-impl.hh>

#include <climits>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <errno.h>
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

template <> struct paludis::Imp<FSPath> {
  friend std::strong_ordering operator<=>(const Imp<FSPath> &me,
                                          const Imp<FSPath> &other) {
    return me.path <=> other.path;
  }

  path_t path;

  Imp(const path_t &p) : path(p) {}

  auto as_string() const -> std::string { return path.string(); }

  auto as_c_str() const noexcept { return path.c_str(); }
};

std::strong_ordering FSPath::compare(const FSPath &other) const noexcept {
  return _imp->path <=> other._imp->path;
}

bool paludis::operator==(const FSPath &me, const FSPath &other) noexcept {
  return std::strong_ordering::equal == (me <=> other);
}

bool paludis::operator!=(const FSPath &, const FSPath &) noexcept = default;

FSPath::FSPath(const std::string &path) try : _imp(path) {
  _normalise();
} catch (const std::exception &e) {
  Context c("When normalising FSPath path '" + path + "':");
  throw InternalError(PALUDIS_HERE,
                      "caught std::exception '" + stringify(e.what()) + "'");
}

FSPath::FSPath(const FSPath & other) :
    _imp(other._imp->path)
{
}

FSPath::~FSPath() = default;

FSStat
FSPath::stat() const
{
    return FSStat(*this);
}

FSPath &
FSPath::operator= (const FSPath & other)
{
    _imp->path = other._imp->path;
    return *this;
}

FSPath &
FSPath::operator/= (const FSPath & rhs)
{
  for (auto p : rhs._imp->path | rv::filter([](auto p) { return p != "/"; }))
    _imp->path /= p;
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

FSPath::operator path_t() const { return _imp->path; }

void
FSPath::_normalise()
{
  auto rev = rv::reverse(_imp->path);
  if (!rev.empty() && *rev.begin() == "")
    _imp->path = _imp->path.parent_path();
}

std::string FSPath::basename() const {
  if (not _imp->path.has_filename())
    return _imp->path.root_path();
  if (_imp->path == "." || _imp->path == "..")
    return _imp->path.string();
  return _imp->path.filename();
}

std::string FSPath::stem() const { return _imp->path.stem(); }

std::string FSPath::extension() const { return _imp->path.extension(); }

FSPath
FSPath::strip_leading(const FSPath & f) const
{
  auto mismatch = rs::mismatch(_imp->path, f._imp->path);

  if (mismatch.in2 != f._imp->path.end())
    throw FSError("Can't strip leading '" + stringify(f) + "' from FSPath '" +
                  _imp->as_string() + "'");

  auto r = path_t{"/"};

  for (auto part : rs::subrange(mismatch.in1, _imp->path.end()))
    r /= part;

  return FSPath{r};
}

bool
FSPath::starts_with(const FSPath & f) const
{
#if defined(__cpp_lib_ranges_starts_ends_with) &&                              \
    __cpp_lib_ranges_starts_ends_with >= 202106L
  return rs::starts_with(_imp->path, f.imp->path);
#else
  const auto mismatch = rs::mismatch(_imp->path, f._imp->path);

  return mismatch.in2 == std::end(f._imp->path);
#endif
}

FSPath
FSPath::dirname() const
{
  if (_imp->path == "." || _imp->path == "..")
    return FSPath{_imp->path.generic_string()};

  auto ret = _imp->path.parent_path().generic_string();
  return FSPath{ret};
}

FSPath
FSPath::realpath() const
{
  Context context("When fetching realpath of '" + _imp->as_string() + "':");

#ifdef HAVE_CANONICALIZE_FILE_NAME
    char * r(canonicalize_file_name(_imp->path.c_str()));
    if (! r)
      throw FSError("Could not resolve path '" + _imp->as_string() + "'");
    FSPath result(r);
    std::free(r);
    return result;
#else
    char r[PATH_MAX + 1];
    std::memset(r, 0, PATH_MAX + 1);
    if (! stat().exists())
      throw FSError("Could not resolve path '" + _imp->as_string() + "'");
    if (! ::realpath(_imp->path.c_str(), r))
      throw FSError("Could not resolve path '" + _imp->as_string() + "'");
    FSPath result(r);
    if (! result.stat().exists())
      throw FSError("Could not resolve path '" + _imp->as_string() + "'");
    return result;
#endif
}

FSPath
FSPath::realpath_if_exists() const
{
  Context context("When fetching realpath of '" + _imp->as_string() +
                  "', if it exists:");

#ifdef HAVE_CANONICALIZE_FILE_NAME
  char *r(canonicalize_file_name(_imp->as_c_str()));
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
    if (!::realpath(_imp->as_c_str(), r))
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
  s << f._imp->path.generic_string();
  return s;
}

bool
FSPath::mkdir(const mode_t mode, const FSPathMkdirOptions & options) const
{
  if (0 == ::mkdir(_imp->as_c_str(), mode))
    return true;

  int e(errno);
  if (e == EEXIST && options[fspmkdo_ok_if_exists]) {
    if (stat().is_directory())
      return false;
    throw FSError("mkdir '" + _imp->as_string() +
                  "' failed: target exists and is not a directory");
    }
    else
      throw FSError(errno, "mkdir '" + _imp->as_string() + "' failed");
}

bool
FSPath::symlink(const std::string & target) const
{
  if (0 == ::symlink(target.c_str(), _imp->as_c_str()))
    return true;

  int e(errno);
  if (e == EEXIST) {
    if (stat().is_symlink() && target == readlink())
      return false;
    throw FSError("symlink '" + _imp->as_string() + "' to '" + target +
                  "' failed: target exists");
    }
    else
      throw FSError(errno, "symlink '" + _imp->as_string() + "' to '" + target +
                               "' failed");
}

bool
FSPath::unlink() const
{
#ifdef HAVE_LCHFLAGS
    if (0 != ::lchflags(_imp->path.c_str(), 0))
    {
        int e(errno);
        if (e != ENOENT)
          throw FSError(e, "lchflags for unlink '" + _imp->as_string() +
                               "' failed");
    }
#endif

    if (0 == ::unlink(_imp->path.c_str()))
        return true;

    int e(errno);
    if (e == ENOENT)
        return false;
    else
      throw FSError(e, "unlink '" + _imp->as_string() + "' failed");
}

bool
FSPath::rmdir() const
{
    if (0 == ::rmdir(_imp->path.c_str()))
        return true;

    int e(errno);
    if (e == ENOENT)
        return false;
    else
      throw FSError(e, "rmdir '" + _imp->as_string() + "' failed");
}

bool
FSPath::utime(const Timestamp & t) const
{
    Context context("When setting utime for '" + stringify(_imp->path) + "':");

#ifdef HAVE_UTIMENSAT
    static bool utimensat_works(true);

    if (utimensat_works)
    {
        struct timespec ts[2] = { t.as_timespec(), t.as_timespec() };
        if (0 == ::utimensat(AT_FDCWD, _imp->path.c_str(), ts, 0))
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
          throw FSError(e, "utimensat '" + _imp->as_string() + "' failed");
    }
#endif

    struct timeval tv[2] = { t.as_timeval(), t.as_timeval() };
    if (0 == ::utimes(_imp->path.c_str(), tv))
        return true;

    int e(errno);
    if (e == ENOENT)
        return false;
    else
      throw FSError(e, "utimes '" + _imp->as_string() + "' failed");
}

std::string
FSPath::readlink() const
{
    char buf[PATH_MAX + 1];
    std::memset(buf, 0, PATH_MAX + 1);
    if (-1 == ::readlink(_imp->path.c_str(), buf, PATH_MAX))
      throw FSError(errno, "readlink '" + _imp->as_string() + "' failed");
    return buf;
}

void
FSPath::chown(const uid_t new_owner, const gid_t new_group) const
{
    if (0 != ::chown(_imp->path.c_str(), new_owner, new_group))
      throw FSError(errno, "chown '" + _imp->as_string() + "' to '" +
                               stringify(new_owner) + "', '" +
                               stringify(new_group) + "' failed");
}

void
FSPath::lchown(const uid_t new_owner, const gid_t new_group) const
{
  if (0 != ::lchown(_imp->as_c_str(), new_owner, new_group))
    throw FSError(errno, "lchown '" + _imp->as_string() + "' to '" +
                             stringify(new_owner) + "', '" +
                             stringify(new_group) + "' failed");
}

void
FSPath::chmod(const mode_t mode) const
{
  if (0 != ::chmod(_imp->as_c_str(), mode))
    throw FSError(errno, "chmod '" + _imp->as_string() + "' failed");
}

void
FSPath::rename(const FSPath & new_name) const
{
  if (0 != std::rename(_imp->as_c_str(), new_name._imp->as_c_str())) {
    throw FSError(errno, "rename('" + stringify(_imp->path) + "', '" +
                             stringify(new_name._imp->path) + "') failed");
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

template class paludis::Pimp<FSPath>;
