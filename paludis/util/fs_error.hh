/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2005, 2006, 2007, 2008, 2009, 2010 Ciaran McCreesh
 * Copyright (c) 2006 Mark Loeser
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

#ifndef PALUDIS_GUARD_PALUDIS_FS_ERROR_HH
#define PALUDIS_GUARD_PALUDIS_FS_ERROR_HH 1

#include <paludis/util/exception.hh>

#include <format>

namespace paludis
{
    /**
     * Generic filesystem error class.
     *
     * \ingroup g_exceptions
     * \ingroup g_fs
     * \nosubgrouping
     */
    class PALUDIS_VISIBLE FSError :
        public Exception
    {
        public:
            ///\name Basic operations
            ///\{

            FSError(const std::string & message) noexcept;
            FSError(int error, const std::string & message) noexcept;

            template <std::formattable<char>... Args>
            FSError(std::format_string<Args...> fmt, Args &&...args) noexcept
                : FSError{std::format(fmt, std::forward<Args>(args)...)} {};

            template <std::formattable<char>... Args>
            FSError(int error, std::format_string<Args...> fmt,
                    Args &&...args) noexcept
                : FSError{error,
                          std::format(fmt, std::forward<Args>(args)...)} {};
            ///\}
    };
}

#endif
