/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2006 Ciaran McCreesh <ciaranm@gentoo.org>
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

#ifndef PALUDIS_GUARD_PALUDIS_QA_CHECK_HH
#define PALUDIS_GUARD_PALUDIS_QA_CHECK_HH 1

#include <paludis/instantiation_policy.hh>
#include <string>

/** \file
 * Declarations for the paludis::qa::Check class.
 *
 * \ingroup QA
 */

namespace paludis
{
    namespace qa
    {
        /**
         * Base class for a QA check.
         *
         * \ingroup QA
         */
        class Check :
            private InstantiationPolicy<Check, instantiation_method::NonCopyableTag>
        {
            protected:
                Check();

                virtual ~Check();

            public:
                virtual std::string describe() const = 0;

                virtual bool is_important() const
                {
                    return false;
                }
        };
    }
}

#endif
