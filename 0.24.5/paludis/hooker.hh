/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2007 Ciaran McCreesh <ciaranm@ciaranm.org>
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

#ifndef PALUDIS_GUARD_PALUDIS_HOOKER_HH
#define PALUDIS_GUARD_PALUDIS_HOOKER_HH 1

#include <paludis/util/instantiation_policy.hh>
#include <paludis/util/private_implementation_pattern.hh>

namespace paludis
{
    class FSEntry;
    class Environment;
    class Hook;

    /**
     * Handles executing hooks for an Environment.
     *
     * \ingroup grphooker
     * \nosubgrouping
     */
    class Hooker :
        private PrivateImplementationPattern<Hooker>,
        private InstantiationPolicy<Hooker, instantiation_method::NonCopyableTag>
    {
        public:
            ///\name Basic operations
            ///\{

            Hooker(const Environment * const) PALUDIS_ATTRIBUTE((nonnull(1)));
            ~Hooker();

            ///\}

            /**
             * Perform a hook, return the highest exit status.
             */
            int perform_hook(const Hook &) const PALUDIS_ATTRIBUTE((warn_unused_result));

            /**
             * Add a new hook directory.
             */
            void add_dir(const FSEntry &, const bool output_prefixed);
    };
}

#endif
