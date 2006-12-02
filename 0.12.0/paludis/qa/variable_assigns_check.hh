/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2006 Ciaran McCreesh <ciaranm@ciaranm.org>
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

#ifndef PALUDIS_GUARD_PALUDIS_QA_VARIABLE_ASSIGNS_CHECK_HH
#define PALUDIS_GUARD_PALUDIS_QA_VARIABLE_ASSIGNS_CHECK_HH 1

#include <paludis/qa/file_check.hh>
#include <string>

namespace paludis
{
    namespace qa
    {
        /**
         * QA check: bad variable assignments.
         *
         * \ingroup grpqacheck
         */
        class VariableAssignsCheck :
            public FileCheck
        {
            public:
                VariableAssignsCheck();

                CheckResult operator() (const FSEntry &) const;

                static const std::string & identifier();

                virtual std::string describe() const
                {
                    return "Checks for illegal variable assigns";
                }
        };

        static const FileCheckMaker::RegisterMaker register_variable_assigns_check(
                VariableAssignsCheck::identifier(), &MakeFileCheck<VariableAssignsCheck>::make_file_check);
    }
}

#endif
