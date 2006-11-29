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

#ifndef PALUDIS_GUARD_PALUDIS_QA_FILES_DIR_SIZE_CHECK_HH
#define PALUDIS_GUARD_PALUDIS_QA_FILES_DIR_SIZE_CHECK_HH 1

#include <paludis/qa/package_dir_check.hh>
#include <string>

namespace paludis
{
    namespace qa
    {
        /**
         * QA check: is files/ too big.
         *
         * \ingroup grpqacheck
         */
        class FilesDirSizeCheck :
            public PackageDirCheck
        {
            public:
                FilesDirSizeCheck();

                CheckResult operator() (const FSEntry &) const;

                static const std::string & identifier();

                virtual std::string describe() const
                {
                    return "Checks that files/ (excluding digests) is below 20k";
                }
        };

        static const PackageDirCheckMaker::RegisterMaker register_files_dir_size_check(
                FilesDirSizeCheck::identifier(), &MakePackageDirCheck<FilesDirSizeCheck>::make_package_dir_check);
    }
}

#endif
