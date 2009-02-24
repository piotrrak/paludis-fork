/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2009 Ciaran McCreesh
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

#ifndef PALUDIS_GUARD_PALUDIS_CREATE_OUTPUT_MANAGER_INFO_HH
#define PALUDIS_GUARD_PALUDIS_CREATE_OUTPUT_MANAGER_INFO_HH 1

#include <paludis/create_output_manager_info-fwd.hh>
#include <paludis/util/private_implementation_pattern.hh>
#include <paludis/util/simple_visitor.hh>
#include <paludis/util/attributes.hh>
#include <paludis/util/type_list.hh>
#include <paludis/action-fwd.hh>
#include <paludis/package_id-fwd.hh>
#include <paludis/repository-fwd.hh>

namespace paludis
{
    /**
     * Information for Environment::create_output_manager.
     *
     * \since 0.36
     * \ingroup g_environment
     * \see Environment::create_output_manager
     */
    class PALUDIS_VISIBLE CreateOutputManagerInfo :
        public virtual DeclareAbstractAcceptMethods<CreateOutputManagerInfo, MakeTypeList<
            CreateOutputManagerForPackageIDActionInfo,
            CreateOutputManagerForRepositorySyncInfo
        >::Type>
    {
    };

    /**
     * Information for Environment::create_output_manager, if we're performing a
     * PackageID action.
     *
     * \since 0.36
     * \ingroup g_environment
     * \see Environment::create_output_manager
     */
    class PALUDIS_VISIBLE CreateOutputManagerForPackageIDActionInfo :
        private PrivateImplementationPattern<CreateOutputManagerForPackageIDActionInfo>,
        public CreateOutputManagerInfo,
        public ImplementAcceptMethods<CreateOutputManagerInfo, CreateOutputManagerForPackageIDActionInfo>
    {
        public:
            CreateOutputManagerForPackageIDActionInfo(
                    const std::tr1::shared_ptr<const PackageID> & id,
                    const Action & action,
                    const OutputExclusivity output_exclusivity);

            ~CreateOutputManagerForPackageIDActionInfo();

            const std::tr1::shared_ptr<const PackageID> package_id() const PALUDIS_ATTRIBUTE((warn_unused_result));
            const Action & action() const PALUDIS_ATTRIBUTE((warn_unused_result));
            const OutputExclusivity output_exclusivity() const PALUDIS_ATTRIBUTE((warn_unused_result));
    };

    /**
     * Information for Environment::create_output_manager, if we're performing a
     * Repository sync.
     *
     * \since 0.36
     * \ingroup g_environment
     * \see Environment::create_output_manager
     */
    class PALUDIS_VISIBLE CreateOutputManagerForRepositorySyncInfo :
        private PrivateImplementationPattern<CreateOutputManagerForRepositorySyncInfo>,
        public CreateOutputManagerInfo,
        public ImplementAcceptMethods<CreateOutputManagerInfo, CreateOutputManagerForRepositorySyncInfo>
    {
        public:
            CreateOutputManagerForRepositorySyncInfo(
                    const Repository & repo,
                    const OutputExclusivity);

            ~CreateOutputManagerForRepositorySyncInfo();

            const Repository & repository() const PALUDIS_ATTRIBUTE((warn_unused_result));
            const OutputExclusivity output_exclusivity() const PALUDIS_ATTRIBUTE((warn_unused_result));
    };
}

#endif