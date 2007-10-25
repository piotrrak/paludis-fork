/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2005, 2006, 2007 Ciaran McCreesh
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

#ifndef PALUDIS_GUARD_PALUDIS_REPOSITORY_FWD_HH
#define PALUDIS_GUARD_PALUDIS_REPOSITORY_FWD_HH 1

#include <paludis/util/set-fwd.hh>
#include <paludis/util/tr1_memory.hh>

/** \file
 * Forward declarations for paludis/repository.hh .
 *
 * \ingroup g_repository
 */

namespace paludis
{
    class NoSuchSetError;
    class RecursivelyDefinedSetError;

    class Environment;
    class RepositoryNameCache;
    class ERepositoryProfile;
    class Hook;
    class HookResult;

    class Repository;
    class RepositoryInstalledInterface;
    class RepositorySetsInterface;
    class RepositorySyncableInterface;
    class RepositoryUseInterface;
    class RepositoryWorldInterface;
    class RepositoryEnvironmentVariableInterface;
    class RepositoryMirrorsInterface;
    class RepositoryProvidesInterface;
    class RepositoryVirtualsInterface;
    class RepositoryMakeVirtualsInterface;
    class RepositoryDestinationInterface;
    class RepositoryLicensesInterface;
    class RepositoryEInterface;
    class RepositoryHookInterface;
    class RepositoryQAInterface;
    class RepositoryManifestInterface;

    class MergeOptions;

    /**
     * A set of destinations, used to decide whether a PackageID can be
     * installed to a particular Repository.
     *
     * \ingroup g_repository
     */
    typedef Set<paludis::tr1::shared_ptr<Repository> > DestinationsSet;
}

#endif
