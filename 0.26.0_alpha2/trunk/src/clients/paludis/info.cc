/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2007 Ciaran McCreesh
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

#include "info.hh"
#include "command_line.hh"
#include "src/output/colour.hh"
#include <paludis/about.hh>
#include <paludis/util/iterator.hh>
#include <paludis/util/sequence.hh>
#include <paludis/package_database.hh>
#include <paludis/environment.hh>
#include <paludis/repository_info.hh>
#include <paludis/query.hh>
#include <paludis/package_id.hh>
#include <paludis/action.hh>
#include <iostream>
#include <iomanip>
#include <libebt/libebt.hh>
#include <libwrapiter/libwrapiter.hh>

using namespace paludis;
using std::endl;
using std::cout;

int do_one_info(
        const tr1::shared_ptr<const Environment> & env,
        const std::string & q)
{
    Context local_context("When handling query '" + q + "':");

    tr1::shared_ptr<PackageDepSpec> spec;
    if (std::string::npos != q.find('/'))
        spec.reset(new PackageDepSpec(q, pds_pm_permissive));
    else
        spec.reset(new PackageDepSpec(tr1::shared_ptr<QualifiedPackageName>(new QualifiedPackageName(
                            env->package_database()->fetch_unique_qualified_package_name(PackageNamePart(q))))));

    tr1::shared_ptr<const PackageIDSequence>
        entries(env->package_database()->query(query::Matches(*spec), qo_order_by_version)),
        preferred_entries(env->package_database()->query(
                    query::Matches(*spec) & query::InstalledAtRoot(env->root()), qo_order_by_version));

    if (entries->empty())
        throw NoSuchPackageError(q);
    if (preferred_entries->empty())
        preferred_entries = entries;

    tr1::shared_ptr<const PackageID> display_entry(*preferred_entries->last());
        for (PackageIDSequence::ConstIterator i(preferred_entries->begin()),
                i_end(preferred_entries->end()) ; i != i_end ; ++i)
            if (! (*i)->masked())
                display_entry = *i;

    InfoAction a;
    try
    {
        cout << "Package " << colour(cl_package_name, *display_entry) << ":" << endl;
        cout << endl;
        display_entry->perform_action(a);
        cout << endl;
    }
    catch (const UnsupportedActionError &)
    {
        cout << "        No extra information available for '" << *display_entry << "'" << endl;
        cout << endl;
        return 1;
    }

    return 0;
}

int
do_info(const tr1::shared_ptr<const Environment> & env)
{
    int return_code(0);

    cout << "Paludis build information:" << endl;

    cout << "    " << colour(cl_heading, "Compiler:") << endl;
    cout << "        " << std::setw(22) << std::left << ("CXX:") << std::setw(0) << " " << PALUDIS_BUILD_CXX
#if defined(__ICC)
        << " " << __ICC
#elif defined(__VERSION__)
        << " " << __VERSION__
#endif
        << endl;

    cout << "        " << std::setw(22) << std::left << ("CXXFLAGS:") << std::setw(0) << " " << PALUDIS_BUILD_CXXFLAGS << endl;
    cout << "        " << std::setw(22) << std::left << ("LDFLAGS:") << std::setw(0) << " " << PALUDIS_BUILD_LDFLAGS << endl;
    cout << "        " << std::setw(22) << std::left << ("DATE:") << std::setw(0) << " " << PALUDIS_BUILD_DATE << endl;

    cout << endl;

    cout << "    " << colour(cl_heading, "Libraries:") << endl;
    cout << "        " << std::setw(22) << std::left << ("C++ Library:") << std::setw(0) << " "
#if defined(__GLIBCXX__)
#  define XSTRINGIFY(x) #x
#  define STRINGIFY(x) XSTRINGIFY(x)
        << "GNU libstdc++ " << STRINGIFY(__GLIBCXX__)
#endif
        << endl;
    cout << "        " << std::setw(22) << std::left << ("libebt:") << std::setw(0) << " "
        << LIBEBT_VERSION_MAJOR << "." << LIBEBT_VERSION_MINOR << "." << LIBEBT_VERSION_MICRO << endl;
    cout << "        " << std::setw(22) << std::left << ("libwrapiter:") << std::setw(0) << " "
        << LIBWRAPITER_VERSION_MAJOR << "." << LIBWRAPITER_VERSION_MINOR << "." << LIBWRAPITER_VERSION_MICRO << endl;

    cout << endl;

    cout << "    " << colour(cl_heading, "Paths:") << endl;

    cout << "        " << std::setw(22) << std::left << ("DATADIR:") << std::setw(0) << " " << DATADIR << endl;
    cout << "        " << std::setw(22) << std::left << ("LIBDIR:") << std::setw(0) << " " << LIBDIR << endl;
    cout << "        " << std::setw(22) << std::left << ("LIBEXECDIR:") << std::setw(0) << " " << LIBEXECDIR << endl;
    cout << "        " << std::setw(22) << std::left << ("SYSCONFDIR:") << std::setw(0) << " " << SYSCONFDIR << endl;
    cout << "        " << std::setw(22) << std::left << ("PYTHONINSTALLDIR:") << std::setw(0) << " " << PYTHONINSTALLDIR << endl;
    cout << "        " << std::setw(22) << std::left << ("RUBYINSTALLDIR:") << std::setw(0) << " " << RUBYINSTALLDIR << endl;

    cout << endl;

    for (IndirectIterator<PackageDatabase::RepositoryConstIterator, const Repository>
            r(env->package_database()->begin_repositories()), r_end(env->package_database()->end_repositories()) ;
            r != r_end ; ++r)
    {
        cout << "Repository " << colour(cl_repository_name, r->name()) << ":" << endl;

        tr1::shared_ptr<const RepositoryInfo> ii(r->info(true));
        for (RepositoryInfo::SectionConstIterator i(ii->begin_sections()),
                i_end(ii->end_sections()) ; i != i_end ; ++i)
        {
            cout << "    " << colour(cl_heading, (*i)->heading() + ":") << endl;
            for (RepositoryInfoSection::KeyValueConstIterator k((*i)->begin_kvs()),
                    k_end((*i)->end_kvs()) ; k != k_end ; ++k)
                cout << "        " << std::setw(22) << std::left << (stringify(k->first) + ":")
                    << std::setw(0) << " " << k->second << endl;
            cout << endl;
        }
    }

    if (CommandLine::get_instance()->empty())
    {
        cout << endl;
        cout << "No packages were specified on the command line, so detailed information is not" << endl;
        cout << "available (Paludis can display detailed information for both installed and" << endl;
        cout << "installable packages)." << endl;
        cout << endl;
    }
    else
    {
        for (CommandLine::ParametersConstIterator q(CommandLine::get_instance()->begin_parameters()),
                q_end(CommandLine::get_instance()->end_parameters()) ;
                q != q_end ; ++q)
        {
            try
            {
                return_code |= do_one_info(env, *q);
            }
            catch (const AmbiguousPackageNameError & e)
            {
                cout << endl;
                cout << "Query error:" << endl;
                cout << "  * " << e.backtrace("\n  * ");
                cout << "Ambiguous package name '" << e.name() << "'. Did you mean:" << endl;
                for (AmbiguousPackageNameError::OptionsConstIterator o(e.begin_options()),
                        o_end(e.end_options()) ; o != o_end ; ++o)
                    cout << "    * " << colour(cl_package_name, *o) << endl;
                cout << endl;
                return_code |= 1;
            }
        }
    }

    return return_code;
}

