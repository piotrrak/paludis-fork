/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2006, 2007 Ciaran McCreesh <ciaranm@ciaranm.org>
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

#include "no_config_environment.hh"
#include <paludis/util/collection_concrete.hh>
#include <paludis/util/tokeniser.hh>
#include <paludis/util/log.hh>
#include <paludis/util/dir_iterator.hh>
#include <paludis/repositories/portage/portage_repository.hh>
#include <paludis/repositories/repository_maker.hh>
#include <paludis/config_file.hh>
#include <set>

using namespace paludis;

#include <paludis/environment/no_config/no_config_environment-sr.cc>

namespace paludis
{
    template<>
    struct Implementation<NoConfigEnvironment>
    {
        const FSEntry top_level_dir;
        const FSEntry write_cache;
        bool accept_unstable;
        bool is_vdb;

        std::tr1::shared_ptr<PortageRepository> portage_repo;
        std::tr1::shared_ptr<PortageRepository> master_repo;

        Implementation(Environment * const env, const NoConfigEnvironmentParams & params);
    };

    /* This goat is for Dave Wickham */
}

namespace
{
    bool is_vdb_repository(const FSEntry & location, NoConfigEnvironmentRepositoryType type)
    {
        switch (type)
        {
            case ncer_portage:
                return false;
            case ncer_vdb:
                return true;
            case ncer_auto:
                ;
        }

        Context context("When determining repository type at '" + stringify(location) + "':");

        if (! location.is_directory())
            throw ConfigurationError("Location is not a directory");

        if ((location / "profiles").is_directory())
        {
            Log::get_instance()->message(ll_debug, lc_context, "Found profiles/, looks like Portage format");
            return false;
        }

        int outer_count(0);
        for (DirIterator d(location), d_end ; d != d_end ; ++d)
        {
            if (! d->is_directory())
                continue;

            int inner_count(0);
            for (DirIterator e(*d), e_end ; e != e_end ; ++e)
            {
                if (! e->is_directory())
                    continue;

                if ((*e / "CONTENTS").exists())
                {
                    Log::get_instance()->message(ll_debug, lc_context, "Found '" + stringify(*e) +
                            "/CONTENTS', looks like VDB format");
                    return true;
                }

                if (inner_count++ >= 5)
                    break;
            }

            if (outer_count++ >= 5)
                break;
        }

        throw ConfigurationError("Can't work out what kind of repository this is");
    }
}

Implementation<NoConfigEnvironment>::Implementation(
        Environment * const env, const NoConfigEnvironmentParams & params) :
    top_level_dir(params.repository_dir),
    write_cache(params.write_cache),
    accept_unstable(params.accept_unstable),
    is_vdb(is_vdb_repository(params.repository_dir, params.repository_type))
{
    Context context("When initialising NoConfigEnvironment at '" + stringify(params.repository_dir) + "':");

    if (! is_vdb)
    {
        if (FSEntry("/var/empty") != params.master_repository_dir)
        {
            std::tr1::shared_ptr<AssociativeCollection<std::string, std::string> > keys(
                    new AssociativeCollection<std::string, std::string>::Concrete);

            keys->insert("format", "ebuild");
            keys->insert("location", stringify(params.master_repository_dir));
            keys->insert("profiles", "/var/empty");
            keys->insert("write_cache", stringify(params.write_cache));
            keys->insert("names_cache", "/var/empty");

            env->package_database()->add_repository(((master_repo =
                            std::tr1::static_pointer_cast<PortageRepository>(
                                RepositoryMaker::get_instance()->find_maker("ebuild")(env, keys)))));
        }

        std::tr1::shared_ptr<AssociativeCollection<std::string, std::string> > keys(
                new AssociativeCollection<std::string, std::string>::Concrete);

        keys->insert("format", "ebuild");
        keys->insert("location", stringify(params.repository_dir));
        keys->insert("profiles", "/var/empty");
        keys->insert("write_cache", stringify(params.write_cache));
        keys->insert("names_cache", "/var/empty");
        if (FSEntry("/var/empty") != params.master_repository_dir)
            keys->insert("master_repository", stringify(master_repo->name()));

        env->package_database()->add_repository(((portage_repo =
                        std::tr1::static_pointer_cast<PortageRepository>(
                            RepositoryMaker::get_instance()->find_maker("ebuild")(env, keys)))));
        env->package_database()->add_repository(RepositoryMaker::get_instance()->find_maker("virtuals")(env,
                    std::tr1::shared_ptr<AssociativeCollection<std::string, std::string> >()));
    }
    else
    {
        Log::get_instance()->message(ll_debug, lc_context, "VDB, using vdb_db");

        std::tr1::shared_ptr<AssociativeCollection<std::string, std::string> > keys(
                new AssociativeCollection<std::string, std::string>::Concrete);

        keys->insert("format", "vdb");
        keys->insert("names_cache", "/var/empty");
        keys->insert("provides_cache", "/var/empty");
        keys->insert("location", stringify(top_level_dir));

        env->package_database()->add_repository(RepositoryMaker::get_instance()->find_maker("vdb")(env, keys));

        std::tr1::shared_ptr<AssociativeCollection<std::string, std::string> > iv_keys(
                new AssociativeCollection<std::string, std::string>::Concrete);
        iv_keys->insert("root", "/");
        env->package_database()->add_repository(RepositoryMaker::get_instance()->find_maker("installed_virtuals")(env,
                    iv_keys));
    }
}

NoConfigEnvironment::NoConfigEnvironment(const NoConfigEnvironmentParams & params) :
    Environment(std::tr1::shared_ptr<PackageDatabase>(new PackageDatabase(this))),
    PrivateImplementationPattern<NoConfigEnvironment>(
            new Implementation<NoConfigEnvironment>(this, params))
{
    if (_imp->portage_repo)
        if (_imp->portage_repo->end_profiles() != _imp->portage_repo->begin_profiles())
            _imp->portage_repo->set_profile(_imp->portage_repo->begin_profiles());

    if (_imp->master_repo)
        if (_imp->master_repo->end_profiles() != _imp->master_repo->begin_profiles())
            _imp->master_repo->set_profile(_imp->master_repo->begin_profiles());
}

NoConfigEnvironment::~NoConfigEnvironment()
{
}

std::string
NoConfigEnvironment::paludis_command() const
{
    return "false";
}

FSEntry
NoConfigEnvironment::main_repository_dir() const
{
    return _imp->top_level_dir;
}

bool
NoConfigEnvironment::accept_keyword(const KeywordName & k, const PackageDatabaseEntry * const,
        const bool override_tilde_keywords) const
{
    if (_imp->is_vdb)
        return true;

    Log::get_instance()->message(ll_debug, lc_no_context, "accept_keyword " + stringify(k) + ":");
    std::string accept_keywords(_imp->portage_repo->profile_variable("ACCEPT_KEYWORDS"));
    if (accept_keywords.empty())
    {
        std::string arch(_imp->portage_repo->profile_variable("ARCH"));
        if (stringify(k) == arch)
        {
            Log::get_instance()->message(ll_debug, lc_no_context, "accept_keyword match on arch");
            return true;
        }

        if ((_imp->accept_unstable || override_tilde_keywords) && ("~" + stringify(k) == arch))
        {
            Log::get_instance()->message(ll_debug, lc_no_context, "accept_keyword match on ~arch");
            return true;
        }

        Log::get_instance()->message(ll_debug, lc_no_context, "accept_keyword no match on arch");
    }
    else
    {
        std::list<KeywordName> accepted;
        WhitespaceTokeniser::get_instance()->tokenise(accept_keywords,
                create_inserter<KeywordName>(std::back_inserter(accepted)));

        if (accepted.end() != std::find(accepted.begin(), accepted.end(), k))
        {
            Log::get_instance()->message(ll_debug, lc_no_context, "accept_keyword match on accepted");
            return true;
        }

        if ((_imp->accept_unstable || override_tilde_keywords) && '~' == stringify(k).at(0))
        {
            if (accepted.end() != std::find(accepted.begin(), accepted.end(),
                        KeywordName(stringify(k).substr(1))))
            {
                Log::get_instance()->message(ll_debug, lc_no_context, "accept_keyword match on ~accepted");
                return true;
            }

            Log::get_instance()->message(ll_debug, lc_no_context, "accept_keyword no match on ~accepted");
        }
        else
            Log::get_instance()->message(ll_debug, lc_no_context, "accept_keyword no match on accepted");
    }

    return false;
}

void
NoConfigEnvironment::set_accept_unstable(const bool value)
{
    _imp->accept_unstable = value;
}

std::tr1::shared_ptr<PortageRepository>
NoConfigEnvironment::portage_repository()
{
    return _imp->portage_repo;
}

std::tr1::shared_ptr<const PortageRepository>
NoConfigEnvironment::portage_repository() const
{
    return _imp->portage_repo;
}

std::tr1::shared_ptr<PortageRepository>
NoConfigEnvironment::master_repository()
{
    return _imp->master_repo;
}

std::tr1::shared_ptr<const PortageRepository>
NoConfigEnvironment::master_repository() const
{
    return _imp->master_repo;
}


