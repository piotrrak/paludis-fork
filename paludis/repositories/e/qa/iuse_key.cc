/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2006, 2007, 2008 Ciaran McCreesh
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

#include "iuse_key.hh"
#include <paludis/qa.hh>
#include <paludis/metadata_key.hh>
#include <paludis/util/config_file.hh>
#include <paludis/util/create_iterator-impl.hh>
#include <paludis/util/fs_entry.hh>
#include <paludis/util/instantiation_policy-impl.hh>
#include <paludis/util/log.hh>
#include <paludis/util/options.hh>
#include <paludis/util/set.hh>
#include <paludis/util/system.hh>
#include <paludis/util/wrapped_forward_iterator.hh>
#include <paludis/repositories/e/e_repository_id.hh>
#include <paludis/repositories/e/iuse.hh>
#include <paludis/name.hh>
#include <paludis/package_id.hh>
#include <paludis/repository.hh>
#include <paludis/choice.hh>
#include <set>

using namespace paludis;
using namespace paludis::erepository;

namespace
{
    struct IUseBlacklist :
        InstantiationPolicy<IUseBlacklist, instantiation_method::SingletonTag>
    {
        std::set<ChoiceNameWithPrefix> iuse_blacklist;

        IUseBlacklist(const FSEntry & f = FSEntry(getenv_with_default("PALUDIS_QA_DATA_DIR",
                        stringify(FSEntry(DATADIR) / "paludis" / "qa"))) / "iuse_blacklist.conf")
        {
            try
            {
                LineConfigFile iuse_blacklist_file(f, LineConfigFileOptions());
                std::copy(iuse_blacklist_file.begin(), iuse_blacklist_file.end(),
                        create_inserter<ChoiceNameWithPrefix>(std::inserter(iuse_blacklist, iuse_blacklist.end())));
            }
            catch (const InternalError &)
            {
                throw;
            }
            catch (const Exception & e)
            {
                Log::get_instance()->message("e.qa.iuse_key_check.configuration_error", ll_warning, lc_context)
                    << "Got error '" << e.message() << "' (" << e.what()
                    << ") when loading iuse_blacklist.conf for QA iuse_key";
            }
        }
    };
}

bool
paludis::erepository::iuse_key_check(
        const FSEntry & entry,
        QAReporter & reporter,
        const std::tr1::shared_ptr<const Repository> &,
        const std::tr1::shared_ptr<const PackageID> & id_raw,
        const std::string & name)
{
    Context context("When performing check '" + name + "' using iuse_key_check on ID '" + stringify(*id_raw) + "':");
    Log::get_instance()->message("e.qa.iuse_key_check", ll_debug, lc_context) << "iuse_key_check '"
        << entry << "', " << *id_raw << "', " << name << "'";

    std::tr1::shared_ptr<const ERepositoryID> id(std::tr1::static_pointer_cast<const ERepositoryID>(id_raw));
    if (id->raw_iuse_key())
    {
        try
        {
            const std::set<ChoiceNameWithPrefix> & iuse_blacklist(IUseBlacklist::get_instance()->iuse_blacklist);

            for (Set<std::string>::ConstIterator
                    it(id->raw_iuse_key()->value()->begin()),
                    it_end(id->raw_iuse_key()->value()->end()); it_end != it; ++it)
            {
                ChoiceNameWithPrefix flag(parse_iuse(id->eapi(), *it).first);
                if (iuse_blacklist.end() != iuse_blacklist.find(flag))
                    reporter.message(QAMessage(entry, qaml_minor, name,
                                "Deprecated flag '" + stringify(flag) + "' in '" + id->raw_iuse_key()->raw_name() + "'")
                                    .with_associated_id(id)
                                    .with_associated_key(id, id->raw_iuse_key()));
            }
        }
        catch (const InternalError &)
        {
            throw;
        }
        catch (const Exception & e)
        {
            reporter.message(QAMessage(entry, qaml_severe, name,
                        "Caught exception '" + stringify(e.message()) + "' ("
                        + stringify(e.what()) + ") when handling key '" + id->raw_iuse_key()->raw_name() + "'")
                            .with_associated_id(id)
                            .with_associated_key(id, id->raw_iuse_key()));
        }
    }

    return true;
}
