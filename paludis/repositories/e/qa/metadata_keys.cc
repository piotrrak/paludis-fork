/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2007, 2008, 2009 Ciaran McCreesh
 * Copyright (c) 2008 David Leverton
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

#include "metadata_keys.hh"
#include <paludis/package_id.hh>
#include <paludis/metadata_key.hh>
#include <paludis/qa.hh>
#include <paludis/dep_spec.hh>
#include <paludis/util/fs_entry.hh>
#include <paludis/util/log.hh>
#include <paludis/util/set.hh>

using namespace paludis;
using namespace paludis::erepository;

namespace
{
    struct KeyValidator
    {
        void visit(const MetadataValueKey<std::string> & k)
        {
            const std::string & PALUDIS_ATTRIBUTE((unused)) s(k.value());
        }

        void visit(const MetadataValueKey<SlotName> & k)
        {
            const SlotName & PALUDIS_ATTRIBUTE((unused)) s(k.value());
        }

        void visit(const MetadataValueKey<long> & k)
        {
            long PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataValueKey<bool> & k)
        {
            bool PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataValueKey<std::tr1::shared_ptr<const PackageID> > & k)
        {
            const std::tr1::shared_ptr<const PackageID> & PALUDIS_ATTRIBUTE((unused)) p(k.value());
        }

        void visit(const MetadataTimeKey & k)
        {
            time_t PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataValueKey<std::tr1::shared_ptr<const Contents> > & k)
        {
            const std::tr1::shared_ptr<const Contents> & PALUDIS_ATTRIBUTE((unused)) c(k.value());
        }

        void visit(const MetadataValueKey<FSEntry>& k)
        {
            const FSEntry & PALUDIS_ATTRIBUTE((unused)) c(k.value());
        }

        void visit(const MetadataValueKey<std::tr1::shared_ptr<const RepositoryMaskInfo> > & k)
        {
            const std::tr1::shared_ptr<const RepositoryMaskInfo> & PALUDIS_ATTRIBUTE((unused)) i(k.value());
        }

        void visit(const MetadataSpecTreeKey<PlainTextSpecTree> & k)
        {
            const std::tr1::shared_ptr<const PlainTextSpecTree> & PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataSpecTreeKey<ProvideSpecTree> & k)
        {
            const std::tr1::shared_ptr<const ProvideSpecTree> & PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataSpecTreeKey<FetchableURISpecTree> & k)
        {
            const std::tr1::shared_ptr<const FetchableURISpecTree> & PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataSpecTreeKey<SimpleURISpecTree> & k)
        {
            const std::tr1::shared_ptr<const SimpleURISpecTree> & PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataSpecTreeKey<LicenseSpecTree> & k)
        {
            const std::tr1::shared_ptr<const LicenseSpecTree> & PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataSpecTreeKey<DependencySpecTree> & k)
        {
            const std::tr1::shared_ptr<const DependencySpecTree> & PALUDIS_ATTRIBUTE((unused)) t(k.value());
        }

        void visit(const MetadataCollectionKey<PackageIDSequence> & k)
        {
            const std::tr1::shared_ptr<const PackageIDSequence> & PALUDIS_ATTRIBUTE((unused)) s(k.value());
        }

        void visit(const MetadataCollectionKey<Set<std::string> > & k)
        {
            const std::tr1::shared_ptr<const Set<std::string> > & PALUDIS_ATTRIBUTE((unused)) s(k.value());
        }

        void visit(const MetadataCollectionKey<Sequence<std::string> > & k)
        {
            const std::tr1::shared_ptr<const Sequence<std::string> > & PALUDIS_ATTRIBUTE((unused)) s(k.value());
        }

        void visit(const MetadataCollectionKey<FSEntrySequence> & k)
        {
            const std::tr1::shared_ptr<const FSEntrySequence> & PALUDIS_ATTRIBUTE((unused)) s(k.value());
        }

        void visit(const MetadataCollectionKey<KeywordNameSet> & k)
        {
            const std::tr1::shared_ptr<const KeywordNameSet> & PALUDIS_ATTRIBUTE((unused)) s(k.value());
        }

        void visit(const MetadataValueKey<std::tr1::shared_ptr<const Choices> > & k)
        {
            const std::tr1::shared_ptr<const Choices> & PALUDIS_ATTRIBUTE((unused)) s(k.value());
        }

        void visit(const MetadataSectionKey & k)
        {
            std::for_each(indirect_iterator(k.begin_metadata()),
                    indirect_iterator(k.end_metadata()), accept_visitor(*this));
        }
    };
}

bool
paludis::erepository::metadata_keys_check(
        const FSEntry & entry,
        QAReporter & reporter,
        const std::tr1::shared_ptr<const PackageID> & id,
        const std::string & name)
{
    Context context("When performing check '" + name + "' on ID '" + stringify(*id) + "':");
    Log::get_instance()->message("e.qa.metadata_keys_check", ll_debug, lc_context) << "extractors_check '"
        << entry << "', " << *id << "', " << name << "'";

    bool retval(true);
    KeyValidator validator;

    for (PackageID::MetadataConstIterator it(id->begin_metadata()),
             it_end(id->end_metadata()); it_end != it; ++it)
    {
        try
        {
            (*it)->accept(validator);
        }
        catch (const InternalError &)
        {
            throw;
        }
        catch (const Exception & e)
        {
            retval = false;
            reporter.message(QAMessage(entry, qaml_severe, name,
                        "Caught exception '" + stringify(e.message()) + "' ("
                        + stringify(e.what()) + ") when handling key '" + (*it)->raw_name() + "'")
                            .with_associated_id(id)
                            .with_associated_key(id, *it));
        }
    }

    return retval;
}
