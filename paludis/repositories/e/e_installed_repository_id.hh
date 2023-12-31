/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011 Ciaran McCreesh
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

#ifndef PALUDIS_GUARD_PALUDIS_REPOSITORIES_E_E_INSTALLED_REPOSITORY_ID_HH
#define PALUDIS_GUARD_PALUDIS_REPOSITORIES_E_E_INSTALLED_REPOSITORY_ID_HH 1

#include <paludis/package_id.hh>
#include <paludis/metadata_key.hh>
#include <paludis/environment-fwd.hh>
#include <paludis/repositories/e/e_repository_id.hh>

namespace paludis
{
    namespace erepository
    {
        class EInstalledRepositoryID :
            public ERepositoryID,
            public std::enable_shared_from_this<EInstalledRepositoryID>
        {
            private:
                Pimp<EInstalledRepositoryID> _imp;

            protected:
                void need_keys_added() const override;
                void need_masks_added() const override;

                EInstalledRepositoryID(const QualifiedPackageName &, const VersionSpec &,
                        const Environment * const,
                        const RepositoryName &,
                        const FSPath & file);

            public:
                ~EInstalledRepositoryID() override;

                const std::string canonical_form(const PackageIDCanonicalForm) const override;
                PackageDepSpec uniquely_identifying_spec() const override;

                const QualifiedPackageName name() const override;
                const VersionSpec version() const override;
                const RepositoryName repository_name() const override;
                const std::shared_ptr<const EAPI> eapi() const override;
                bool is_installed() const override PALUDIS_ATTRIBUTE((warn_unused_result));

                const std::shared_ptr<const MetadataValueKey<Slot> > slot_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<KeywordNameSet> > keywords_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<DependencySpecTree> > build_dependencies_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<DependencySpecTree> > run_dependencies_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<DependencySpecTree> > post_dependencies_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<DependencySpecTree> > dependencies_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<PlainTextSpecTree> > restrict_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<PlainTextSpecTree> > properties_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<FetchableURISpecTree> > fetches_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<SimpleURISpecTree> > homepage_key() const override;
                const std::shared_ptr<const MetadataValueKey<std::string> > short_description_key() const override;
                const std::shared_ptr<const MetadataValueKey<std::string> > long_description_key() const override;
                const std::shared_ptr<const MetadataTimeKey> installed_time_key() const override;
                const std::shared_ptr<const MetadataValueKey<FSPath> > fs_location_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > from_repositories_key() const override;

                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > raw_use_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > raw_iuse_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > raw_iuse_effective_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<PlainTextSpecTree> > raw_myoptions_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > raw_use_expand_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > raw_use_expand_hidden_key() const override;
                const std::shared_ptr<const MetadataValueKey<std::shared_ptr<const Choices> > > choices_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > inherited_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > defined_phases_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<LicenseSpecTree> > license_key() const override;
                const std::shared_ptr<const MetadataSpecTreeKey<RequiredUseSpecTree> > required_use_key() const override;
                const std::shared_ptr<const MetadataValueKey<std::string> > scm_revision_key() const override;
                const std::shared_ptr<const MetadataCollectionKey<Set<std::string> > > behaviours_key() const override;

                bool supports_action(const SupportsActionTestBase &) const override PALUDIS_ATTRIBUTE((warn_unused_result));
                void perform_action(Action &) const override;

                bool arbitrary_less_than_comparison(const PackageID &) const
                    override PALUDIS_ATTRIBUTE((warn_unused_result));

                std::size_t extra_hash_value() const
                    override PALUDIS_ATTRIBUTE((warn_unused_result));

                virtual std::string fs_location_raw_name() const = 0;
                virtual std::string fs_location_human_name() const = 0;
                virtual std::string contents_filename() const = 0;

                const std::shared_ptr<const ChoiceValue> make_choice_value(
                        const std::shared_ptr<const Choice> &, const UnprefixedChoiceName &, const Tribool,
                        const bool, const ChoiceOrigin, const std::string &, const bool, const bool) const override;

                void add_build_options(const std::shared_ptr<Choices> &) const override;

                void purge_invalid_cache() const override;
                void can_drop_in_memory_cache() const override;

                void set_scm_revision(const std::string &) const override PALUDIS_ATTRIBUTE((noreturn));
        };
    }
}

#endif
