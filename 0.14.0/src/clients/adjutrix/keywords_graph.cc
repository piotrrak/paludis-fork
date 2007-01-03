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

#include "keywords_graph.hh"
#include "command_line.hh"
#include <output/colour.hh>

#include <paludis/util/tokeniser.hh>
#include <paludis/util/compare.hh>

#include <set>
#include <map>
#include <iostream>
#include <iomanip>
#include <algorithm>

using namespace paludis;
using std::cout;
using std::cerr;
using std::endl;

using namespace paludis;

namespace
{
    struct CompareByStringLength
    {
        template<typename T_>
        bool
        operator() (const T_ & l, const T_ & r) const
        {
            return stringify(l).length() < stringify(r).length();
        }
    };

    void
    write_keywords_graph(const Environment &, const Repository & repo,
            const QualifiedPackageName & package)
    {
        Context context("When writing keyword graph for '" + stringify(package) + "' in '"
                + stringify(repo.name()) + "':");

        cout << "Keywords for " << package << ":" << endl;
        cout << endl;

        VersionSpecCollection::ConstPointer versions(repo.version_specs(package));
        if (versions->empty())
            return;

        if (! repo.use_interface)
            throw InternalError(PALUDIS_HERE, "Repository has no use_interface");

        UseFlagNameCollection::ConstPointer arch_flags(repo.use_interface->arch_flags());
        if (arch_flags->empty())
            return;

        std::set<SlotName> slots;
        for (VersionSpecCollection::Iterator v(versions->begin()), v_end(versions->end()) ;
                v != v_end ; ++v)
            slots.insert(repo.version_metadata(package, *v)->slot);

        unsigned version_specs_columns_width(stringify(*std::max_element(versions->begin(),
                        versions->end(), CompareByStringLength())).length() + 1);

        unsigned tallest_arch_name(std::max(stringify(*std::max_element(arch_flags->begin(),
                            arch_flags->end(), CompareByStringLength())).length(), static_cast<std::size_t>(4)));

        unsigned longest_slot_name(stringify(*std::max_element(slots.begin(),
                        slots.end(), CompareByStringLength())).length());

        for (unsigned h = 0 ; h < tallest_arch_name ; ++h)
        {
            cout << std::left << std::setw(version_specs_columns_width) << " " << "| ";
            for (UseFlagNameCollection::Iterator a(arch_flags->begin()), a_end(arch_flags->end()) ;
                    a != a_end ; ++a)
            {
                if ((tallest_arch_name - h) > a->data().length())
                    cout << "  ";
                else
                    cout << a->data().at(a->data().length() - tallest_arch_name + h) << " ";
            }
            cout << "| ";
            if ((tallest_arch_name - h) <= 4)
                cout << std::string("slot").at(4 - tallest_arch_name + h);
            cout << endl;
        }

        cout << std::string(version_specs_columns_width, '-') << "+"
            << std::string(arch_flags->size() * 2 + 1, '-') << "+"
            << std::string(longest_slot_name + 1, '-') << endl;

        SlotName old_slot("first_slot");
        for (VersionSpecCollection::Iterator v(versions->begin()), v_end(versions->end()) ;
                v != v_end ; ++v)
        {
            VersionMetadata::ConstPointer metadata(repo.version_metadata(package, *v));
            if (! metadata->get_ebuild_interface())
                continue;

            if (metadata->slot != old_slot)
                if (old_slot != SlotName("first_slot"))
                    cout << std::string(version_specs_columns_width, '-') << "+"
                        << std::string(arch_flags->size() * 2 + 1, '-') << "+"
                        << std::string(longest_slot_name + 1, '-') << endl;

            cout << std::left << std::setw(version_specs_columns_width) << *v << "| ";

            std::set<KeywordName> keywords;
            WhitespaceTokeniser::get_instance()->tokenise(metadata->get_ebuild_interface()->keywords,
                    create_inserter<KeywordName>(std::inserter(keywords, keywords.end())));

            for (UseFlagNameCollection::Iterator a(arch_flags->begin()), a_end(arch_flags->end()) ;
                    a != a_end ; ++a)
            {
                if (keywords.end() != keywords.find(KeywordName(stringify(*a))))
                    cout << colour(cl_bold_green, "+ ");
                else if (keywords.end() != keywords.find(KeywordName("~" + stringify(*a))))
                    cout << colour(cl_bold_yellow, "~ ");
                else if (keywords.end() != keywords.find(KeywordName("-" + stringify(*a))))
                    cout << colour(cl_red, "- ");
                else if (keywords.end() != keywords.find(KeywordName("-*")))
                    cout << colour(cl_red, "* ");
                else
                    cout << "  ";
            }

            cout << "| ";
            if (metadata->slot != old_slot)
            {
                cout << metadata->slot;
                old_slot = metadata->slot;
            }

            cout << endl;
        }

        cout << endl;
    }
}

void do_keywords_graph(const Environment & env)
{
    Context context("When performing keywords-graph action:");

    for (IndirectIterator<PackageDatabase::RepositoryIterator, const Repository>
            r(env.package_database()->begin_repositories()),
            r_end(env.package_database()->end_repositories()) ; r != r_end ; ++r)
    {
        if (r->name() == RepositoryName("virtuals"))
            continue;

        CategoryNamePartCollection::ConstPointer cat_names(r->category_names());
        for (CategoryNamePartCollection::Iterator c(cat_names->begin()), c_end(cat_names->end()) ;
                c != c_end ; ++c)
        {
            if (CommandLine::get_instance()->a_category.specified())
                if (CommandLine::get_instance()->a_category.args_end() == std::find(
                            CommandLine::get_instance()->a_category.args_begin(),
                            CommandLine::get_instance()->a_category.args_end(),
                            stringify(*c)))
                    continue;

            QualifiedPackageNameCollection::ConstPointer pkg_names(r->package_names(*c));
            for (QualifiedPackageNameCollection::Iterator p(pkg_names->begin()), p_end(pkg_names->end()) ;
                    p != p_end ; ++p)
            {
                if (CommandLine::get_instance()->a_package.specified())
                    if (CommandLine::get_instance()->a_package.args_end() == std::find(
                                CommandLine::get_instance()->a_package.args_begin(),
                                CommandLine::get_instance()->a_package.args_end(),
                                stringify(p->package)))
                        continue;

                write_keywords_graph(env, *r, *p);
            }
        }
    }
}

