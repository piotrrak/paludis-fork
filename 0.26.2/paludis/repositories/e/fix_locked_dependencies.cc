/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2008 Ciaran McCreesh
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

#include <paludis/repositories/e/fix_locked_dependencies.hh>
#include <paludis/repositories/e/package_dep_spec.hh>
#include <paludis/util/visitor-impl.hh>
#include <paludis/util/visitor_cast.hh>
#include <paludis/util/tr1_functional.hh>
#include <paludis/util/exception.hh>
#include <paludis/util/stringify.hh>
#include <paludis/util/make_shared_ptr.hh>
#include <paludis/util/fs_entry.hh>
#include <paludis/dep_spec.hh>
#include <paludis/query.hh>
#include <paludis/environment.hh>
#include <paludis/package_database.hh>
#include <paludis/package_id.hh>
#include <algorithm>
#include <list>

using namespace paludis;
using namespace paludis::erepository;

namespace
{
    void cannot_add(const tr1::shared_ptr<const DependencySpecTree::ConstItem> &) PALUDIS_ATTRIBUTE((noreturn));

    void cannot_add(const tr1::shared_ptr<const DependencySpecTree::ConstItem> &)
    {
        throw InternalError(PALUDIS_HERE, "Got weird tree");
    }

    struct Fixer :
        ConstVisitor<DependencySpecTree>
    {
        std::list<std::pair<
            tr1::shared_ptr<DependencySpecTree::ConstItem>,
            tr1::function<void (const tr1::shared_ptr<DependencySpecTree::ConstItem> &)> > > stack;

        tr1::shared_ptr<const DependencySpecTree::ConstItem> result;

        const Environment * const env;
        const EAPI & eapi;
        const tr1::shared_ptr<const PackageID> id;

        Fixer(const Environment * const e, const EAPI & a, const tr1::shared_ptr<const PackageID> & i) :
            env(e),
            eapi(a),
            id(i)
        {
        }

        void visit_sequence(const AllDepSpec & s,
                DependencySpecTree::ConstSequenceIterator cur,
                DependencySpecTree::ConstSequenceIterator end)
        {
            tr1::shared_ptr<ConstTreeSequence<DependencySpecTree, AllDepSpec> > c(
                    new ConstTreeSequence<DependencySpecTree, AllDepSpec>(
                        tr1::static_pointer_cast<AllDepSpec>(s.clone())));

            if (! stack.empty())
                stack.begin()->second(c);
            else
                result = c;

            using namespace tr1::placeholders;
            stack.push_front(std::make_pair(c, tr1::bind(&ConstTreeSequence<DependencySpecTree, AllDepSpec>::add, c.get(), _1)));
            std::for_each(cur, end, accept_visitor(*this));
            stack.pop_front();
        }

        void visit_sequence(const AnyDepSpec & s,
                DependencySpecTree::ConstSequenceIterator cur,
                DependencySpecTree::ConstSequenceIterator end)
        {
            tr1::shared_ptr<ConstTreeSequence<DependencySpecTree, AnyDepSpec> > c(
                    new ConstTreeSequence<DependencySpecTree, AnyDepSpec>(
                        tr1::static_pointer_cast<AnyDepSpec>(s.clone())));

            if (! stack.empty())
                stack.begin()->second(c);
            else
                result = c;

            using namespace tr1::placeholders;
            stack.push_front(std::make_pair(c, tr1::bind(&ConstTreeSequence<DependencySpecTree, AnyDepSpec>::add, c.get(), _1)));
            std::for_each(cur, end, accept_visitor(*this));
            stack.pop_front();
        }

        void visit_sequence(const ConditionalDepSpec & s,
                DependencySpecTree::ConstSequenceIterator cur,
                DependencySpecTree::ConstSequenceIterator end)
        {
            tr1::shared_ptr<ConstTreeSequence<DependencySpecTree, ConditionalDepSpec> > c(
                    new ConstTreeSequence<DependencySpecTree, ConditionalDepSpec>(
                        tr1::static_pointer_cast<ConditionalDepSpec>(s.clone())));

            if (! stack.empty())
                stack.begin()->second(c);
            else
                result = c;

            using namespace tr1::placeholders;
            stack.push_front(std::make_pair(c, tr1::bind(&ConstTreeSequence<DependencySpecTree, ConditionalDepSpec>::add, c.get(), _1)));
            std::for_each(cur, end, accept_visitor(*this));
            stack.pop_front();
        }

        void visit_leaf(const PackageDepSpec & s)
        {
            tr1::shared_ptr<TreeLeaf<DependencySpecTree, PackageDepSpec> > c;

            do
            {
                if (! s.slot_requirement_ptr())
                    break;

                const SlotAnyLockedRequirement * const r(visitor_cast<const SlotAnyLockedRequirement>(*s.slot_requirement_ptr()));
                if (! r)
                    break;

                tr1::shared_ptr<const PackageIDSequence> matches(env->package_database()->query(
                            query::Matches(s) & query::InstalledAtRoot(FSEntry("/")), qo_order_by_version));
                if (matches->empty())
                    break;

                PackageDepSpec new_s(partial_parse_e_package_dep_spec(stringify(s), eapi, id).slot_requirement(
                            make_shared_ptr(new ESlotExactRequirement((*matches->last())->slot(), true))));

                c.reset(new TreeLeaf<DependencySpecTree, PackageDepSpec>(tr1::static_pointer_cast<PackageDepSpec>(
                                PackageDepSpec(new_s).clone())));
            } while (false);

            if (! c)
                c.reset(new TreeLeaf<DependencySpecTree, PackageDepSpec>(tr1::static_pointer_cast<PackageDepSpec>(s.clone())));

            if (stack.empty())
            {
                stack.push_front(std::make_pair(c, &cannot_add));
                result = c;
            }
            else
                stack.begin()->second(c);
        }

        void visit_leaf(const NamedSetDepSpec & s)
        {
            tr1::shared_ptr<TreeLeaf<DependencySpecTree, NamedSetDepSpec> > c(
                    new TreeLeaf<DependencySpecTree, NamedSetDepSpec>(tr1::static_pointer_cast<NamedSetDepSpec>(s.clone())));

            if (stack.empty())
            {
                stack.push_front(std::make_pair(c, &cannot_add));
                result = c;
            }
            else
                stack.begin()->second(c);
        }

        void visit_leaf(const BlockDepSpec & s)
        {
            tr1::shared_ptr<TreeLeaf<DependencySpecTree, BlockDepSpec> > c(
                    new TreeLeaf<DependencySpecTree, BlockDepSpec>(tr1::static_pointer_cast<BlockDepSpec>(s.clone())));

            if (stack.empty())
            {
                stack.push_front(std::make_pair(c, &cannot_add));
                result = c;
            }
            else
                stack.begin()->second(c);
        }

        void visit_leaf(const DependencyLabelsDepSpec & s)
        {
            tr1::shared_ptr<TreeLeaf<DependencySpecTree, DependencyLabelsDepSpec> > c(
                    new TreeLeaf<DependencySpecTree, DependencyLabelsDepSpec>(tr1::static_pointer_cast<DependencyLabelsDepSpec>(s.clone())));

            if (stack.empty())
            {
                stack.push_front(std::make_pair(c, &cannot_add));
                result = c;
            }
            else
                stack.begin()->second(c);
        }
    };
}

const tr1::shared_ptr<const DependencySpecTree::ConstItem>
paludis::erepository::fix_locked_dependencies(
        const Environment * const env,
        const EAPI & e, const tr1::shared_ptr<const PackageID> & id,
        const tr1::shared_ptr<const DependencySpecTree::ConstItem> & b)
{
    Fixer f(env, e, id);
    b->accept(f);
    return f.result;
}

