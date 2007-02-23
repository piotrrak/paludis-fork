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

#include <paludis/dep_spec.hh>
#include <paludis/package_database_entry.hh>
#include <paludis/environment.hh>
#include <paludis/portage_dep_parser.hh>
#include <paludis/qa/deps_exist_check.hh>
#include <paludis/util/save.hh>
#include <paludis/query.hh>
#include <paludis/qa/qa_environment.hh>
#include <paludis/repositories/gentoo/portage_repository.hh>

using namespace paludis;
using namespace paludis::qa;

namespace
{
    struct Checker :
        DepSpecVisitorTypes::ConstVisitor,
        DepSpecVisitorTypes::ConstVisitor::VisitChildren<Checker, AllDepSpec>,
        DepSpecVisitorTypes::ConstVisitor::VisitChildren<Checker, UseDepSpec>
    {
        using DepSpecVisitorTypes::ConstVisitor::VisitChildren<Checker, AllDepSpec>::visit;
        using DepSpecVisitorTypes::ConstVisitor::VisitChildren<Checker, UseDepSpec>::visit;

        CheckResult & result;
        const std::string role;
        const Environment * env;
        bool in_any;

        Checker(CheckResult & rr, const std::string & r, const Environment * e) :
            result(rr),
            role(r),
            env(e),
            in_any(false)
        {
        }

        void visit(const PackageDepSpec * const p)
        {
            if (env->package_database()->query(query::Package(p->package()), qo_whatever)->empty())
            {
                if (in_any)
                    result << Message(qal_maybe, "No match for " + role + " entry '"
                            + stringify(*p) + "' inside || ( ) block");
                else
                    result << Message(qal_major, "No match for " + role + " entry '"
                            + stringify(*p) + "'");
            }
        }

        void visit(const AnyDepSpec * const a)
        {
            /// \todo VV make this smarter
            Save<bool> save_in_any(&in_any, true);
            std::for_each(a->begin(), a->end(), accept_visitor(this));
        }

        void visit(const BlockDepSpec * const b)
        {
            if (env->package_database()->query(query::Package(b->blocked_spec()->package()),
                        qo_whatever)->empty())
                result << Message(qal_maybe, "No match for " + role + " block '!"
                        + stringify(b->blocked_spec()->package()) + "'");
        }

        void visit(const PlainTextDepSpec * const)
        {
        }
    };
}

DepsExistCheck::DepsExistCheck()
{
}

CheckResult
DepsExistCheck::operator() (const EbuildCheckData & e) const
{
    CheckResult result(stringify(e.name) + "-" + stringify(e.version),
            identifier());

    try
    {
        PackageDatabaseEntry ee(e.name, e.version,
                e.environment->portage_repository()->name());
        std::tr1::shared_ptr<const VersionMetadata> metadata(
                e.environment->package_database()->fetch_repository(ee.repository)->version_metadata(ee.name, ee.version));

        Checker depend_checker(result, "DEPEND", e.environment);
        std::string depend(metadata->deps_interface->build_depend_string);
        PortageDepParser::parse(depend)->accept(&depend_checker);

        Checker rdepend_checker(result, "RDEPEND", e.environment);
        std::string rdepend(metadata->deps_interface->run_depend_string);
        PortageDepParser::parse(rdepend)->accept(&rdepend_checker);

        Checker pdepend_checker(result, "PDEPEND", e.environment);
        std::string pdepend(metadata->deps_interface->post_depend_string);
        PortageDepParser::parse(pdepend)->accept(&pdepend_checker);
    }
    catch (const InternalError &)
    {
        throw;
    }
    catch (const Exception & err)
    {
        result << Message(qal_fatal, "Caught Exception '" + err.message() + "' ("
                + err.what() + ")");
    }

    return result;
}

const std::string &
DepsExistCheck::identifier()
{
    static const std::string id("deps_exist");
    return id;
}

