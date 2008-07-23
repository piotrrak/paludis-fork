/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2005, 2006, 2007, 2008 Ciaran McCreesh
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

#include <paludis/user_dep_spec.hh>
#include <paludis/elike_package_dep_spec.hh>
#include <paludis/environment.hh>
#include <paludis/elike_use_requirement.hh>
#include <paludis/version_operator.hh>
#include <paludis/version_spec.hh>
#include <paludis/version_requirements.hh>
#include <paludis/package_database.hh>
#include <paludis/filter.hh>
#include <paludis/util/make_shared_ptr.hh>
#include <paludis/util/options.hh>
#include <paludis/util/log.hh>
#include <paludis/util/visitor-impl.hh>

using namespace paludis;

#include <paludis/user_dep_spec-se.cc>

namespace
{
    void user_add_package_requirement(const std::string & s, PartiallyMadePackageDepSpec & result,
            const Environment * const env, const UserPackageDepSpecOptions & options,
            const Filter & filter)
    {
        if (s.length() >= 3 && (0 == s.compare(0, 2, "*/")))
        {
            if (! options[updso_allow_wildcards])
                throw PackageDepSpecError("Wildcard '*' not allowed");

            if (0 != s.compare(s.length() - 2, 2, "/*"))
                result.package_name_part(PackageNamePart(s.substr(2)));
        }
        else if (s.length() >= 3 && (0 == s.compare(s.length() - 2, 2, "/*")))
        {
            if (! options[updso_allow_wildcards])
                throw PackageDepSpecError("Wildcard '*' not allowed in '" + stringify(s) + "'");

            result.category_name_part(CategoryNamePart(s.substr(0, s.length() - 2)));
        }
        else if (s == "*")
            throw PackageDepSpecError("Use '*/*' not '*' to match everything");
        else if (std::string::npos != s.find('/'))
            result.package(QualifiedPackageName(s));
        else
        {
            if (options[updso_no_disambiguation])
                throw PackageDepSpecError("Need an explicit category specified");
            result.package(env->package_database()->fetch_unique_qualified_package_name(PackageNamePart(s), filter));
        }
    }

    void user_check_sanity(const std::string & s, const UserPackageDepSpecOptions & options,
            const Environment * const env)
    {
        if (s.empty())
            throw PackageDepSpecError("Got empty dep spec");

        if (options[updso_throw_if_set] && std::string::npos == s.find_first_of("/:[<>=~"))
            try
            {
                SetName sn(s);
                if (options[updso_no_disambiguation] || env->set(sn))
                    throw GotASetNotAPackageDepSpec(s);
            }
            catch (const SetNameError &)
            {
            }
    }

    bool user_remove_trailing_square_bracket_if_exists(std::string & s, PartiallyMadePackageDepSpec & result,
            bool & had_bracket_version_requirements)
    {
        std::string::size_type use_group_p;
        if (std::string::npos == ((use_group_p = s.rfind('['))))
            return false;

        if (s.at(s.length() - 1) != ']')
            throw PackageDepSpecError("Mismatched []");

        std::string flag(s.substr(use_group_p + 1));
        if (flag.length() < 2)
            throw PackageDepSpecError("Invalid [] contents");

        flag.erase(flag.length() - 1);

        switch (flag.at(0))
        {
            case '<':
            case '>':
            case '=':
            case '~':
                {
                    char needed_mode(0);

                    while (! flag.empty())
                    {
                        Context cc("When parsing [] segment '" + flag + "':");

                        std::string op;
                        std::string::size_type opos(0);
                        while (opos < flag.length())
                            if (std::string::npos == std::string("><=~").find(flag.at(opos)))
                                break;
                            else
                                ++opos;

                        op = flag.substr(0, opos);
                        flag.erase(0, opos);

                        if (op.empty())
                            throw PackageDepSpecError("Missing operator inside []");

                        VersionOperator vop(op);

                        std::string ver;
                        opos = flag.find_first_of("|&");
                        if (std::string::npos == opos)
                        {
                            ver = flag;
                            flag.clear();
                        }
                        else
                        {
                            if (0 == needed_mode)
                                needed_mode = flag.at(opos);
                            else if (needed_mode != flag.at(opos))
                                throw PackageDepSpecError("Mixed & and | inside []");

                            result.version_requirements_mode((flag.at(opos) == '|' ? vr_or : vr_and));
                            ver = flag.substr(0, opos++);
                            flag.erase(0, opos);
                        }

                        if (ver.empty())
                            throw PackageDepSpecError("Missing version after operator '" + stringify(vop) + " inside []");

                        if ('*' == ver.at(ver.length() - 1))
                        {
                            ver.erase(ver.length() - 1);
                            if (vop == vo_equal)
                                vop = vo_equal_star;
                            else
                                throw PackageDepSpecError("Invalid use of * with operator '" + stringify(vop) + " inside []");
                        }

                        VersionSpec vs(ver);
                        result.version_requirement(VersionRequirement(vop, vs));
                        had_bracket_version_requirements = true;
                    }
                }
                break;

            default:
                {
                    std::tr1::shared_ptr<const AdditionalPackageDepSpecRequirement> req(parse_elike_use_requirement(flag,
                                std::tr1::shared_ptr<const PackageID>(), ELikeUseRequirementOptions()));
                    result.additional_requirement(req);
                }
                break;
        };

        s.erase(use_group_p);

        return true;
    }

    void
    user_remove_trailing_slot_if_exists(std::string & s, PartiallyMadePackageDepSpec & result)
    {
        std::string::size_type slot_p(s.rfind(':'));
        if (std::string::npos != slot_p)
        {
            result.slot_requirement(make_shared_ptr(new UserSlotExactRequirement(SlotName(s.substr(slot_p + 1)))));
            s.erase(slot_p);
        }
    }

    void
    user_remove_trailing_repo_if_exists(std::string & s, PartiallyMadePackageDepSpec & result)
    {
        std::string::size_type repo_p;
        if (std::string::npos == ((repo_p = s.rfind("::"))))
            return;

        std::string repo_name(s.substr(repo_p + 2));
        s.erase(repo_p);

        std::string::size_type arrow_p(repo_name.find("->"));
        if (std::string::npos == arrow_p)
            result.in_repository(RepositoryName(repo_name));
        else
        {
            std::string from_repository(repo_name.substr(0, arrow_p));
            std::string to_repository(repo_name.substr(arrow_p + 2));

            if (from_repository.empty() && to_repository.empty())
                throw PackageDepSpecError("::-> requires either a from or a to repository");

            if (! to_repository.empty())
                result.in_repository(RepositoryName(to_repository));

            if (! from_repository.empty())
                result.from_repository(RepositoryName(from_repository));
        }
    }
}

PackageDepSpec
paludis::parse_user_package_dep_spec(const std::string & ss, const Environment * const env,
        const UserPackageDepSpecOptions & options, const Filter & filter)
{
    using namespace std::tr1::placeholders;

    Context context("When parsing user package dep spec '" + ss + "':");

    bool had_bracket_version_requirements(false);
    return partial_parse_generic_elike_package_dep_spec(ss, GenericELikePackageDepSpecParseFunctions::named_create()
            (k::check_sanity(), std::tr1::bind(&user_check_sanity, _1, options, env))
            (k::remove_trailing_square_bracket_if_exists(), std::tr1::bind(&user_remove_trailing_square_bracket_if_exists,
                    _1, _2, std::tr1::ref(had_bracket_version_requirements)))
            (k::remove_trailing_repo_if_exists(), std::tr1::bind(&user_remove_trailing_repo_if_exists, _1, _2))
            (k::remove_trailing_slot_if_exists(), std::tr1::bind(&user_remove_trailing_slot_if_exists, _1, _2))
            (k::has_version_operator(), std::tr1::bind(&elike_has_version_operator, _1, std::tr1::cref(had_bracket_version_requirements)))
            (k::get_remove_version_operator(), std::tr1::bind(&elike_get_remove_version_operator, _1,
                    ELikePackageDepSpecOptions() + epdso_allow_tilde_greater_deps + epdso_strict_star_operator))
            (k::get_remove_trailing_version(), std::tr1::bind(&elike_get_remove_trailing_version, _1))
            (k::add_version_requirement(), std::tr1::bind(&elike_add_version_requirement, _1, _2, _3))
            (k::add_package_requirement(), std::tr1::bind(&user_add_package_requirement, _1, _2, env, options, filter))
            );
}

UserSlotExactRequirement::UserSlotExactRequirement(const SlotName & s) :
    _s(s)
{
}
const SlotName
UserSlotExactRequirement::slot() const
{
    return _s;
}

const std::string
UserSlotExactRequirement::as_string() const
{
    return ":" + stringify(_s);
}

GotASetNotAPackageDepSpec::GotASetNotAPackageDepSpec(const std::string & s) throw () :
    Exception("'" + s + "' is a set, not a package")
{
}

