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

#include "root_variable.hh"
#include <paludis/qa.hh>
#include <paludis/util/log.hh>
#include <paludis/util/strip.hh>
#include <paludis/util/tokeniser.hh>
#include <pcre++.h>
#include <list>

using namespace paludis;

namespace
{
    enum State
    {
        st_default,
        st_in_src
    };
}

bool
paludis::erepository::root_variable_check(
        const FSEntry & entry,
        QAReporter & reporter,
        const tr1::shared_ptr<const PackageID> & id,
        const std::string & content,
        const std::string & name)
{
    Context context("When performing check '" + name + "' using root_variable_check on '" + stringify(*id) + "':");

    pcrepp::Pcre::Pcre r_root("\\$[{]?ROOT[}]?([^=a-zA-Z0-9]|$)");
    pcrepp::Pcre::Pcre r_start("^src_");
    pcrepp::Pcre::Pcre r_end("^}");

    Log::get_instance()->message(ll_debug, lc_context) << "root_variable '"
        << entry << "', '" << *id << "', '" << name << "'";

    std::list<std::string> lines;
    tokenise<delim_kind::AnyOfTag, delim_mode::DelimiterTag>(content, "\n", "", std::back_inserter(lines));

    State state(st_default);
    std::string func;
    unsigned line_number(0);

    for (std::list<std::string>::const_iterator l(lines.begin()), l_end(lines.end()) ;
            l != l_end ; ++l)
    {
        ++line_number;

        switch (state)
        {
            case st_default:
                {
                    if (r_start.search(*l))
                    {
                        state = st_in_src;
                        func = *l;
                        if (std::string::npos != func.find('('))
                            func = func.substr(0, func.find('('));
                    }
                }
                continue;

            case st_in_src:
                {
                    if (r_end.search(*l))
                        state = st_default;
                    else if (r_root.search(*l))
                        reporter.message(QAMessage(entry, qaml_maybe, name, "ROOT abuse in " + func + " on line "
                                + stringify(line_number) + ": " + strip_leading(*l, " \t"))
                                .with_associated_id(id));
                }
                continue;
        }

        throw InternalError(PALUDIS_HERE, "bad state");
    }

    return true;
}

