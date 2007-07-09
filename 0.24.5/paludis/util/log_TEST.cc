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

#include <paludis/util/log.hh>
#include <sstream>
#include <test/test_framework.hh>
#include <test/test_runner.hh>

/** \file
 * Test cases for Log.
 *
 * \ingroup grptestcases
 */

using namespace paludis;
using namespace test;

namespace
{
    struct Monkey
    {
    };

    std::ostream &
    operator<< (std::ostream &, const Monkey &) PALUDIS_ATTRIBUTE((noreturn));

    std::ostream &
    operator<< (std::ostream &, const Monkey & m)
    {
        throw m;
    }

    int throws_a_monkey() PALUDIS_ATTRIBUTE((noreturn));
    int throws_a_monkey()
    {
        throw Monkey();
    }

    Monkey throws_a_monkey_when_stringified()
    {
        return Monkey();
    }
}

namespace test_cases
{
    struct LogTest : TestCase
    {
        LogTest() : TestCase("log") { }

        void run()
        {
            Log::destroy_instance();

            TEST_CHECK(Log::get_instance());
            TEST_CHECK(Log::get_instance() == Log::get_instance());

            std::stringstream s;
            Log::get_instance()->set_log_stream(&s);
            Log::get_instance()->set_log_level(ll_debug);

            TEST_CHECK(s.str().empty());
            Log::get_instance()->message(ll_debug, lc_no_context) << "one";
            TEST_CHECK(! s.str().empty());
            TEST_CHECK(std::string::npos != s.str().find("one"));

            std::stringstream t;
            Log::get_instance()->set_log_stream(&t);
            TEST_CHECK(t.str().empty());

            Log::get_instance()->set_log_level(ll_warning);
            Log::get_instance()->message(ll_debug, lc_no_context) << "two";
            TEST_CHECK(t.str().empty());
            Log::get_instance()->message(ll_warning, lc_no_context) << "three" << "." << 14;
            TEST_CHECK(! t.str().empty());
            TEST_CHECK(std::string::npos == t.str().find("one"));
            TEST_CHECK(std::string::npos == t.str().find("two"));
            TEST_CHECK(std::string::npos != t.str().find("three.14"));
        }
    } test_log;

    struct LogExceptionTest : TestCase
    {
        LogExceptionTest() : TestCase("log exception") { }

        void run()
        {
            Log::destroy_instance();

            std::stringstream s;
            Log::get_instance()->set_log_stream(&s);
            Log::get_instance()->set_log_level(ll_debug);
            TEST_CHECK(s.str().empty());

            TEST_CHECK_THROWS(Log::get_instance()->message(ll_debug, lc_no_context) << throws_a_monkey(), Monkey);
            TEST_CHECK(s.str().empty());
            TEST_CHECK_THROWS(Log::get_instance()->message(ll_debug, lc_no_context)
                    << "one" << throws_a_monkey() << "two", Monkey);
            TEST_CHECK(s.str().empty());

            TEST_CHECK_THROWS(Log::get_instance()->message(ll_debug, lc_no_context) << throws_a_monkey_when_stringified(), Monkey);
            TEST_CHECK(s.str().empty());
            TEST_CHECK_THROWS(Log::get_instance()->message(ll_debug, lc_no_context)
                    << "one" << throws_a_monkey_when_stringified() << "two", Monkey);
            TEST_CHECK(s.str().empty());
        }
    } test_log_exception;
}

