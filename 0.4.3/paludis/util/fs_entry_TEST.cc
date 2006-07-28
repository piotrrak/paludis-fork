/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2005, 2006 Ciaran McCreesh <ciaran.mccreesh@blueyonder.co.uk>
 * Copyright (c) 2006 Mark Loeser <halcy0n@gentoo.org>
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

#include <ctime>
#include <paludis/util/fs_entry.hh>
#include <test/test_framework.hh>
#include <test/test_runner.hh>

using namespace paludis;
using namespace test;

/** \file
 * Test cases for fs_entry.hh.
 *
 * \todo this is nowhere near complete.
 *
 * \ingroup grpfilesystem
 */

namespace test_cases
{
    /**
     * \test Test FSEntry construction and manipulation.
     *
     * \ingroup grpfilesystem
     */
    struct FSEntryManipulationTest : TestCase
    {
        FSEntryManipulationTest() : TestCase("construction and manipulation") { }

        void run()
        {
            FSEntry f("/foo/bar");
            FSEntry c(f);
            TEST_CHECK_EQUAL(f, FSEntry("/foo/bar"));
            TEST_CHECK_EQUAL(c, FSEntry("/foo/bar"));
            f = FSEntry("/baz");
            TEST_CHECK_EQUAL(f, FSEntry("/baz"));
            TEST_CHECK_EQUAL(c, FSEntry("/foo/bar"));
            c /= "moo";
            TEST_CHECK_EQUAL(f, FSEntry("/baz"));
            TEST_CHECK_EQUAL(c, FSEntry("/foo/bar/moo"));
            f = c / f;
            TEST_CHECK_EQUAL(f, FSEntry("/foo/bar/moo/baz"));
            TEST_CHECK_EQUAL(c, FSEntry("/foo/bar/moo"));

            f = FSEntry::cwd();
        }
    } test_fs_entry_manipulation;

    /**
     * \test Test FSEntry behavior.
     *
     * \ingroup grpfilesystem
     */
    struct FSEntryRealpathTest : TestCase
    {
        FSEntryRealpathTest() : TestCase("behavior") { }

        bool repeatable() const
        {
            return false;
        }

        void run()
        {
            FSEntry d("fs_entry_TEST_dir");
            TEST_CHECK(d.is_directory());

            d /= "all_perms";
            TEST_CHECK(d.is_regular_file());

            FSEntry e("fs_entry_TEST_dir/nosuchfile");
            TEST_CHECK(! e.is_regular_file());
            d = e;
            TEST_CHECK(! d.is_regular_file());
            TEST_CHECK(! d.exists());

            d = FSEntry("fs_entry_TEST_dir/all_perms");
            TEST_CHECK(! e.is_regular_file());
            TEST_CHECK(d.is_regular_file());
            TEST_CHECK(d.exists());

            d = FSEntry("fs_entry_TEST_dir/symlink_to_dir_a");
            TEST_CHECK(d.is_symbolic_link());
            TEST_CHECK(! d.is_directory());
            TEST_CHECK(! d.is_regular_file());

            FSEntry f("fs_entry_TEST_dir/symlink_to_dir_a/file_in_a");
            TEST_CHECK(f.is_regular_file());
            TEST_CHECK(! f.is_symbolic_link());
            FSEntry r(f.realpath());
            TEST_CHECK(r.is_regular_file());
            std::string g("fs_entry_TEST_dir/dir_a/file_in_a");
            TEST_CHECK_EQUAL(stringify(r).substr(stringify(r).length() - g.length()), g);

            FSEntry h("fs_entry_TEST_dir/symlink_to_file_in_a");
            TEST_CHECK(h.is_symbolic_link());
            TEST_CHECK(! h.is_regular_file());

            FSEntry i("fs_entry_TEST_dir/dir_to_make");
            TEST_CHECK(i.mkdir());
            TEST_CHECK(! i.mkdir());

            FSEntry j("fs_entry_TEST_dir/dir_a/file_in_a");
            TEST_CHECK_THROWS(j.mkdir(), FSError);
        }
    } test_fs_entry_behaviour;

    /**
     * \test Test FSEntry has_permission methods.
     *
     * \ingroup grpfilesystem
     */
    struct FSEntryHasPermission: TestCase
    {
        FSEntryHasPermission() : TestCase("has_permission") {}

        void run()
        {
            FSEntry a("fs_entry_TEST_dir/all_perms");
            FSEntry b("fs_entry_TEST_dir/no_perms");
            FSEntry c("fs_entry_TEST_dir/no_such_file");

            TEST_CHECK(a.has_permission(fs_ug_owner, fs_perm_read));
            TEST_CHECK(a.has_permission(fs_ug_owner, fs_perm_write));
            TEST_CHECK(a.has_permission(fs_ug_owner, fs_perm_execute));
            TEST_CHECK(a.has_permission(fs_ug_group, fs_perm_read));
            TEST_CHECK(a.has_permission(fs_ug_group, fs_perm_write));
            TEST_CHECK(a.has_permission(fs_ug_group, fs_perm_execute));
            TEST_CHECK(a.has_permission(fs_ug_others, fs_perm_read));
            TEST_CHECK(a.has_permission(fs_ug_others, fs_perm_write));
            TEST_CHECK(a.has_permission(fs_ug_others, fs_perm_execute));

            TEST_CHECK(!b.has_permission(fs_ug_owner, fs_perm_read));
            TEST_CHECK(!b.has_permission(fs_ug_owner, fs_perm_write));
            TEST_CHECK(!b.has_permission(fs_ug_owner, fs_perm_execute));
            TEST_CHECK(!b.has_permission(fs_ug_group, fs_perm_read));
            TEST_CHECK(!b.has_permission(fs_ug_group, fs_perm_write));
            TEST_CHECK(!b.has_permission(fs_ug_group, fs_perm_execute));
            TEST_CHECK(!b.has_permission(fs_ug_others, fs_perm_read));
            TEST_CHECK(!b.has_permission(fs_ug_others, fs_perm_write));
            TEST_CHECK(!b.has_permission(fs_ug_others, fs_perm_execute));

            TEST_CHECK_THROWS(c.has_permission(fs_ug_owner, fs_perm_read), FSError);
        }
    } test_fs_entry_permission;

    /**
     * \test Test FSEntry ctime and mtime methods
     *
     * \ingroup grpfilesystem
     */
    struct FSEntryTime : TestCase
    {
        FSEntryTime() : TestCase("ctime and mtime") {}

        void run()
        {
            FSEntry a("fs_entry_TEST_dir");
            FSEntry b("fs_entry_TEST_dir/no_perms");
            FSEntry c("fs_entry_TEST_dir/no_such_file");

            TEST_CHECK(a.ctime() <= std::time(NULL));
            TEST_CHECK((a.mtime() >= a.ctime()) && (a.mtime() <= std::time(NULL)));
            TEST_CHECK(b.ctime() <= std::time(NULL));
            TEST_CHECK((b.mtime() >= b.ctime()) && (b.mtime() <= std::time(NULL)));

            TEST_CHECK_THROWS(c.ctime(), FSError);
            TEST_CHECK_THROWS(c.mtime(), FSError);
        }
    } test_fs_entry_time;

    /**
     * \test Test FSEntry file_size
     *
     * \ingroup grpfilesystem
     */
    struct FSEntryFileSize : TestCase
    {
        FSEntryFileSize() : TestCase("file size") {}

        void run()
        {
            FSEntry f("fs_entry_TEST_dir/ten_bytes");
            FSEntry d("fs_entry_TEST_dir/dir_a");
            FSEntry e("fs_entry_TEST_dir/no_such_file");

            TEST_CHECK_EQUAL(f.file_size(), 10);
            TEST_CHECK_THROWS(d.file_size(), FSError);
            TEST_CHECK_THROWS(e.file_size(), FSError);
        }
    } test_fs_entry_size;

    /**
     * \test Test FSEntry basename and dirname methods
     *
     * \ingroup grpfilesystem
     */
    struct FSEntryBaseDirName : TestCase
    {
        FSEntryBaseDirName() : TestCase("basename and dirname") {}

        void run()
        {
            FSEntry a("/foo/bar");
            FSEntry b("/moo/went/the/cow");
            FSEntry c("/");
            FSEntry d(".");
            FSEntry e("..");

            TEST_CHECK(a.basename() == "bar");
            TEST_CHECK(stringify(a.dirname()) == "/foo");
            TEST_CHECK(b.basename() == "cow");
            TEST_CHECK(stringify(b.dirname()) == "/moo/went/the");
            TEST_CHECK(c.basename() == "/");
            TEST_CHECK(stringify(c.dirname()) == "/");
            TEST_CHECK(d.basename() == ".");
            TEST_CHECK(stringify(d.dirname()) == ".");
            TEST_CHECK(e.basename() == "..");
            TEST_CHECK(stringify(e.dirname()) == "..");
        }
    } test_fs_entry_dir_base_name;
}

