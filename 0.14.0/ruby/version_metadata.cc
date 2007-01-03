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

#include <paludis_ruby.hh>
#include <paludis/version_metadata.hh>
#include <paludis/package_database_entry.hh>
#include <paludis/util/stringify.hh>
#include <ruby.h>

using namespace paludis;
using namespace paludis::ruby;

#define RUBY_FUNC_CAST(x) reinterpret_cast<VALUE (*)(...)>(x)

namespace
{
    static VALUE c_version_metadata;

    template <typename T_>
    VALUE version_metadata_get_interface(VALUE self, const T_ * (VersionMetadata::* m) () const)
    {
        VersionMetadata::ConstPointer * self_ptr;
        Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
        return ((**self_ptr).*m)() ? self : Qnil;
    }

    /*
     * call-seq:
     *     get_ebuild_interface -> self or Nil
     *
     * Returns self if the VersionMetadata supports the interface, otherwise Nil.
     */
    VALUE version_metadata_get_ebuild_interface(VALUE self)
    {
        return version_metadata_get_interface(self, &VersionMetadata::get_ebuild_interface);
    }

    /*
     * call-seq:
     *     get_ebin_interface -> self or Nil
     *
     * Returns self if the VersionMetadata supports the interface, otherwise Nil.
     */
    VALUE version_metadata_get_ebin_interface(VALUE self)
    {
        return version_metadata_get_interface(self, &VersionMetadata::get_ebin_interface);
    }

    /*
     * call-seq:
     *     get_cran_interface -> self or Nil
     *
     * Returns self if the VersionMetadata supports the interface, otherwise Nil.
     */
    VALUE version_metadata_get_cran_interface(VALUE self)
    {
        return version_metadata_get_interface(self, &VersionMetadata::get_cran_interface);
    }

    /*
     * call-seq:
     *     get_virtual_interface -> self or Nil
     *
     * Returns self if the VersionMetadata supports the interface, otherwise Nil.
     */
    VALUE version_metadata_get_virtual_interface(VALUE self)
    {
        return version_metadata_get_interface(self, &VersionMetadata::get_virtual_interface);
    }

    /*
     * call-seq:
     *     license
     *
     * Fetch our license, as a DepAtom structure.
     */
    VALUE
    version_metadata_license(VALUE self)
    {
        VersionMetadata::ConstPointer * self_ptr;
        Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
        return dep_atom_to_value((*self_ptr)->license());
    }

    /*
     * Document-method: slot
     *
     * call-seq:
     *     slot
     *
     * Our slot
     */
    /*
     * Document-method: license_string
     *
     * call-seq:
     *     license_string
     *
     * Our license as a String
     */
    /*
     * Document-method: eapi
     *
     * call-seq:
     *     eapi
     *
     * Our eapi
     */
    /*
     * Document-method: homepage
     *
     * call-seq:
     *     homepage
     *
     * Our homepage
     */
    /*
     * Document-method: description
     *
     * call-seq:
     *     description
     *
     * Our description
     */
    template <typename T_, T_ VersionMetadataBase::* m_>
    struct BaseValue
    {
        static VALUE
        fetch(VALUE self)
        {
            VersionMetadata::ConstPointer * self_ptr;
            Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
            return rb_str_new2(stringify((**self_ptr).*m_).c_str());
        }
    };

    /*
     * Document-method: provide_string
     *
     * call-seq:
     *     provide_string
     *
     * Fetches the package provide_string, if get_ebuild_interface is not Nil.
     */
    /*
     * Document-method: src_uri
     *
     * call-seq:
     *     src_uri
     *
     * Fetches the package src_uri, if get_ebuild_interface is not Nil.
     */
    /*
     * Document-method: restrict_string
     *
     * call-seq:
     *     restrict_string
     *
     * Fetches the package restrict_string, if get_ebuild_interface is not Nil.
     */
    /*
     * Document-method: eclass_keywords
     *
     * call-seq:
     *     eclass_keywords
     *
     * Fetches the package eclass_keywords, if get_ebuild_interface is not Nil.
     */
    /*
     * Document-method: iuse
     *
     * call-seq:
     *     iuse
     *
     * Fetches the package iuse, if get_ebuild_interface is not Nil.
     */
    /*
     * Document-method: inherited
     *
     * call-seq:
     *     inherited
     *
     * Fetches the package inherited, if get_ebuild_interface is not Nil.
     */
    template <typename T_, T_ EbuildVersionMetadata::* m_>
    struct EbuildValue
    {
        static VALUE
        fetch(VALUE self)
        {
            VersionMetadata::ConstPointer * self_ptr;
            Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
            if ((*self_ptr)->get_ebuild_interface())
                return rb_str_new2(stringify((*self_ptr)->get_ebuild_interface()->*m_).c_str());
            else
                return Qnil;
        }
    };

    /*
     * Document-method: bin_uri
     *
     * call-seq:
     *     bin_uri -> String
     *
     * Fetches the package bin_uri, if get_ebin_interface is not Nil
     */
    /*
     * Document-method: src_repository
     *
     * call-seq:
     *     src_repository -> String
     *
     * Fetches the package src_repository, if get_ebin_interface is not Nil
     */
    template <typename T_, T_ EbinVersionMetadata::* m_>
    struct EbinValue
    {
        static VALUE
        fetch(VALUE self)
        {
            VersionMetadata::ConstPointer * self_ptr;
            Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
            if ((*self_ptr)->get_ebin_interface())
                return rb_str_new2(stringify((*self_ptr)->get_ebin_interface()->*m_).c_str());
            else
                return Qnil;
        }
    };

    /*
     * Document-method: package
     *
     * call-seq:
     *     package -> String
     *
     * Fetches the package name, if get_cran_interface is not Nil
     */
    /*
     * Document-method: version
     *
     * call-seq:
     *     version -> String
     *
     * Fetches the package version, if get_ebin_interface is not Nil
     */
    template <typename T_, T_ CRANVersionMetadata::* m_>
    struct CRANValue
    {
        static VALUE
        fetch(VALUE self)
        {
            VersionMetadata::ConstPointer * self_ptr;
            Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
            if ((*self_ptr)->get_cran_interface())
                return rb_str_new2(stringify((*self_ptr)->get_cran_interface()->*m_).c_str());
            else
                return Qnil;
        }
    };

    /*
     * Document-method: build_depend
     *
     * call-seq:
     *     build_depend -> DepAtom
     *
     * Fetches build_depend information as a DepAtom
     */
    /*
     * Document-method: run_depend
     *
     * call-seq:
     *     run_depend -> DepAtom
     *
     * Fetches run_depend information as a DepAtom
     */
    /*
     * Document-method: post_depend
     *
     * call-seq:
     *     post_depend -> DepAtom
     *
     * Fetches post_depend information as a DepAtom
     */
    template <DepAtom::ConstPointer (VersionMetadataDeps::* m_) () const>
    struct DependValue
    {
        static VALUE
        fetch(VALUE self)
        {
            VersionMetadata::ConstPointer * self_ptr;
            Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
            // don't change the line below to something cleaner, it makes g++-4.1 puke
            return dep_atom_to_value(((&(*self_ptr)->deps)->*m_)());
        }
    };

    /*
     * Document-method: build_depend_string
     *
     * call-seq:
     *     build_depend_string -> String
     *
     * Fetches build_depend information as a String
     */
    /*
     * Document-method: run_depend_string
     *
     * call-seq:
     *     run_depend_string -> String
     *
     * Fetches run_depend information as a String
     */
    /*
     * Document-method: post_depend_string
     *
     * call-seq:
     *     post_depend_string -> String
     *
     * Fetches post_depend information as a String
     */
    template <std::string VersionMetadataDeps::* m_>
    struct DependValueString
    {
        static VALUE
        fetch(VALUE self)
        {
            VersionMetadata::ConstPointer * self_ptr;
            Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
            return rb_str_new2(((&(*self_ptr)->deps)->*m_).c_str());
        }
    };
    /*
     * Document-method: origin_source
     *
     * call-seq:
     *     origin_source -> PackageDatabaseEntry
     *
     * Returnd the PackageDatabaseEntry from which the package was installed.
     */
    template <CountedPtr<PackageDatabaseEntry, count_policy::ExternalCountTag> VersionMetadataOrigins::* m_>
    struct VMOrigins
    {
        static VALUE
        fetch(VALUE self)
        {
            VersionMetadata::ConstPointer * self_ptr;
            Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
            if ((&(*self_ptr)->origins)->*m_)
            {
                return package_database_entry_to_value(*((&(*self_ptr)->origins)->*m_));
            }
            else
            {
                return Qnil;
            }
        }
    };

    /*
     * call-seq:
     *     virtual_for -> PackageDatabaseEntry
     *
     * Fetch package we are a virtual for, if get_virtual_interface is not Nil.
     */
    VALUE version_metadata_virtual_for(VALUE self)
    {
        VersionMetadata::ConstPointer * self_ptr;
        Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
        if ((*self_ptr)->get_virtual_interface())
            return package_database_entry_to_value((*self_ptr)->get_virtual_interface()->virtual_for);
        else
            return Qnil;

    }

    /*
     * call-seq:
     *     is_bundle? -> true or false
     *
     * Are we a bundle? True or false, if get_virtual_interface is not Nil.
     */
    VALUE version_metadata_is_bundle(VALUE self)
    {
        VersionMetadata::ConstPointer * self_ptr;
        Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
        if ((*self_ptr)->get_cran_interface())
            return ((*self_ptr)->get_cran_interface()->is_bundle) ? Qtrue : Qfalse;
        else
            return Qnil;

    }

    /*
     * call-seq:
     *     keywords -> String
     *
     * Fetches the package keywords, if get_ebuild_interface or get_cran_interface is not Nil.
     */
    VALUE version_metadata_keywords(VALUE self)
    {
        VersionMetadata::ConstPointer * self_ptr;
        Data_Get_Struct(self, VersionMetadata::ConstPointer, self_ptr);
        if ((*self_ptr)->get_ebuild_interface())
            return rb_str_new2(((*self_ptr)->get_ebuild_interface()->keywords).c_str());
        if ((*self_ptr)->get_cran_interface())
            return rb_str_new2(((*self_ptr)->get_cran_interface()->keywords).c_str());
        else
            return Qnil;
    }

    void do_register_version_metadata()
    {
        /*
         * Document-class: Paludis::VersionMetadata
         *
         * Metadata about a package version.
         */
        c_version_metadata = rb_define_class_under(paludis_module(), "VersionMetadata", rb_cObject);
        rb_funcall(c_version_metadata, rb_intern("private_class_method"), 1, rb_str_new2("new"));
        rb_define_method(c_version_metadata, "get_ebuild_interface", RUBY_FUNC_CAST(&version_metadata_get_ebuild_interface), 0);
        rb_define_method(c_version_metadata, "get_virtual_interface", RUBY_FUNC_CAST(&version_metadata_get_virtual_interface), 0);
        rb_define_method(c_version_metadata, "get_ebin_interface", RUBY_FUNC_CAST(&version_metadata_get_ebin_interface), 0);
        rb_define_method(c_version_metadata, "get_cran_interface", RUBY_FUNC_CAST(&version_metadata_get_cran_interface), 0);

        rb_define_method(c_version_metadata, "license", RUBY_FUNC_CAST(&version_metadata_license), 0);

        rb_define_method(c_version_metadata, "slot", RUBY_FUNC_CAST((&BaseValue<SlotName, &VersionMetadataBase::slot>::fetch)), 0);
        rb_define_method(c_version_metadata, "license_string", RUBY_FUNC_CAST((&BaseValue<std::string,
                        &VersionMetadataBase::license_string>::fetch)), 0);
        rb_define_method(c_version_metadata, "eapi", RUBY_FUNC_CAST((&BaseValue<std::string, &VersionMetadataBase::eapi>::fetch)), 0);
        rb_define_method(c_version_metadata, "homepage", RUBY_FUNC_CAST((&BaseValue<std::string, &VersionMetadataBase::homepage>::fetch)), 0);
        rb_define_method(c_version_metadata, "description", RUBY_FUNC_CAST((&BaseValue<std::string,
                        &VersionMetadataBase::description>::fetch)), 0);

        rb_define_method(c_version_metadata, "provide_string", RUBY_FUNC_CAST((&EbuildValue<std::string,
                        &EbuildVersionMetadata::provide_string>::fetch)), 0);
        rb_define_method(c_version_metadata, "src_uri", RUBY_FUNC_CAST((&EbuildValue<std::string,
                        &EbuildVersionMetadata::src_uri>::fetch)), 0);
        rb_define_method(c_version_metadata, "restrict_string", RUBY_FUNC_CAST((&EbuildValue<std::string,
                        &EbuildVersionMetadata::restrict_string>::fetch)), 0);
        rb_define_method(c_version_metadata, "eclass_keywords", RUBY_FUNC_CAST((&EbuildValue<std::string,
                        &EbuildVersionMetadata::eclass_keywords>::fetch)), 0);
        rb_define_method(c_version_metadata, "iuse", RUBY_FUNC_CAST((&EbuildValue<std::string,
                        &EbuildVersionMetadata::iuse>::fetch)), 0);
        rb_define_method(c_version_metadata, "inherited", RUBY_FUNC_CAST((&EbuildValue<std::string,
                        &EbuildVersionMetadata::inherited>::fetch)), 0);

        rb_define_method(c_version_metadata, "build_depend", RUBY_FUNC_CAST((&DependValue<
                        &VersionMetadataDeps::build_depend>::fetch)), 0);
        rb_define_method(c_version_metadata, "run_depend", RUBY_FUNC_CAST((&DependValue<
                        &VersionMetadataDeps::run_depend>::fetch)), 0);
        rb_define_method(c_version_metadata, "post_depend", RUBY_FUNC_CAST((&DependValue<
                        &VersionMetadataDeps::post_depend>::fetch)), 0);

        rb_define_method(c_version_metadata, "build_depend_string", RUBY_FUNC_CAST((&DependValueString<
                        &VersionMetadataDeps::build_depend_string>::fetch)), 0);
        rb_define_method(c_version_metadata, "run_depend_string", RUBY_FUNC_CAST((&DependValueString<
                        &VersionMetadataDeps::run_depend_string>::fetch)), 0);
        rb_define_method(c_version_metadata, "post_depend_string", RUBY_FUNC_CAST((&DependValueString<
                        &VersionMetadataDeps::post_depend_string>::fetch)), 0);

        rb_define_method(c_version_metadata, "origin_source", RUBY_FUNC_CAST((&VMOrigins<
                        &VersionMetadataOrigins::source>::fetch)), 0);
        rb_define_method(c_version_metadata, "origin_binary", RUBY_FUNC_CAST((&VMOrigins<
                        &VersionMetadataOrigins::binary>::fetch)), 0);

        rb_define_method(c_version_metadata, "virtual_for", RUBY_FUNC_CAST(&version_metadata_virtual_for), 0);

        rb_define_method(c_version_metadata, "bin_uri", RUBY_FUNC_CAST((&EbinValue<std::string,
                        &EbinVersionMetadata::bin_uri>::fetch)), 0);
        rb_define_method(c_version_metadata, "src_repository", RUBY_FUNC_CAST((&EbinValue<RepositoryName,
                        &EbinVersionMetadata::src_repository>::fetch)), 0);

        rb_define_method(c_version_metadata, "package", RUBY_FUNC_CAST((&CRANValue<std::string,
                        &CRANVersionMetadata::package>::fetch)), 0);
        rb_define_method(c_version_metadata, "version", RUBY_FUNC_CAST((&CRANValue<std::string,
                        &CRANVersionMetadata::version>::fetch)), 0);
        rb_define_method(c_version_metadata, "is_bundle?", RUBY_FUNC_CAST(&version_metadata_is_bundle), 0);

        rb_define_method(c_version_metadata, "keywords", RUBY_FUNC_CAST(&version_metadata_keywords), 0);

    }
}

VALUE
paludis::ruby::version_metadata_to_value(VersionMetadata::ConstPointer m)
{
    VersionMetadata::ConstPointer * m_ptr(0);
    try
    {
        m_ptr = new VersionMetadata::ConstPointer(m);
        return Data_Wrap_Struct(c_version_metadata, 0, &Common<VersionMetadata::ConstPointer>::free, m_ptr);
    }
    catch (const std::exception & e)
    {
        delete m_ptr;
        exception_to_ruby_exception(e);
    }
}

VersionMetadata::ConstPointer
paludis::ruby::value_to_version_metadata(VALUE v)
{
    if (rb_obj_is_kind_of(v, c_version_metadata))
    {
        VersionMetadata::ConstPointer * v_ptr;
        Data_Get_Struct(v, VersionMetadata::ConstPointer, v_ptr);
        return *v_ptr;
    }
    else
    {
        rb_raise(rb_eTypeError, "Can't convert %s into VersionMetadata", rb_obj_classname(v));
    }
}

RegisterRubyClass::Register paludis_ruby_register_version_metadata PALUDIS_ATTRIBUTE((used))
    (&do_register_version_metadata);


