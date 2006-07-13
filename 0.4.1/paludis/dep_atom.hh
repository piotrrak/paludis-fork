/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2005, 2006 Ciaran McCreesh <ciaran.mccreesh@blueyonder.co.uk>
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

#ifndef PALUDIS_GUARD_PALUDIS_DEP_ATOM_HH
#define PALUDIS_GUARD_PALUDIS_DEP_ATOM_HH 1

#include <list>
#include <paludis/dep_tag.hh>
#include <paludis/name.hh>
#include <paludis/util/attributes.hh>
#include <paludis/util/counted_ptr.hh>
#include <paludis/util/instantiation_policy.hh>
#include <paludis/util/visitor.hh>
#include <paludis/version_operator.hh>
#include <paludis/version_spec.hh>
#include <map>

/** \file
 * Declarations for the DepAtom classes.
 *
 * \ingroup grpdepatoms
 */

namespace paludis
{
    class DepAtom;
    class CompositeDepAtom;
    class PackageDepAtom;
    class PlainTextDepAtom;
    class AllDepAtom;
    class AnyDepAtom;
    class UseDepAtom;
    class BlockDepAtom;

    /**
     * Visitor types for a visitor that can visit a DepAtom heirarchy.
     *
     * \ingroup grpdepatoms
     */
    typedef VisitorTypes<PackageDepAtom *, PlainTextDepAtom *, AllDepAtom *, AnyDepAtom *,
            UseDepAtom *, BlockDepAtom *> DepAtomVisitorTypes;

    /**
     * Base class for a dependency atom.
     *
     * \ingroup grpdepatoms
     */
    class DepAtom :
        public virtual VisitableInterface<DepAtomVisitorTypes>,
        private InstantiationPolicy<DepAtom, instantiation_method::NonCopyableTag>,
        public InternalCounted<DepAtom>
    {
        protected:
            DepAtom();

        public:
            ///\name Basic operations
            ///\{

            virtual ~DepAtom();

            ///\}

            ///\name Upcasts
            ///\{

            /**
             * Return us as a UseDepAtom, or 0 if we are not a
             * UseDepAtom.
             */
            virtual const UseDepAtom * as_use_dep_atom() const;

            ///\}
    };

    /**
     * Class for dependency atoms that have a number of child dependency
     * atoms.
     *
     * \ingroup grpdepatoms
     */
    class CompositeDepAtom :
        public DepAtom
    {
        private:
            std::list<DepAtom::ConstPointer> _children;

        protected:
            ///\name Basic operations
            ///\{

            CompositeDepAtom();

            ///\}

        public:
            ///\name Modify our children
            ///\{

            /**
             * Append a child to our collection.
             */
            virtual void add_child(DepAtom::ConstPointer);

            ///\}

            ///\name Iterate over our children
            ///\{

            typedef std::list<DepAtom::ConstPointer>::const_iterator Iterator;

            Iterator begin() const
            {
                return _children.begin();
            }

            Iterator end() const
            {
                return _children.end();
            }

            ///\name Pointer types
            ///\{
            typedef CountedPtr<CompositeDepAtom, count_policy::InternalCountTag> Pointer;

            typedef CountedPtr<const CompositeDepAtom, count_policy::InternalCountTag> ConstPointer;
            ///\}
    };

    /**
     * Represents a "|| ( )" dependency block.
     *
     * \ingroup grpdepatoms
     */
    class AnyDepAtom :
        public CompositeDepAtom,
        public Visitable<AnyDepAtom, DepAtomVisitorTypes>
    {
        public:
            ///\name Basic operations
            ///\{

            AnyDepAtom();

            ///\}
    };

    /**
     * Represents a ( first second third ) or top level group of dependency
     * atoms.
     *
     * \ingroup grpdepatoms
     */
    class AllDepAtom :
        public CompositeDepAtom,
        public Visitable<AllDepAtom, DepAtomVisitorTypes>
    {
        public:
            ///\name Basic operations
            ///\{

            AllDepAtom();

            ///\}
    };

    /**
     * Represents a use? ( ) dependency atom.
     *
     * \ingroup grpdepatoms
     */
    class UseDepAtom :
        public CompositeDepAtom,
        public Visitable<UseDepAtom, DepAtomVisitorTypes>
    {
        private:
            const UseFlagName _flag;
            const bool _inverse;

        public:
            ///\name Basic operations
            ///\{

            UseDepAtom(const UseFlagName &, bool);

            ///\}

            /**
             * Fetch our use flag name.
             */
            const UseFlagName & flag() const
            {
                return _flag;
            }

            /**
             * Fetch whether we are a ! flag.
             */
            bool inverse() const
            {
                return _inverse;
            }

            virtual const UseDepAtom * as_use_dep_atom() const;
    };

    /**
     * A StringDepAtom represents a non-composite dep atom with an associated
     * piece of text.
     *
     * \ingroup grpdepatoms
     */
    class StringDepAtom :
        public DepAtom
    {
        private:
            const std::string _str;

        protected:
            ///\name Basic operations
            ///\{

            StringDepAtom(const std::string &);

            ~StringDepAtom();

            ///\}

        public:
            /**
             * Fetch our text.
             */
            const std::string & text() const
            {
                return _str;
            }
    };

    /**
     * A selection of USE flag requirements.
     *
     * \ingroup grpdepatoms
     */
    class UseRequirements :
        public InternalCounted<UseRequirements>
    {
        private:
            std::map<UseFlagName, UseFlagState> _reqs;

        public:
            ///\name Iterate over our USE requirements
            ///\{

            typedef std::map<UseFlagName, UseFlagState>::const_iterator Iterator;

            Iterator begin() const
            {
                return _reqs.begin();
            }

            Iterator end() const
            {
                return _reqs.end();
            }

            ///\}

            /// Find the requirement for a particular USE flag.
            Iterator find(const UseFlagName & u) const
            {
                return _reqs.find(u);
            }

            /// Insert a new requirement.
            bool insert(const UseFlagName & u, UseFlagState s)
            {
                return _reqs.insert(std::make_pair(u, s)).second;
            }

            /// What state is desired for a particular use flag?
            UseFlagState state(const UseFlagName &) const;
    };

    /**
     * A PackageDepAtom represents a package name (for example,
     * 'app-editors/vim'), possibly with associated version and SLOT
     * restrictions.
     *
     * \ingroup grpdepatoms
     */
    class PackageDepAtom :
        public StringDepAtom,
        public Visitable<PackageDepAtom, DepAtomVisitorTypes>
    {
        private:
            QualifiedPackageName _package;
            VersionOperator _version_operator;
            CountedPtr<VersionSpec, count_policy::ExternalCountTag> _version_spec;
            CountedPtr<SlotName, count_policy::ExternalCountTag> _slot;
            CountedPtr<RepositoryName, count_policy::ExternalCountTag> _repository;
            UseRequirements::Pointer _use_requirements;
            DepTag::ConstPointer _tag;

        public:
            ///\name Basic operations
            ///\{

            /**
             * Constructor, no version or SLOT restrictions.
             */
            PackageDepAtom(const QualifiedPackageName & package);

            /**
             * Constructor, parse restrictions ourself.
             */
            PackageDepAtom(const std::string &);

            ~PackageDepAtom();

            ///\}

            /**
             * Fetch the package name.
             */
            const QualifiedPackageName & package() const
            {
                return _package;
            }

            /**
             * Fetch the version operator.
             */
            const VersionOperator version_operator() const
            {
                return _version_operator;
            }

            /**
             * Fetch the version spec (may be a zero pointer).
             */
            CountedPtr<VersionSpec, count_policy::ExternalCountTag> version_spec_ptr() const
            {
                return _version_spec;
            }

            /**
             * Fetch the slot name (may be a zero pointer).
             */
            CountedPtr<SlotName, count_policy::ExternalCountTag> slot_ptr() const
            {
                return _slot;
            }

            /**
             * Fetch the repo name (may be a zero pointer).
             */
            CountedPtr<RepositoryName, count_policy::ExternalCountTag> repository_ptr() const
            {
                return _repository;
            }

            /**
             * Fetch the use requirements (may be a zero pointer).
             */
            UseRequirements::ConstPointer use_requirements_ptr() const
            {
                return _use_requirements;
            }

            ///\name Pointer types
            ///\{

            /**
             * A non-constant smart pointer to ourself.
             */
            typedef CountedPtr<PackageDepAtom, count_policy::InternalCountTag> Pointer;

            /**
             * A constant smart pointer to ourself.
             */
            typedef CountedPtr<const PackageDepAtom, count_policy::InternalCountTag> ConstPointer;

            ///\}

            /**
             * Fetch our tag.
             */
            DepTag::ConstPointer tag() const
            {
                return _tag;
            }

            /**
             * Set our tag.
             */
            void set_tag(const DepTag::ConstPointer & s)
            {
                _tag = s;
            }

            /**
             * Fetch a copy of ourself without the USE requirements.
             */
            Pointer without_use_requirements() const;
    };

    /**
     * A PlainTextDepAtom represents a plain text entry (for example,
     * a URI in SRC_URI).
     *
     * \ingroup grpdepatoms
     */
    class PlainTextDepAtom :
        public StringDepAtom,
        public Visitable<PlainTextDepAtom, DepAtomVisitorTypes>
    {
        public:
            ///\name Basic operations
            ///\{

            PlainTextDepAtom(const std::string &);

            ///\}

            ///\name Pointer types
            ///\{

            /**
             * A non-constant smart pointer to ourself.
             */
            typedef CountedPtr<PlainTextDepAtom, count_policy::InternalCountTag> Pointer;

            /**
             * A constant smart pointer to ourself.
             */
            typedef CountedPtr<const PlainTextDepAtom, count_policy::InternalCountTag> ConstPointer;

            ///\}
    };

    /**
     * Thrown if an invalid package dep atom specification is encountered.
     *
     * \ingroup grpexceptions
     * \ingroup grpdepatoms
     */
    class PackageDepAtomError :
        public Exception
    {
        public:
            ///\name Basic operations
            ///\{

            PackageDepAtomError(const std::string & msg) throw ();

            ///\}
    };

    /**
     * A PackageDepAtom can be written to an ostream.
     *
     * \ingroup grpdepatoms
     */
    std::ostream & operator<< (std::ostream &, const PackageDepAtom &);

    /**
     * A BlockDepAtom represents a block on a package name (for example,
     * 'app-editors/vim'), possibly with associated version and SLOT
     * restrictions.
     *
     * \ingroup grpdepatoms
     */
    class BlockDepAtom :
        public StringDepAtom,
        public Visitable<BlockDepAtom, DepAtomVisitorTypes>
    {
        private:
            PackageDepAtom::ConstPointer _atom;

        public:
            ///\name Basic operations
            ///\{

            /**
             * Constructor, with blocking atom.
             */
            BlockDepAtom(PackageDepAtom::ConstPointer atom);

            ///\}

            /**
             * Fetch the atom we're blocking.
             */
            PackageDepAtom::ConstPointer blocked_atom() const
            {
                return _atom;
            }

            ///\name Pointer operations
            ///\{

            /**
             * A non-constant smart pointer to ourself.
             */
            typedef CountedPtr<BlockDepAtom, count_policy::InternalCountTag> Pointer;

            /**
             * A constant smart pointer to ourself.
             */
            typedef CountedPtr<const BlockDepAtom, count_policy::InternalCountTag> ConstPointer;

            ///\}
    };

}

#endif
