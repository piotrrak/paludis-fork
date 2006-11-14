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

#ifndef PALUDIS_GUARD_PALUDIS_QA_METADATA_FILE_HH
#define PALUDIS_GUARD_PALUDIS_QA_METADATA_FILE_HH 1

#include <paludis/util/private_implementation_pattern.hh>
#include <libwrapiter/libwrapiter_forward_iterator.hh>

namespace paludis
{
    class FSEntry;

    namespace qa
    {
        /**
         * Wrapper around metadata.xml files.
         *
         * \ingroup grpqa
         */
        class MetadataFile :
            public PrivateImplementationPattern<MetadataFile>
        {
            public:
                MetadataFile(const FSEntry & location);
                MetadataFile(const std::string & text);
                ~MetadataFile();

                typedef libwrapiter::ForwardIterator<MetadataFile, const std::string> HerdsIterator;
                HerdsIterator begin_herds() const;
                HerdsIterator end_herds() const;

                typedef libwrapiter::ForwardIterator<MetadataFile, const std::pair<std::string, std::string> > MaintainersIterator;
                MaintainersIterator begin_maintainers() const;
                MaintainersIterator end_maintainers() const;
        };
    }
}

#endif
