/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Copyright (c) 2009 Ciaran McCreesh
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

#ifndef PALUDIS_GUARD_PALUDIS_FILE_OUTPUT_MANAGER_HH
#define PALUDIS_GUARD_PALUDIS_FILE_OUTPUT_MANAGER_HH 1

#include <paludis/file_output_manager-fwd.hh>
#include <paludis/output_manager.hh>
#include <paludis/output_manager_factory.hh>
#include <paludis/util/set-fwd.hh>
#include <paludis/util/private_implementation_pattern.hh>
#include <paludis/util/fs_entry-fwd.hh>
#include <tr1/memory>
#include <tr1/functional>

namespace paludis
{
    class PALUDIS_VISIBLE FileOutputManager :
        private PrivateImplementationPattern<FileOutputManager>,
        public OutputManager
    {
        public:
            FileOutputManager(const FSEntry &, const FSEntry &, const bool keep_on_success,
                    const bool keep_on_empty, const std::tr1::shared_ptr<OutputManager> & summary_output_manager,
                    const std::string & summary_output_stdout_message,
                    const std::string & summary_output_stderr_message);
            ~FileOutputManager();

            virtual std::ostream & stdout_stream() PALUDIS_ATTRIBUTE((warn_unused_result));
            virtual std::ostream & stderr_stream() PALUDIS_ATTRIBUTE((warn_unused_result));

            virtual void succeeded();
            virtual void message(const MessageType, const std::string &);

            static const std::tr1::shared_ptr<const Set<std::string> > factory_managers()
                PALUDIS_ATTRIBUTE((warn_unused_result));

            static const std::tr1::shared_ptr<OutputManager> factory_create(
                    const OutputManagerFactory::KeyFunction &,
                    const OutputManagerFactory::CreateChildFunction &,
                    const OutputManagerFactory::ReplaceVarsFunc &)
                PALUDIS_ATTRIBUTE((warn_unused_result));
    };

#ifdef PALUDIS_HAVE_EXTERN_TEMPLATE
    extern template class PrivateImplementationPattern<FileOutputManager>;
#endif
}

#endif