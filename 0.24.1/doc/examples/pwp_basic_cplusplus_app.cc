/* vim: set sw=4 sts=4 et foldmethod=syntax : */

#include <paludis/paludis.hh>
#include <paludis/environments/environment_maker.hh>

#include <iostream>
#include <tr1/memory>
#include <cstdlib>

using std::cout;
using std::cerr;
using std::endl;

int main(int, char *[])
{
    try
    {
        std::tr1::shared_ptr<paludis::Environment> env(
                paludis::EnvironmentMaker::get_instance()->make_from_spec(""));

        std::tr1::shared_ptr<const paludis::PackageDatabaseEntryCollection> packages(
                env->package_database()->query(
                    paludis::query::Matches(paludis::PackageDepSpec("app-editors/vim", paludis::pds_pm_eapi_0_strict)) &
                    paludis::query::InstalledAtRoot(env->root()), paludis::qo_order_by_version));

        if (packages->empty())
            cout << "Vim is not installed" << endl;
        else
            cout << "Vim " << packages->last()->version << " is installed" << endl;
    }
    catch (const paludis::Exception & e)
    {
        cerr << "Caught exception '" << e.message() << "' ("
            << e.what() << ")" << endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

