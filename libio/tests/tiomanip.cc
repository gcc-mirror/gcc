// test the parametrized manipulators

#include <stdlib.h>
#include <iomanip.h>

main()
{
#ifdef _G_NO_TEMPLATES
    cerr << "(IO manipulators are not supported with this compiler)\n");
    exit(-1);
#else

    cout << dec  << 1234 << ' '
         << hex  << 1234 << ' '
	 << oct  << 1234 << endl;

    //SMANIP<int> x = setw(4);
    //operator<<(cout, x);

    cout
      << "("
      << dec << setw(4) << setfill('*') 
      << 12 << ")\n";
    
    cout << "(" << 12 << ")\n";

    cout << setiosflags(ios::internal);
    cout << "(" << setw(6) << -12 << ")\n";

    exit(0);
#endif
}


					
