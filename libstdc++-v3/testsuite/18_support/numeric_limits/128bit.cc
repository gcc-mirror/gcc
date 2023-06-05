// { dg-do compile }

#include <limits>

#if __SIZEOF_FLOAT128__
__extension__ template class std::numeric_limits<__float128>;
#endif

#if __SIZEOF_INT128__
__extension__ template class std::numeric_limits<__int128>;
__extension__ template class std::numeric_limits<unsigned __int128>;
#endif
