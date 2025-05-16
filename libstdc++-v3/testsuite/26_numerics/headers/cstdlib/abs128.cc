// { dg-do compile }
// { dg-add-options strict_std }

#include <cstdlib>

template<typename T> T same_type(T, T) { return T(); }

#ifdef __SIZEOF_INT128__
__int128 i = 0;
__int128 j = same_type(std::abs(i), i);
#endif

#ifdef __SIZEOF_FLOAT128__
__float128 f = 0.0;
__float128 g = same_type(std::abs(f), f);
#endif
