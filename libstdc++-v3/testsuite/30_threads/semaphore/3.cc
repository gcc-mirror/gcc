// { dg-do compile { target *-*-*linux* } }
// { dg-require-effective-target c++20 }
// { dg-require-effective-target hosted }

#include <semaphore>
#include <limits.h>

// on Linux these specializations all use a futex:
static_assert(sizeof(std::counting_semaphore<0>) == sizeof(int));
static_assert(sizeof(std::counting_semaphore<1>) == sizeof(int));
static_assert(sizeof(std::counting_semaphore<INT_MAX>) == sizeof(int));
static_assert(sizeof(std::counting_semaphore<>) == sizeof(int));

// This will use a futex iff ptrdiff_t has 32 bits:
static_assert(sizeof(std::counting_semaphore<PTRDIFF_MAX>) == sizeof(std::ptrdiff_t));

#if PTRDIFF_MAX > INT_MAX
static_assert(sizeof(std::counting_semaphore<INT_MAX+1LL>) == sizeof(std::ptrdiff_t));
#endif
