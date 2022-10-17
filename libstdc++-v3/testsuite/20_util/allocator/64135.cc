// { dg-do compile { target std_allocator_new } }
// { dg-add-options no_pch }
// { dg-require-effective-target hosted }

// PR libstdc++/64135

#define new_allocator 1
#define malloc_allocator 1
#define bitmap_allocator 1
#include <memory>

#if __cplusplus >= 201103L
#define STATIC_ASSERT(X) static_assert((X), #X)
#else
#define PASTE2(X, Y) X##Y
#define PASTE(X, Y) PASTE2(X, Y)
#define STATIC_ASSERT(X) char PASTE(_assertion_, __LINE__) [(X) ? 1 : -1]
#endif

#undef new_allocator
#undef malloc_allocator
#include <ext/new_allocator.h>
#include <ext/malloc_allocator.h>

struct N : __gnu_cxx::new_allocator<char> { };

struct A : std::allocator<char>, N { };
struct B : std::allocator<char> { N n; };

// Verify that layout was not changed by removing std::allocator inheritance
// from __gnu_cxx::new_allocator:
STATIC_ASSERT( sizeof(A) == 2 );
STATIC_ASSERT( sizeof(B) == 2 );

struct M : __gnu_cxx::malloc_allocator<char> { };
struct C : N, M { };

// Verify that malloc_allocator can be an overlapping subobject with
// __new_allocator:
STATIC_ASSERT( sizeof(M) == 1 );
STATIC_ASSERT( sizeof(C) == 1 );

struct D : std::allocator<char>, M { };

// This test uses { target std_allocator_new } so this is true too:
STATIC_ASSERT( sizeof(D) == 1 );
