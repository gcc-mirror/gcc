// { dg-do compile { target c++11 } }

// Bug 121046
// Asking is_constructible_v<std::bitset<1>, NonTrivial*> is ill-formed

// LWG 4294. bitset(const CharT*) constructor needs to be constrained

#include <bitset>
struct NonTrivial { ~NonTrivial() { } };
static_assert( ! std::is_constructible<std::bitset<1>, NonTrivial*>::value,
	       "std::bitset cannot be constructed from this pointer" );
