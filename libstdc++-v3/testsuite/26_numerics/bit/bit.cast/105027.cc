// { dg-do compile { target c++20 } }

// PR libstdc++/105027 - Missing constraints on std::bit_cast

#include <bit>

template<class T, class U>
concept BitCastable = requires(const U& u) { std::bit_cast<T>(u); };

static_assert(BitCastable<int, unsigned>); // OK

static_assert(!BitCastable<int, char>); // #1: different size

struct A { A(A const&); int i; };
static_assert(!BitCastable<int, A>); // #2: not trivially copyable

static_assert(!BitCastable<long, int()>); // #3: sizeof(int()) is ill-formed
