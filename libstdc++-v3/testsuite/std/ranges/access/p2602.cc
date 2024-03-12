// { dg-do compile { target c++20 } }

// P2602R2 Poison Pills are Too Toxic

#include <ranges>

struct A {
    friend auto begin(A const&) -> int const*;
    friend auto end(A const&)   -> int const*;
};

struct B {
    friend auto begin(B&) -> int*;
    friend auto end(B&) -> int*;
};

static_assert( std::ranges::range<A> );
static_assert( std::ranges::range<const A> );
static_assert( std::ranges::range<B> );
static_assert( ! std::ranges::range<const B> );

class Test {
    friend size_t size(const Test&) {
	return 0;
    }
};

size_t f(Test t) {
   return std::ranges::size(t);
}
