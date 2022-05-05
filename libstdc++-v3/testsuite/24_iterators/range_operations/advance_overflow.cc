// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

// Public domain testcase from Casey Carter, send to LWG list on 2021-07-24.
//
// Here's a compile-only test case for which n is INT_MIN, which will overflow
// if simply negated to get |n|: https://godbolt.org/z/M7Wz1nW58.

#include <cassert>
#include <iterator>
#include <limits>

struct I {
    using difference_type = int;
    using value_type = int;

    int x;

    constexpr int operator*() const { return x; }
    constexpr I& operator++() { ++x; return *this; }
    constexpr I operator++(int) { ++x; return {x - 1}; }
    constexpr bool operator==(const I&) const = default;

    constexpr int operator-(const I& that) const { return x - that.x; }

    constexpr I& operator--() { --x; return *this; }
    constexpr I operator--(int) { --x; return {x - 1}; }
};
static_assert(std::bidirectional_iterator<I>);
static_assert(std::sized_sentinel_for<I, I>);

constexpr bool test() {
    using L = std::numeric_limits<int>;
    I i{-2};
    return std::ranges::advance(i, L::min(), I{-4}) == L::min() + 2;
}
static_assert(test());
