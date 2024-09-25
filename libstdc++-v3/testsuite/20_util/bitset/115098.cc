// { dg-do compile { target c++11 } }

#include <bitset>

using namespace std;

static_assert( ! is_default_constructible<bitset<10>::reference>::value,
    "std::bitset<N>::reference is not default constructible");

static_assert( ! is_constructible<bitset<10>::reference, bitset<10>&, size_t>::value,
    "std::bitset<N>::reference is not default constructible");
