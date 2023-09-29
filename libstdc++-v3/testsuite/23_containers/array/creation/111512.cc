// { dg-do compile { target c++20 } }

// Bug libstdc++/111511 - Incorrect ADL in std::to_array in GCC 11/12/13
// Bug c++/111512 - GCC's __builtin_memcpy can trigger ADL

#include <array>
#include <utility>

struct incomplete;

template<class T>
struct holder {
    T t; // { dg-bogus "'holder<T>::t' has incomplete type" }
};

// A complete type that cannot be used as an associated type for ADL.
using adl_bomb = holder<incomplete>*;

int main()
{
    adl_bomb a[1]{};
    (void) std::to_array(a);
    (void) std::to_array(std::move(a));
}
