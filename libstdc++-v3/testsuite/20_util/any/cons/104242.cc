// { dg-do compile { target c++17 } }

// PR libstdc++/104242 - Class with constructor from std::any is not copyable

#include <any>
#include <type_traits>

struct A {
    A(const A&) = default;
    explicit A(std::any value);
};
static_assert(std::is_copy_constructible_v<A>);
