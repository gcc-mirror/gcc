// { dg-do compile { target c++11 } }

#include <vector>

static_assert(
    !std::is_default_constructible<std::vector<bool>::reference>::value,
    "std::vector<bool>::reference is not default constructible"
    );
