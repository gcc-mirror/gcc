// { dg-do compile { target c++11 } }
#include <type_traits>

// Check that we can name invalid specializations, just don't instantiate them.

using X = std::make_signed<float>;
using Y = std::make_signed<bool>;
using Z = std::make_signed<void>;
