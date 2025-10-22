// { dg-do compile { target c++26 }  }

#include <optional>
#include <variant>

template<typename T>
constexpr bool _Never_valueless
 = std::__detail::__variant::_Never_valueless_alt<T>::value;

static_assert( _Never_valueless<std::optional<int&>> );
static_assert( _Never_valueless<std::optional<const int&>> );
