// { dg-do compile { target c++11 } }
#include <system_error>

static_assert(std::is_nothrow_destructible<std::system_error>::value, "");
static_assert(noexcept(std::declval<const std::system_error&>().code()), "");
static_assert(noexcept(std::declval<const std::system_error&>().what()), "");
