// { dg-do compile { target c++11 } }
#include <system_error>

extern const std::error_category& cat;

static_assert(std::is_nothrow_destructible<std::error_category>::value, "");
static_assert(noexcept(cat.name()), "");
static_assert(noexcept(cat.default_error_condition(1)), "");
static_assert(noexcept(cat.equivalent(1, {})), "");
static_assert(noexcept(cat.equivalent({}, 1)), "");
static_assert(noexcept(cat == cat), "");
static_assert(noexcept(cat != cat), "");
static_assert(noexcept(cat < cat), "");
