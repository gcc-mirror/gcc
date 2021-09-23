// { dg-do compile { target c++17 }  }

// LWG 2762 adds noexcept to operator-> and operator*
#include <optional>

struct S
{
  void can_throw();
  void cannot_throw() noexcept;
};

static_assert( ! noexcept(std::declval<std::optional<S>&>()->can_throw()) );
static_assert( noexcept(std::declval<std::optional<S>&>()->cannot_throw()) );

static_assert( noexcept(std::declval<std::optional<S>&>().operator->()) );
static_assert( noexcept(std::declval<std::optional<int>&>().operator->()) );

static_assert( noexcept(*std::declval<std::optional<int>&>()) );
static_assert( noexcept(*std::declval<const std::optional<int>&>()) );
static_assert( noexcept(*std::declval<std::optional<int>&&>()) );
static_assert( noexcept(*std::declval<const std::optional<int>&&>()) );
