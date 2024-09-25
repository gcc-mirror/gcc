// { dg-do compile { target c++23 } }

#include <expected>

struct E {
  E() = default;
  E(E&&) = default;
};

void
test_pr105146()
{
  std::bad_expected_access(E{});
}

void
test_lwg4031()
{
  struct test_type : std::bad_expected_access<void> { };

  static_assert( std::is_nothrow_default_constructible_v<test_type> );
  // LWG 4031. bad_expected_access<void> member functions should be noexcept
  static_assert( std::is_nothrow_copy_constructible_v<test_type> );
  static_assert( std::is_nothrow_move_constructible_v<test_type> );
  static_assert( std::is_nothrow_copy_assignable_v<test_type> );
  static_assert( std::is_nothrow_move_assignable_v<test_type> );
}
