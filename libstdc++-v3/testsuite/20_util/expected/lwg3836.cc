// { dg-do run { target c++23 } }

#include <expected>
#include <testsuite_hooks.h>

constexpr void
test_convert_contained_value_to_bool()
{
  struct BaseError { };
  struct DerivedError : BaseError { };

  std::expected<bool, DerivedError> e = false;

  // Should use expected(const expected<U, G>&) ctor, not expected(U&&):
  std::expected<bool, BaseError> e2 = e;

  // Contained value should be e.value() not static_cast<bool>(e):
  VERIFY( e2.value() == false );

  std::expected<bool, DerivedError> e3(std::unexpect);
  std::expected<const bool, BaseError> e4 = e3;
  // Should have error, not static_cast<bool>(e3):
  VERIFY( ! e4.has_value() );
}

int main()
{
  test_convert_contained_value_to_bool();

  static_assert([] {
    test_convert_contained_value_to_bool();
    return true;
  }());
}
