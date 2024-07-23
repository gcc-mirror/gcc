// { dg-do run { target c++17 } }

#include <optional>
#include <testsuite_hooks.h>

constexpr void
test_convert_contained_value_to_bool()
{
  struct False { constexpr operator bool() const { return false; } };

  False f;
  std::optional<False> o = f;

  // Should use optional(const optional<U>&) ctor, not optional(U&&):
  std::optional<bool> o2 = o;

  // Contained value should be static_cast<bool>(f) not static_cast<bool>(o):
  VERIFY( o2.value() == false );

  std::optional<False> o3;
  std::optional<const bool> o4 = o3;
  // Should have no contained value, not static_cast<bool>(o3):
  VERIFY( ! o4.has_value() );
}

constexpr void
test_convert_contained_value_to_bool_explicit()
{
  struct False { constexpr explicit operator bool() const { return false; } };

  False f;
  std::optional<False> o = f;

  // Should use optional(const optional<U>&) ctor, not optional(U&&):
  std::optional<bool> o2(o);

  // Contained value should be static_cast<bool>(f) not static_cast<bool>(o):
  VERIFY( o2.value() == false );

  std::optional<False> o3;
  std::optional<const bool> o4(o3);
  // Should have no contained value, not static_cast<bool>(o3):
  VERIFY( ! o4.has_value() );
}

int main()
{
  test_convert_contained_value_to_bool();
  test_convert_contained_value_to_bool_explicit();

#if __cpp_lib_optional >= 202106
  static_assert([] {
    test_convert_contained_value_to_bool();
    test_convert_contained_value_to_bool_explicit();
    return true;
  }());
#endif
}
