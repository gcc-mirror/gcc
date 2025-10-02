// { dg-do run { target c++26 } }
#include <mdspan>

#include <cstdint>
#include <testsuite_hooks.h>

constexpr void
check_strided_slice(auto s, auto offset, auto extent, auto stride)
{
  using slice_type = std::strided_slice<decltype(offset), decltype(extent),
					decltype(stride)>;
  static_assert(std::same_as<decltype(s), slice_type>);
  VERIFY(s.offset == offset);
  VERIFY(s.extent == extent);
  VERIFY(s.stride == stride);
}

constexpr void
test_initializers(auto offset, auto extent, auto stride)
{
  auto check = [&](auto s)
  {
    check_strided_slice(s, offset, extent, stride);
  };

  check(std::strided_slice{.offset=offset, .extent=extent, .stride=stride});
  check(std::strided_slice{offset, extent, stride});
  check(std::strided_slice(offset, extent, stride));
}

constexpr bool
test_all()
{
  test_initializers(0, 1, 2);
  test_initializers(std::integral_constant<short, 0>{}, size_t{1}, std::cw<2>);
  test_initializers(-1, 2, 2);
  return true;
}

int
main()
{
  test_all();
  static_assert(test_all());
  return 0;
}
