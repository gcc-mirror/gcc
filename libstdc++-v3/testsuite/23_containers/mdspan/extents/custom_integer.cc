// { dg-do run { target c++23 } }
#include <mdspan>

#include <testsuite_hooks.h>

// Test construction from a custom integer-like object, that has
// no copy/move ctor or copy/move assignment operator.

constexpr size_t dyn = std::dynamic_extent;

class IntLike
{
public:
  explicit
  IntLike(int i)
  : _M_i(i)
  { }

  IntLike() = delete;
  IntLike(const IntLike&) = delete;
  IntLike(IntLike&&) = delete;

  const IntLike&
  operator=(const IntLike&) = delete;

  const IntLike&
  operator=(IntLike&&) = delete;

  constexpr
  operator int() const noexcept
  { return _M_i; }

private:
  int _M_i;
};

static_assert(std::is_convertible_v<IntLike, int>);
static_assert(std::is_nothrow_constructible_v<int, IntLike>);

void
test_shape(const auto& s2, const auto& s23)
{
  std::extents<int, 2, 3> expected;

  std::extents<int, 2, 3> e1(s23);
  VERIFY(e1 == expected);

  std::extents<int, dyn, 3> e2(s2);
  VERIFY(e2 == expected);

  std::extents<int, dyn, 3> e3(s23);
  VERIFY(e3 == expected);

  std::extents<int, dyn, dyn> e4(s23);
  VERIFY(e4 == expected);
}

void
test_pack()
{
  std::extents<int, 2, 3> expected;

  std::extents<int, dyn, 3> e1(IntLike(2));
  VERIFY(e1 == expected);

  std::extents<int, dyn, 3> e2(IntLike(2), IntLike(3));
  VERIFY(e2 == expected);

  std::extents<int, dyn, dyn> e3(IntLike(2), IntLike(3));
  VERIFY(e3 == expected);
}

int
main()
{
  auto a2 = std::array<IntLike, 1>{IntLike(2)};
  auto s2 = std::span<IntLike, 1>(a2);

  auto a23 = std::array<IntLike, 2>{IntLike(2), IntLike(3)};
  auto s23 = std::span<IntLike, 2>(a23);

  test_shape(a2, a23);
  test_shape(s2, s23);
  test_pack();

  return 0;
}
