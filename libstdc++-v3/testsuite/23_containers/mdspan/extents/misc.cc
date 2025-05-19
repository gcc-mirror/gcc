// { dg-do run { target c++23 } }
#include <mdspan>

#include <cstdint>
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;

// Check class traits.
static_assert(std::regular<std::extents<int>>);
static_assert(std::regular<std::extents<int, 1>>);
static_assert(std::regular<std::extents<int, dyn>>);

static_assert(std::is_trivially_copyable_v<std::extents<int>>);
static_assert(std::is_trivially_copyable_v<std::extents<int, 1>>);
static_assert(std::is_trivially_copyable_v<std::extents<int, dyn>>);

// Check member typedefs.
static_assert(std::is_same_v<std::extents<int, 1, 2>::rank_type, size_t>);

static_assert(std::is_unsigned_v<std::extents<int, 2>::size_type>);
static_assert(std::is_unsigned_v<std::extents<unsigned int, 2>::size_type>);

static_assert(std::is_same_v<std::extents<int, 2>::index_type, int>);
static_assert(std::is_same_v<std::extents<unsigned int, 2>::index_type,
	      unsigned int>);

// Check `rank`.
static_assert(std::extents<int, 1>::rank() == 1);
static_assert(std::extents<int, dyn>::rank() == 1);
static_assert(std::extents<int, 2, dyn>::rank() == 2);

// Check `rank_dynamic`.
static_assert(std::extents<int, 1>::rank_dynamic() == 0);
static_assert(std::extents<int, dyn>::rank_dynamic() == 1);
static_assert(std::extents<int, 2, dyn>::rank_dynamic() == 1);
static_assert(std::extents<int, dyn, dyn>::rank_dynamic() == 2);

template<typename T, size_t... Extents>
  constexpr bool
  check_rank_return_types()
  {
    auto e = std::extents<T, Extents...>();
    return std::is_same_v<decltype(e.rank()), size_t>
	   && std::is_same_v<decltype(e.rank_dynamic()), size_t>;
  }

static_assert(check_rank_return_types<int, 1>());

// Check that the static extents don't take up space.
static_assert(sizeof(std::extents<int, 1, dyn>) == sizeof(int));
static_assert(sizeof(std::extents<short, 1, dyn>) == sizeof(short));

template<typename Extents>
class Container
{
  int dummy;
  [[no_unique_address]] std::extents<size_t> b0;
};

static_assert(sizeof(Container<std::extents<short, 1, 2>>) == sizeof(int));
static_assert(sizeof(Container<std::extents<size_t, 1, 2>>) == sizeof(int));

// operator=
static_assert(std::is_nothrow_assignable_v<std::extents<int, dyn, 2>,
					   std::extents<int, 1, 2>>);

constexpr bool
test_assign()
{
  auto e1 = std::extents<int, 1, 2>();
  auto e2 = std::extents<int, 1, 2>();

  e2 = e1;
  VERIFY(e2 == e1);

  auto e5 = std::extents<int, 1, dyn>();
  e5 = e1;
  VERIFY(e5 == e1);

  auto e3 = std::extents<int, dyn, dyn>(1, 2);
  auto e4 = std::extents<int, dyn, dyn>(3, 4);
  e3 = e4;
  VERIFY(e3 == e4);
  return true;
}

// Deduction guide
template<size_t Rank, typename... Extents>
constexpr void
test_deduction(Extents... exts)
{
  std::array<size_t, sizeof...(exts)> shape{static_cast<size_t>(exts)...};
  std::dextents<size_t, Rank> expected(shape);
  std::extents e(exts...);
  static_assert(std::is_same_v<decltype(e), std::dextents<size_t, Rank>>);
  VERIFY(e == expected);
}

constexpr bool
test_deduction_all()
{
  test_deduction<0>();
  test_deduction<1>(1);
  test_deduction<2>(1.0, 2.0f);
  test_deduction<3>(int(1), short(2), size_t(3));
  return true;
}

class A {};

template<typename... Extents>
  concept deducible = requires
  {
    { std::extents(Extents{}...) }
      -> std::convertible_to<std::dextents<size_t, sizeof...(Extents)>>;
  };

static_assert(deducible<int>);
static_assert(!deducible<A, A>);

// dextents
static_assert(std::is_same_v<std::dextents<int, 0>, std::extents<int>>);
static_assert(std::is_same_v<std::dextents<int, 1>, std::extents<int, dyn>>);
static_assert(std::is_same_v<std::dextents<int, 5>,
	      std::extents<int, dyn, dyn, dyn, dyn, dyn>>);

static_assert(std::dextents<int, 5>::rank() == 5);
static_assert(std::dextents<int, 5>::rank_dynamic() == 5);
static_assert(std::is_same_v<typename std::dextents<int, 5>::index_type, int>);

// static_extent
static_assert(std::extents<int, 1, 2>::static_extent(0) == 1);
static_assert(std::extents<int, 1, 2>::static_extent(1) == 2);

static_assert(std::extents<int, 1, dyn>::static_extent(0) == 1);
static_assert(std::extents<int, 1, dyn>::static_extent(1) == dyn);

static_assert(std::extents<int, dyn, dyn>::static_extent(0) == dyn);
static_assert(std::extents<int, dyn, dyn>::static_extent(1) == dyn);

// extent
template<typename Extent>
  constexpr void
  test_extent(const Extent& e,
    const std::array<typename Extent::index_type, Extent::rank()>& shape)
  {
    for(size_t i = 0; i < e.rank(); ++i)
      VERIFY(e.extent(i) == shape[i]);
  }

constexpr bool
test_extent_all()
{
  test_extent(std::extents<int, 1, 2>{}, {1, 2});
  test_extent(std::extents<int, 1, dyn>{2}, {1, 2});
  test_extent(std::extents<int, dyn, dyn>{1, 2}, {1, 2});
  return true;
}

// operator==
template<typename Lhs, typename Rhs>
  constexpr void
  test_ops_eq(const Lhs& lhs, const Rhs& rhs, bool expected)
  {
    VERIFY((lhs == rhs) == expected);
    VERIFY((lhs != rhs) == !expected);
  }

constexpr void
test_op_eq_rank_zero()
{
  auto e1 = std::extents<int>();
  auto e2 = std::extents<int>();
  auto e3 = std::extents<unsigned int>();

  test_ops_eq(e1, e2, true);
  test_ops_eq(e1, e3, true);
}

constexpr void
test_op_eq_common()
{
  auto e1 = std::extents<int, 1, 2, 3>();
  auto e2 = std::extents<int, 1, 2, 3>();
  auto e3 = std::extents<int, 1, dyn, 3>(2);
  auto e4 = std::extents<int, 1, dyn, 3>(3);

  auto e5 = std::extents<int, 1>();
  auto e6 = std::extents<int, 1, 3, 3>();

  test_ops_eq(e1, e2, true);
  test_ops_eq(e1, e3, true);
  test_ops_eq(e1, e4, false);

  test_ops_eq(e1, e5, false);
  test_ops_eq(e1, e6, false);
  test_ops_eq(e3, e6, false);
}

constexpr bool
test_op_eq_all()
{
  test_op_eq_rank_zero();
  test_op_eq_common();
  return true;
}

int
main()
{
  test_assign();
  static_assert(test_assign());

  test_deduction_all();
  static_assert(test_deduction_all());

  test_extent_all();
  static_assert(test_extent_all());

  test_op_eq_all();
  static_assert(test_op_eq_all());
  return 0;
}
