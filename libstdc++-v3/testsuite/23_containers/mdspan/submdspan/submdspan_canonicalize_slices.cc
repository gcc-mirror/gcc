// { dg-do run { target c++26 } }
#include <mdspan>

#include <testsuite_hooks.h>
#include <cstddef>
#include <cstdint>

constexpr size_t dyn = std::dynamic_extent;

template<typename Extents, typename CInt>
  constexpr bool
  check_collapsing(Extents exts, CInt ci_raw)
  {
    using IndexType = typename Extents::index_type;
    auto ci_expected = std::cw<IndexType{ci_raw.value}>;
    auto [ci] = std::submdspan_canonicalize_slices(exts, ci_raw);
    static_assert(std::same_as<decltype(ci), decltype(ci_expected)>);
    VERIFY(std::cmp_equal(ci.value, ci_raw.value));

    auto [i] = std::submdspan_canonicalize_slices(exts, ci_raw.value);
    static_assert(std::same_as<decltype(i), IndexType>);
    VERIFY(std::cmp_equal(i, ci_raw.value));
    return true;
  }

template<typename Extents>
  constexpr bool
  test_scalar(Extents exts)
  {
    using IndexType = typename Extents::index_type;

    check_collapsing(exts, std::cw<uint8_t{0}>);
    check_collapsing(exts, std::cw<IndexType{0}>);

    check_collapsing(exts, std::cw<uint8_t{4}>);
    check_collapsing(exts, std::cw<IndexType{4}>);
    return true;
  }

constexpr bool
test_scalar()
{
  test_scalar(std::extents<int, dyn>{5});
  test_scalar(std::extents<int, 5>{});
  test_scalar(std::extents<unsigned int, dyn>{5});
  test_scalar(std::extents<unsigned int, 5>{});
  return true;
}

constexpr void
assert_same(auto lhs, auto rhs)
{
  static_assert(std::same_as<decltype(lhs), decltype(rhs)>);
  VERIFY(lhs == rhs);
}

template<template<typename, typename> typename Pair>
  constexpr bool
  test_pair(auto exts, auto cbegin, auto cend, auto coffset, auto cextent)
  {
    using IndexType = typename decltype(exts)::index_type;
    auto c1 = std::cw<IndexType{1}>;

    auto raw_cc = Pair{cbegin, cend};
    auto [cc] = std::submdspan_canonicalize_slices(exts, raw_cc);
    assert_same(cc.offset, coffset);
    assert_same(cc.extent, cextent);
    assert_same(cc.stride, c1);

    auto raw_cd = Pair{cbegin, cend.value};
    auto [cd] = std::submdspan_canonicalize_slices(exts, raw_cd);
    assert_same(cd.offset, coffset);
    assert_same(cd.extent, cextent.value);
    assert_same(cd.stride, c1);

    auto raw_dc = Pair{cbegin.value, cend};
    auto [dc] = std::submdspan_canonicalize_slices(exts, raw_dc);
    assert_same(dc.offset, coffset.value);
    assert_same(dc.extent, cextent.value);
    assert_same(dc.stride, c1);

    auto raw_dd = Pair{cbegin.value, cend.value};
    auto [dd] = std::submdspan_canonicalize_slices(exts, raw_dd);
    assert_same(dd.offset, coffset.value);
    assert_same(dd.extent, cextent.value);
    assert_same(dd.stride, c1);
    return true;
  }

template<template<typename, typename> typename Pair>
  constexpr bool
  test_pair()
  {
    test_pair<Pair>(std::extents<int, dyn>{5}, std::cw<uint8_t{2}>,
	std::cw<uint8_t{5}>, std::cw<2>, std::cw<3>);
    test_pair<Pair>(std::extents<int, 5>{}, std::cw<uint8_t{2}>,
	std::cw<uint8_t{5}>, std::cw<2>, std::cw<3>);
    test_pair<Pair>(std::extents<int, 0>{}, std::cw<uint8_t{0}>,
	std::cw<uint8_t{0}>, std::cw<0>, std::cw<0>);
    test_pair<Pair>(std::extents<int, dyn>{0}, std::cw<uint8_t{0}>,
	std::cw<uint8_t{0}>, std::cw<0>, std::cw<0>);
    return true;
  }

template<typename Lower, typename Upper>
struct Range
{
  Lower lower;
  Upper upper;
};

constexpr bool
test_pair_all()
{
  test_pair<std::pair>();
  test_pair<std::tuple>();
  test_pair<Range>();
  return true;
}

constexpr bool
test_strided_slice(auto exts, auto co, auto ce, auto cs)
{
  using IndexType = decltype(exts)::index_type;

  auto coffset = std::cw<IndexType{co.value}>;
  auto cextent = std::cw<IndexType{ce.value}>;
  auto cstride = std::cw<IndexType{cs.value}>;

  auto raw_ccc = std::strided_slice{co, ce, cs};
  auto [ccc] = std::submdspan_canonicalize_slices(exts, raw_ccc);
  assert_same(ccc.offset, coffset);
  assert_same(ccc.extent, cextent);
  assert_same(ccc.stride, cstride);

  auto raw_dcc = std::strided_slice{co.value, ce, cs};
  auto [dcc] = std::submdspan_canonicalize_slices(exts, raw_dcc);
  assert_same(dcc.offset, coffset.value);
  assert_same(dcc.extent, cextent);
  assert_same(dcc.stride, cstride);

  auto raw_cdc = std::strided_slice{co, ce.value, cs};
  auto [cdc] = std::submdspan_canonicalize_slices(exts, raw_cdc);
  assert_same(cdc.offset, coffset);
  assert_same(cdc.extent, cextent.value);
  assert_same(cdc.stride, cstride);

  auto raw_ccd = std::strided_slice{co, ce, cs.value};
  auto [ccd] = std::submdspan_canonicalize_slices(exts, raw_ccd);
  assert_same(ccd.offset, coffset);
  assert_same(ccd.extent, cextent);
  assert_same(ccd.stride, cstride.value);
  return true;
}

constexpr bool
test_strided_slice()
{
  auto run = [](auto exts)
  {
    auto cs = std::cw<uint8_t{9}>;
    test_strided_slice(exts, std::cw<uint8_t{2}>, std::cw<uint8_t{3}>, cs);
    test_strided_slice(exts, std::cw<uint8_t{0}>, std::cw<uint8_t{5}>, cs);
  };

  run(std::extents<int, 5>{});
  run(std::extents<int, dyn>{5});
  return true;
}

constexpr bool
test_strided_slice_zero_extent(auto exts, auto cs)
{
  using IndexType = typename decltype(exts)::index_type;
  auto c0 = std::cw<uint8_t{0}>;
  auto raw_ccc = std::strided_slice{c0, c0, cs};
  auto [ccc] = std::submdspan_canonicalize_slices(exts, raw_ccc);
  assert_same(ccc.stride, std::cw<IndexType{1}>);

  auto raw_ccd = std::strided_slice{c0, c0, cs.value};
  auto [ccd] = std::submdspan_canonicalize_slices(exts, raw_ccd);
  assert_same(ccd.stride, std::cw<IndexType{1}>);
  return true;
}

constexpr bool
test_strided_slice_zero_extent(auto exts)
{
  test_strided_slice_zero_extent(exts, std::cw<uint8_t{0}>);
  test_strided_slice_zero_extent(exts, std::cw<uint8_t{9}>);
  return true;
}

constexpr bool
test_strided_slice_zero_extent()
{
  test_strided_slice_zero_extent(std::extents<int, 0>{});
  test_strided_slice_zero_extent(std::extents<int, dyn>{0});
  test_strided_slice_zero_extent(std::extents<int, 5>{});
  test_strided_slice_zero_extent(std::extents<int, dyn>{5});
  return true;
}

constexpr bool
test_all()
{
  test_scalar();
  test_pair_all();
  test_strided_slice();
  test_strided_slice_zero_extent();
  return true;
}

int
main()
{
  test_all();
  static_assert(test_all());
  return 0;
}
