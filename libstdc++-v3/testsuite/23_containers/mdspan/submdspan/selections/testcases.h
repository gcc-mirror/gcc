#include <mdspan>

#include <vector>
#include <numeric>
#include "../../layout_traits.h"
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;
constexpr auto all = std::full_extent;

template<typename T>
  constexpr bool is_strided_slice = false;

template<typename O, typename E, typename S>
  constexpr bool is_strided_slice<std::strided_slice<O, E, S>> = true;

template<typename MDSpan>
  constexpr void
  fill(const MDSpan& md)
  {
    using IndexType = typename MDSpan::index_type;
    auto exts = md.extents();
    if constexpr (exts.rank() == 3)
      for(IndexType i = 0; i < exts.extent(0); ++i)
	for(IndexType j = 0; j < exts.extent(1); ++j)
	  for(IndexType k = 0; k < exts.extent(2); ++k)
	    md[i, j, k] = 100 * i + 10 * j + k;
  }

template<typename Int, size_t Rank>
  class multi_index_generator
  {
    struct sentinel
    { };

    class iterator
    {
    public:
      constexpr
      iterator(const std::array<Int, Rank>& shape)
	: M_shape(shape)
      { }

      constexpr iterator&
      operator++()
      {
	if constexpr (Rank > 0)
	  {
	    ++M_indices[Rank-1];
	    for(size_t i = Rank; i > 1; --i)
	      if (M_indices[i-1] == M_shape[i-1])
		{
		  M_indices[i-1] = 0;
		  ++M_indices[i-2];
		}
	  }
	return *this;
      }

      constexpr auto
      operator*()
      { return M_indices; }

    private:
      friend constexpr bool
      operator==(const iterator& it, sentinel)
      {
	if constexpr (Rank > 0)
	  return it.M_indices[0] == it.M_shape[0];
	else
	  return true;
      }

      std::array<Int, Rank> M_indices{};
      std::array<Int, Rank> M_shape;
    };

  public:
    constexpr
    multi_index_generator(std::array<Int, Rank> shape)
      : M_shape(shape)
    { }

    constexpr iterator
    begin() const
    { return iterator(M_shape); }

    constexpr sentinel
    end() const
    { return sentinel{}; }

  private:
    std::array<Int, Rank> M_shape;
  };

constexpr bool
test_multi_index()
{
  auto shape = std::array{3, 5, 7, 1};
  auto gen = multi_index_generator(shape);
  auto it = gen.begin();
  auto end = gen.end();
  for (int i = 0; i < shape[0]; ++i)
    for (int j = 0; j < shape[1]; ++j)
      for (int k = 0; k < shape[2]; ++k)
	for (int l = 0; l < shape[3]; ++l)
	  {
	    VERIFY(it != end);
	    VERIFY(*it == std::array{i, j, k, l});
	    ++it;
	  }
  return true;
}
static_assert(test_multi_index());

struct
collapse
{ };

template<typename... Slices>
  consteval auto
  inv_collapsed_index_map()
  {
    constexpr size_t rank = sizeof...(Slices);
    auto is_collapsing = std::array{std::same_as<Slices, collapse>...};
    constexpr auto collapsed_rank = ((!std::same_as<Slices, collapse>) + ... + 0);

    std::array<size_t, collapsed_rank> ret;
    if constexpr (collapsed_rank > 0)
      for(size_t k = 0, i = 0; i < rank; ++i)
	if (!is_collapsing[i])
	  ret[k++] = i;
    return ret;
  }

static_assert(inv_collapsed_index_map<collapse, collapse, collapse>()
	      == std::array<size_t, 0>{});

static_assert(inv_collapsed_index_map<collapse, decltype(all), collapse>()
	      == std::array<size_t, 1>{1});

template<typename IndexType, typename Slice>
  constexpr std::vector<IndexType>
  make_selection(IndexType extent, const Slice& slice)
  {
    if constexpr (std::convertible_to<Slice, IndexType>)
      return {static_cast<IndexType>(slice)};
    else if constexpr (std::same_as<Slice, std::full_extent_t>)
      {
	auto ret = std::vector<IndexType>(static_cast<size_t>(extent));
	std::ranges::iota(ret, 0);
	return ret;
      }
    else if constexpr (is_strided_slice<Slice>)
      {
	auto ret = std::vector<IndexType>{};
	size_t n = static_cast<size_t>(slice.extent);
	for(size_t i = 0; i < n; i += slice.stride)
	  ret.push_back(slice.offset + i);
	return ret;
      }
    else
      {
	auto [begin, end] = slice;
	auto ret = std::vector<IndexType>(static_cast<size_t>(end - begin));
	std::ranges::iota(ret, begin);
	return ret;
      }
  }

template<typename Layout, size_t... I, typename... Slices>
  constexpr bool
  check_selection(std::index_sequence<I...>, auto md, Slices... slices)
  {
    auto exts = md.extents();
    auto outer_shape = std::array{exts.extent(0), exts.extent(1), exts.extent(2)};

    constexpr auto full_index = inv_collapsed_index_map<Slices...>();
    auto make_slice = [](size_t i, auto slice)
    {
      if constexpr (std::same_as<decltype(slice), collapse>)
	return i;
      else
	return slice;
    };

    auto loop_body = [&]<size_t... J>(std::index_sequence<J...>, auto ijk,
				      auto... slices)
    {
      auto submd = submdspan(md, slices...[I]...);
      auto selection = std::tuple{make_selection(exts.extent(I), slices...[I])...};
      auto inner_shape = std::array<size_t, full_index.size()>{
	std::get<full_index[J]>(selection).size()...
      };

      for (auto ij : multi_index_generator(inner_shape))
      {
	((ijk[full_index[J]] = get<full_index[J]>(selection)[ij[J]]),...);
	VERIFY(submd[ij] == md[ijk]);
      }
    };

    for (auto ijk : multi_index_generator(outer_shape))
      loop_body(std::make_index_sequence<full_index.size()>(), ijk,
		make_slice(ijk[I], slices...[I])...);
    return true;
  }

template<typename Layout, typename...MD, typename... Slices>
  constexpr bool
  check_selection(std::mdspan<MD...> md, Slices... slices)
  {
    auto indices = std::make_index_sequence<sizeof...(slices)>();
    return check_selection<Layout>(indices, md, slices...);
  }

template<typename Layout, typename IndexType, size_t... Extents,
	 typename... Slices>
  constexpr bool
  check_selection(std::extents<IndexType, Extents...>exts, Slices... slices)
  {
    auto run = [&](auto m)
    {
      auto storage = std::vector<double>(m.required_span_size());
      auto md = std::mdspan(storage.data(), m);
      fill(md);
      return check_selection<Layout>(md, slices...);
    };

    if constexpr (std::same_as<Layout, std::layout_stride>)
      {
	auto m = typename Layout::mapping(exts, std::array{15, 2, 50});
	return run(m);
      }
    else
      {
	auto m = typename Layout::mapping(exts);
	return run(m);
      }
  }

template<typename Layout>
  constexpr bool
  test_scalar_selection(auto exts)
  {
    check_selection<Layout>(exts, collapse{}, collapse{}, collapse{});
    return true;
  }

template<typename Layout>
  constexpr bool
  test_full_lines(auto exts)
  {
    check_selection<Layout>(exts, all, collapse{}, collapse{});
    check_selection<Layout>(exts, collapse{}, all, collapse{});
    check_selection<Layout>(exts, collapse{}, collapse{}, all);
    return true;
  }

template<typename Layout>
  constexpr bool
  test_full_blocks(auto exts)
  {
    check_selection<Layout>(exts, all, all, collapse{});
    check_selection<Layout>(exts, all, collapse{}, all);
    check_selection<Layout>(exts, collapse{}, all, all);
    return true;
  }

template<typename Layout>
  constexpr bool
  test_cubes(auto exts)
  {
    auto s0 = std::pair{0, 2};
    auto s1 = std::pair{1, 4};
    auto s2 = std::pair{3, 7};

    check_selection<Layout>(exts, all, all, all);
    check_selection<Layout>(exts, all, all, s2);
    check_selection<Layout>(exts, s0, all, all);
    check_selection<Layout>(exts, s0, all, s2);
    check_selection<Layout>(exts, s0, s1, s2);
    return true;
  }

template<typename Layout>
  constexpr bool
  test_strided_line_selection(auto exts)
  {
    auto check = [&](auto s)
    {
      check_selection<Layout>(exts, collapse{}, s, collapse{});
    };

    check(std::strided_slice(0, 2, 2));
    check(std::strided_slice(0, 3, 2));
    check(std::strided_slice(1, 3, 2));
    check(std::strided_slice(1, std::cw<3>, std::cw<2>));
    return true;
  }

template<typename Layout>
  constexpr bool
  test_strided_box_selection(auto exts)
  {
    auto s0 = std::strided_slice(0, 3, 2);
    auto s1 = std::strided_slice(1, 4, 2);
    auto s2 = std::strided_slice(0, 7, 3);

    check_selection<Layout>(exts, s0, s1, s2);
    return true;
  }

template<typename Layout>
  constexpr bool
  test_all_cheap()
  {
    constexpr auto dyn_exts = std::extents(3, 5, 7);
    constexpr auto sta_exts = std::extents<int, 3, 5, 7>{};

    test_scalar_selection<Layout>(dyn_exts);
    test_scalar_selection<Layout>(sta_exts);
    static_assert(test_scalar_selection<Layout>(dyn_exts));
    static_assert(test_scalar_selection<Layout>(sta_exts));

    test_full_lines<Layout>(dyn_exts);
    test_full_lines<Layout>(sta_exts);
    static_assert(test_full_lines<Layout>(dyn_exts));
    static_assert(test_full_lines<Layout>(sta_exts));

    test_strided_box_selection<Layout>(dyn_exts);
    test_strided_box_selection<Layout>(sta_exts);
    static_assert(test_strided_box_selection<Layout>(dyn_exts));
    static_assert(test_strided_box_selection<Layout>(sta_exts));
    return true;
  }

template<typename Layout>
  constexpr bool
  test_all_expensive()
  {
    auto run = [](auto exts)
    {
      test_full_blocks<Layout>(exts);
      test_cubes<Layout>(exts);
    };

    run(std::extents(3, 5, 7));
    run(std::extents<int, 3, 5, 7>{});
    return true;
  }

template<typename Layout>
  constexpr bool
  test_all()
  {
    test_all_cheap<Layout>();
    test_all_expensive<Layout>();
    return true;
  }
