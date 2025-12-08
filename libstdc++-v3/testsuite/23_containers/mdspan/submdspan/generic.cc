// { dg-do compile { target c++26 } }
#include <mdspan>

namespace adl
{
  struct NoFriend
  {
    template<typename Extents>
      class mapping
      {
      public:
	using extents_type = Extents;
	using index_type = typename extents_type::index_type;
      };
  };

  struct NoFull
  {
    template<typename Extents>
      class mapping
      {
      public:
	using extents_type = Extents;
	using index_type = typename extents_type::index_type;

      private:
	friend constexpr auto
	submdspan_mapping(mapping, int)
	{ return std::submdspan_mapping_result{mapping{}, 0}; }
      };
  };

  struct WrongReturnValue
  {
    template<typename Extents>
      class mapping
      {
      public:
	using extents_type = Extents;
	using index_type = typename extents_type::index_type;

      private:
	friend constexpr int
	submdspan_mapping(mapping, std::full_extent_t)
	{ return 42; }
      };
  };
}

template<typename MdSpan, typename... Slices>
  concept submdspan_exists = requires (MdSpan md, Slices... slices)
  {
    std::submdspan(md, slices...);
  };

template<typename Layout, bool Expected>
constexpr bool
test_invalid_mapping()
{
  using Extents = std::extents<int, 3>;
  using MdSpan = std::mdspan<double, Extents, Layout>;
  static_assert(submdspan_exists<MdSpan, int> == Expected);
  static_assert(submdspan_exists<MdSpan, std::full_extent_t> == Expected);
  static_assert(!submdspan_exists<MdSpan>);
  static_assert(!submdspan_exists<MdSpan, int, int>);
  return true;
}
static_assert(test_invalid_mapping<std::layout_left, true>());
static_assert(test_invalid_mapping<adl::NoFriend, false>());
static_assert(test_invalid_mapping<adl::NoFull, false>());
static_assert(test_invalid_mapping<adl::WrongReturnValue, false>());
