#ifndef TEST_MDSPAN_LAYOUT_LIKE_H
#define TEST_MDSPAN_LAYOUT_LIKE_H 1

template<bool Noexcept>
  struct CustomLayout
  {
    template<typename Extents>
      class mapping
      {
      public:
	using extents_type = Extents;
	using index_type = typename extents_type::index_type;
	using size_type = typename extents_type::size_type;
	using rank_type = typename extents_type::rank_type;
	using layout_type = CustomLayout;

	constexpr
	mapping() noexcept = default;

	constexpr
	mapping(Extents exts)
	: m_exts(exts)
	{ }

	constexpr const extents_type&
	extents() const noexcept(Noexcept) { return m_exts; }

	constexpr index_type
	required_span_size() const noexcept(Noexcept)
	{
	  for (size_t i = 0; i < extents_type::rank(); ++i)
	    if (m_exts.extent(i) == 0)
	      return 0;
	  return 1;
	}

	template<typename... Indices>
	  requires (sizeof...(Indices) == extents_type::rank())
	  constexpr index_type
	  operator()(Indices...) const noexcept(Noexcept)
	  { return 0; }

	static constexpr index_type
	stride(rank_type) noexcept(Noexcept)
	{ return 0; }

	static constexpr bool
	is_always_unique() noexcept(Noexcept)
	{ return false; }

	static constexpr bool
	is_always_exhaustive() noexcept(Noexcept)
	{ return true; }

	static constexpr bool
	is_always_strided() noexcept(Noexcept)
	{ return true; }

	constexpr bool
	is_unique() const noexcept(Noexcept)
	{
	  if (required_span_size() == 0)
	    return true;

	  for (size_t i = 0; i < extents_type::rank(); ++i)
	    if (m_exts.extent(i) > 1)
	      return false;
	  return true;
	}

	static constexpr bool
	is_exhaustive() noexcept(Noexcept)
	{ return true; }

	static constexpr bool
	is_strided() noexcept(Noexcept)
	{ return true; }

      private:
	Extents m_exts;
      };
  };

using LayoutLike = CustomLayout<true>;
using ThrowingLayout = CustomLayout<false>;

#endif
