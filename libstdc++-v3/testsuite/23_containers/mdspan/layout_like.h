#ifndef TEST_MDSPAN_LAYOUT_LIKE_H
#define TEST_MDSPAN_LAYOUT_LIKE_H 1

struct LayoutLike
{
  template<typename Extents>
    class mapping
    {
    public:
      using extents_type = Extents;
      using index_type = typename extents_type::index_type;
      using size_type = typename extents_type::size_type;
      using rank_type = typename extents_type::rank_type;
      using layout_type = LayoutLike;

      constexpr
      mapping() noexcept = default;

      constexpr
      mapping(Extents exts)
      : m_exts(exts)
      { }

      constexpr const extents_type&
      extents() const noexcept { return m_exts; }

      constexpr index_type
      required_span_size() const noexcept
      {
	for (size_t i = 0; i < extents_type::rank(); ++i)
	  if (m_exts.extent(i) == 0)
	    return 0;
	return 1;
      }

      template<typename... Indices>
	requires (sizeof...(Indices) == extents_type::rank())
	constexpr index_type
	operator()(Indices...) const noexcept
	{ return 0; }

      static constexpr index_type
      stride(rank_type) noexcept
      { return 0; }

      static constexpr bool
      is_always_unique() noexcept
      { return false; }

      static constexpr bool
      is_always_exhaustive() noexcept
      { return true; }

      static constexpr bool
      is_always_strided() noexcept
      { return true; }

      constexpr bool
      is_unique() noexcept
      {
	if (required_span_size() == 0)
	  return true;

	for (size_t i = 0; i < extents_type::rank(); ++i)
	  if (m_exts.extent(i) > 1)
	    return false;
	return true;
      }

      static constexpr bool
      is_exhaustive() noexcept
      { return true; }

      static constexpr bool
      is_strided() noexcept
      { return true; }

    private:
      Extents m_exts;
    };
};

#endif
