// { dg-do run { target c++23 } }
#include <mdspan>

#include <cstdint>
#include <testsuite_hooks.h>

constexpr auto dyn = std::dynamic_extent;

template<typename Extents>
  constexpr void
  test_default_ctor()
  {
    Extents exts;
    for(size_t i = 0; i < Extents::rank(); ++i)
      if(exts.static_extent(i) == std::dynamic_extent)
	VERIFY(exts.extent(i) == 0);
      else
	VERIFY(exts.extent(i) == Extents::static_extent(i));
  }

constexpr bool
test_default_ctor_all()
{
  test_default_ctor<std::extents<int, 1>>();
  test_default_ctor<std::extents<int, dyn>>();
  test_default_ctor<std::extents<int, 1, 2>>();
  test_default_ctor<std::extents<int, dyn, 2>>();
  test_default_ctor<std::extents<int, dyn, dyn>>();
  test_default_ctor<std::extents<int, 1, 2, 3>>();
  test_default_ctor<std::extents<int, dyn, 2, dyn>>();
  test_default_ctor<std::extents<int, dyn, dyn, dyn>>();
  return true;
}

int
main()
{
  test_default_ctor_all();
  static_assert(test_default_ctor_all());
  return 0;
}
