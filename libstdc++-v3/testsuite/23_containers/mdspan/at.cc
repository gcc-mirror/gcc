// { dg-do run { target c++26 } }
#include <mdspan>

#include <testsuite_hooks.h>
#include "int_like.h"
#include <stdexcept>

template<typename MDSpan, typename... Args>
concept valid_at = requires (MDSpan md, Args... args)
{
  { md.at(args...) } -> std::same_as<typename MDSpan::reference>;
};

template<typename Int, bool ValidForPacks, bool ValidForArrays>
  constexpr bool
  test_at()
  {
    using Extents = std::extents<int, 3, 5, 7>;
    auto exts = Extents{};

    auto mapping = std::layout_left::mapping(exts);
    constexpr size_t n = mapping.required_span_size();
    std::array<double, n> storage{};

    auto md = std::mdspan(storage.data(), mapping);
    using MDSpan = decltype(md);

    for(int i = 0; i < exts.extent(0); ++i)
      for(int j = 0; j < exts.extent(1); ++j)
	for(int k = 0; k < exts.extent(2); ++k)
	  {
	    storage[mapping(i, j, k)] = 1.0;
	    if constexpr (ValidForPacks)
	      VERIFY(md.at(Int(i), Int(j), Int(k)) == 1.0);

	    if constexpr (ValidForArrays)
	      {
		std::array<Int, 3> ijk{Int(i), Int(j), Int(k)};
		VERIFY(md.at(ijk) == 1.0);
		VERIFY(md.at(std::span(ijk)) == 1.0);
	      }
	    storage[mapping(i, j, k)] = 0.0;
	  }

    if constexpr (!ValidForPacks)
      static_assert(!valid_at<MDSpan, Int, int, Int>);

    if constexpr (!ValidForArrays)
      {
	static_assert(!valid_at<MDSpan, std::array<Int, 3>>);
	static_assert(!valid_at<MDSpan, std::span<Int, 3>>);
      }

    auto verify_throw = [&md](int i, int j, int k)
    {
      if constexpr (ValidForPacks)
	try
	{
	  md.at(Int(i), Int(j), Int(k));
	  VERIFY(false);
	}
	catch (std::out_of_range&)
	{
	  VERIFY(true);
	}

      if constexpr (ValidForArrays)
      {
	std::array<Int, 3> ijk{Int(i), Int(j), Int(k)};
	try
	{
	  md.at(ijk);
	  VERIFY(false);
	}
	catch (std::out_of_range&)
	{
	  VERIFY(true);
	}

	try
	{
	  md.at(std::span(ijk));
	  VERIFY(false);
	}
	catch (std::out_of_range&)
	{
	  VERIFY(true);
	}
      }
    };

#if !__cpp_lib_constexpr_exceptions
    if consteval {
      return true;
    }
#endif

    verify_throw(-1, 0, 0);
    verify_throw(0, -3, 0);
    verify_throw(0, 0, -5);

    verify_throw(11, 0, 0);
    verify_throw(0, 13, 0);
    verify_throw(0, 0, 15);

    return true;
  }

int
main()
{
  test_at<int, true, true>();
  static_assert(test_at<int, true, true>());
  test_at<short, true, true>();
  test_at<IntLike, true, true>();
  test_at<ThrowingInt, false, false>();
  test_at<MutatingInt, true, false>();
  test_at<RValueInt, true, false>();
}
