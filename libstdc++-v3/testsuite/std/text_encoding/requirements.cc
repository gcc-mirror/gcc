// { dg-do compile { target c++26 } }
// { dg-require-cpp-feature-test __cpp_lib_text_encoding }
// { dg-add-options no_pch }

#include <text_encoding>
#ifndef __cpp_lib_text_encoding
# error "Feature-test macro for text_encoding missing in <text_encoding>"
#elif __cpp_lib_text_encoding != 202306L
# error "Feature-test macro for text_encoding has wrong value in <text_encoding>"
#endif

#undef __cpp_lib_text_encoding
#include <version>
#ifndef __cpp_lib_text_encoding
# error "Feature-test macro for text_encoding missing in <version>"
#elif __cpp_lib_text_encoding != 202306L
# error "Feature-test macro for text_encoding has wrong value in <version>"
#endif

#include <concepts>
#include <ranges>
static_assert( std::is_trivially_copyable_v<std::text_encoding> );

using aliases_view = std::text_encoding::aliases_view;
static_assert( std::copyable<aliases_view> );
static_assert( std::ranges::view<aliases_view> );
static_assert( std::ranges::random_access_range<aliases_view> );
static_assert( std::ranges::borrowed_range<aliases_view> );
static_assert( std::same_as<std::ranges::range_value_t<aliases_view>,
			    const char*> );
static_assert( std::same_as<std::ranges::range_reference_t<aliases_view>,
			    const char*> );

#include <testsuite_hooks.h>

constexpr bool
test_constexpr_iterator()
{
  // This encoding has two aliases, "UTF-8" and "csUTF8".
  const auto a = std::text_encoding(std::text_encoding::UTF8).aliases();
  const auto begin = a.begin();
  const auto end = a.end();
  auto iter = begin;
  VERIFY( *iter == *begin );
  VERIFY( *iter == iter[0] );
  VERIFY( iter[1] == begin[1] );
  --++iter;
  VERIFY( iter == begin );
  auto iter2 = iter++;
  VERIFY( iter2 == begin );
  VERIFY( iter != begin );
  iter2 = iter--;
  VERIFY( iter2 != begin );
  VERIFY( iter == begin );
  iter++++; // Increments prvalue returned by operator++(int) instead of iter.
  VERIFY( iter == iter2 );
  iter----; // Decrements prvalue returned by operator++(int) instead of iter.
  VERIFY( iter == begin );
  const auto d = std::ranges::distance(a);
  iter += d;
  VERIFY( iter == end );
  VERIFY( (iter - begin) == d );
  VERIFY( (begin + 2) == iter );
  VERIFY( (2 + begin) == iter );
  VERIFY( iter[-1] == begin[1] );
  iter -= d;
  VERIFY( iter == begin );
  VERIFY( *(iter + 1) == iter[1] );
  VERIFY( (1 + iter - 1) == begin );
  VERIFY( (-1 + (iter - -2) + -1) == begin );

  std::ranges::iterator_t<std::text_encoding::aliases_view> singular{};
  VERIFY( (singular + 0) == singular );
  VERIFY( (singular - 0) == singular );

  return true;
}
static_assert( test_constexpr_iterator() );
