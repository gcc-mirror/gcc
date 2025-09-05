// { dg-do compile { target c++17 } }
// libstdc++/121782
// Missing Mandates for operator() of std::boyer_moore_[horspool]_searcher

// N.B. we only enforce this for C++20 and later.
// { dg-error "static assertion failed" "" { target c++20 } 0 }

#include <algorithm>
#include <functional>
#include <testsuite_iterators.h>

template<typename T>
using Range = __gnu_test::random_access_container<T>;

void
test_bm(Range<char> needle, Range<unsigned char> haystack)
{
  std::boyer_moore_searcher s(needle.begin(), needle.end());
  (void) std::search(haystack.begin(), haystack.end(), s); // { dg-error "here" "" { target c++20 } }
  // { dg-error "'char' is not the same as 'unsigned char'" "" { target c++20 } 0 }
}

void
test_bmh(Range<char> needle, Range<signed char> haystack)
{
  std::boyer_moore_horspool_searcher s(needle.begin(), needle.end());
  (void) std::search(haystack.begin(), haystack.end(), s); // { dg-error "here" "" { target c++20 } }
  // { dg-error "'char' is not the same as 'signed char'" "" { target c++20 } 0 }
}

