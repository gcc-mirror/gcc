// { dg-do run { target c++26 } }

#include <ranges>
#include <vector>
#include <algorithm>
#include <testsuite_hooks.h>

namespace ranges = std::ranges;

void
test01()
{
  // 4 discrete vectors serving as segments
  std::vector<int> v0 = {0, 1, 2};    // Segment 0
  std::vector<int> v1 = {3, 4, 5};    // Segment 1
  std::vector<int> v2 = {6, 7, 8};    // Segment 2
  std::vector<int> v3 = {9, 10, 11};  // Segment 3

  auto cv = std::views::concat(v0, v1, v2, v3);
  std::__segmented_iterator auto begin = cv.begin();
  std::__segmented_iterator auto end = cv.end();

  static int call_count = 0;
  auto make_finder = [](int target) {
    return [target](auto first, auto last) {
      if (first != last)
	++call_count;
      return std::find(first, last, target);
    };
  };

  call_count = 0;
  auto it = std::__for_each_segment(begin + 3, begin + 6, make_finder(4));
  VERIFY( it == begin + 4 );
  VERIFY( call_count == 1 );

  call_count = 0;
  it = std::__for_each_segment(begin + 3, begin + 6, make_finder(99));
  VERIFY( it == begin + 6 );
  VERIFY( call_count == 1 );

  call_count = 0;
  it = std::__for_each_segment(begin + 1, begin + 11, make_finder(2));
  VERIFY( it == begin + 2 );
  VERIFY( call_count == 1 );

  call_count = 0;
  it = std::__for_each_segment(begin + 1, begin + 11, make_finder(7));
  VERIFY( it == begin + 7 );
  VERIFY( call_count == 3 );

  call_count = 0;
  it = std::__for_each_segment(begin + 1, begin + 11, make_finder(9));
  VERIFY( it == begin + 9 );
  VERIFY( call_count == 4 );

  call_count = 0;
  it = std::__for_each_segment(begin + 1, end, make_finder(99));
  VERIFY( it == end );
  VERIFY( call_count == 4 );
}

int
main()
{
  test01();
}
