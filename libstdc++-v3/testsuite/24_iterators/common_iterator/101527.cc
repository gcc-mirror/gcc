// { dg-do compile { target c++20 } }

// PR libstdc++/101527
// implementation of std::common_iterator and std::counted_iterator's
// operator== seems to be wrong

#include <iterator>

bool test_pr101527()
{
  std::common_iterator<int*, std::unreachable_sentinel_t> it1;
  std::common_iterator<const int*, std::unreachable_sentinel_t> it2;
  return it1 == it2;
}
