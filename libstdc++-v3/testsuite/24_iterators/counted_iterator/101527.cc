// { dg-do compile { target c++20 } }

// PR libstdc++/101527
// implementation of std::common_iterator and std::counted_iterator's
// operator== seems to be wrong

#include <iterator>

bool test_pr101527()
{
  std::counted_iterator<int*> it1;
  std::counted_iterator<const int*> it2;
  return it1 == it2;
}
