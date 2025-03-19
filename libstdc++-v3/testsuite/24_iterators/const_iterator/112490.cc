// { dg-do compile { target c++23 } }

// PR libstdc++/112490 - infinite meta error in
// reverse_iterator<basic_const_iterator<vector<int>::iterator>>

#include <iterator>
#include <vector>

using I = std::vector<int>::iterator;
using CI = std::basic_const_iterator<I>;
using RCI = std::reverse_iterator<CI>;
static_assert(std::totally_ordered<RCI>);
