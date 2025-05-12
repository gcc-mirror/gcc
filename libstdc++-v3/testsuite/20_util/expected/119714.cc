// { dg-do compile { target c++23 } }

// PR libstdc++/119714 - constraint recursion with std::expected::operator==

#include <expected>
#include <vector>

using I = std::vector<std::expected<int,int>>::iterator;
static_assert(std::totally_ordered<I>);
