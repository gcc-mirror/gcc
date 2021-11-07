// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <algorithm>
#include <span>

constexpr bool
test01()
{
  // PR libstdc++/102358
  int x[2] = {1,2}, y[2];
  std::span in(x), out(y);
  std::move(std::move_iterator(in.begin()), std::move_iterator(in.end()),
	    out.begin());
  return std::equal(std::move_iterator(in.begin()), std::move_iterator(in.end()),
		    std::move_iterator(out.begin()));
}

static_assert(test01());

constexpr bool
test02()
{
  int x[2] = {1,2}, y[2];
  std::span in(x), out(y);
  std::move(in.rbegin(), in.rend(), out.rbegin());
  return std::equal(in.rbegin(), in.rend(), out.rbegin());
}

static_assert(test02());
