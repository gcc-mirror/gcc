// PR libstdc++ - ranges::sort doesn't use iter_move, cannot sort zip of move-only type
// { dg-do compile { target c++23 } }

#include <algorithm>
#include <ranges>
#include <vector>

struct F {
  int a = -1;
  explicit F(int d) : a(d) { }
  F(const F&) = delete;
  F(F&& o) : a(o.a) { }
  void operator=(const F&) = delete;
  F& operator=(F&& o) { return *this; }
  auto operator<=>(const F&) const = default;
};

int main () {
  int a[] = {3,2,1};
  std::vector<F> v(a, a+3);
  std::ranges::sort(v); // OK
  std::ranges::sort(std::views::zip(v)); // didn't compile
}
