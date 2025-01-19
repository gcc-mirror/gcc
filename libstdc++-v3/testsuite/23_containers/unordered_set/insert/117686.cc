// { dg-do compile { target c++11 } }
// Bug 117686 - error in unordered_set::emplace

#include <unordered_set>
#include <utility>

struct H {
  std::size_t operator()(const std::pair<int, int>&) const { return 0; }
};

void
test_117686()
{
  std::unordered_set<std::pair<int, int>, H> s;
  s.emplace(1, 2);
}
