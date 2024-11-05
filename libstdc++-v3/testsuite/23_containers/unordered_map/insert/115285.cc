// { dg-do run { target c++11 } }

// PR libstdc++/115285

#include <unordered_map>
#include <iterator>
#include <testsuite_hooks.h>

struct Pair
{
  explicit operator std::pair<const int, int>() const&& { return {1, 2}; }
};

void
test01()
{
  Pair p[2];
  auto mi = std::make_move_iterator(p);
  auto me = std::make_move_iterator(p+2);
  std::unordered_map<int, int> m(mi, me);
  VERIFY( m.size() == 1 );
}

struct K {
  explicit K(int) noexcept { }
  bool operator==(const K&) const { return true; }
};

template<> struct std::hash<K> {
  std::size_t operator()(const K&) const { return 0ul; }
};

void
test02()
{
  const std::pair<int, int> p[2]{{1,2}, {3,4}};
  auto mi = std::make_move_iterator(p);
  auto me = std::make_move_iterator(p+2);
  std::unordered_map<K, int> m(mi, me);
  VERIFY( m.size() == 1 );
}

int main()
{
  test01();
  test02();
}
