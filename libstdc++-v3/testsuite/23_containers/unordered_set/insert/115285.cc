// { dg-do run { target c++11 } }

// PR libstdc++/115285

#include <unordered_set>
#include <testsuite_hooks.h>

struct K {
  explicit K(int) noexcept { }
  bool operator==(const K&) const { return true; }
};

template<> struct std::hash<K> {
  std::size_t operator()(const K&) const { return 0ul; }
};

void
test01()
{
  int i[2]{1, 2};
  std::unordered_set<K> s(i, i+2);
  VERIFY( s.size() == 1 );
}

int main()
{
  test01();
}
