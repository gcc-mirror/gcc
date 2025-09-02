// { dg-do run { target c++26 } }

#include <inplace_vector>
#include <testsuite_hooks.h>
#include <span>

template<typename T, size_t N>
constexpr bool
eq(const std::inplace_vector<T, N>& l, std::span<const T> r) {
  if (l.size() != r.size())
    return false;
  for (auto i = 0u; i < l.size(); ++i)
    if (l[i] != r[i])
      return false;
  return true;
};

constexpr void
test_erase()
{
  std::inplace_vector<int, 15> c{1, 0, 3, 4, 5, 6, 5, 4, 3, 0, 1, 4, 4, 9};
  std::erase(c, 4);
  VERIFY( c.size() == 10 );
  std::erase(c, 1);
  VERIFY( c.size() == 8 );
  std::erase(c, 9);
  VERIFY( c.size() == 7 );
  VERIFY( eq(c, {0, 3, 5, 6, 5, 3, 0}) );
  
  std::erase(c, {});
  VERIFY( c.size() == 5 );
  VERIFY( eq(c, {3, 5, 6, 5, 3}) );

  std::erase(c, {5});
  VERIFY( c.size() == 3 );
  VERIFY( eq(c, {3, 6, 3}) );

  std::inplace_vector<int, 0> e;
  std::erase(e, 10);
  VERIFY( e.empty() );
}

constexpr void
test_erase_if()
{
  std::inplace_vector<int, 15> c{1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1, 4, 4, 9};
  std::erase_if(c, [](int i) { return i > 5; });
  VERIFY( c.size() == 12 );
  std::erase_if(c, [](int i) { return i == 4; });
  VERIFY( c.size() == 8 );
  std::erase_if(c, [](int i) { return i & 1; });
  VERIFY( eq(c, {2, 2}) );

  std::inplace_vector<int, 0> e;
  std::erase_if(e, [](int i) { return i > 5; });
  VERIFY( e.empty() );
}

int main()
{
  test_erase();
  test_erase_if();

  constexpr bool _ = [] {
    test_erase();
    test_erase_if();
    return true;
  }();
}
