// { dg-do compile { target c++26 } }

#include <utility>
#include <type_traits>

template<auto N>
void test()
{
  constexpr auto [id0, ...ids] = std::_IotaArray<N>;
  static_assert( std::is_same_v<decltype(id0), const decltype(N)> );
  static_assert( sizeof...(ids) == N - 1 );
  static_assert( (id0 + ... + ids) == N*(N-1)/2 );
}

int main()
{
  test<1>();
  test<4u>();
  test<8ull>();
}
