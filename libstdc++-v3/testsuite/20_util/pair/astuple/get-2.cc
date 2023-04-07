// { dg-do compile { target c++11 } }

#include <utility>

template<int N, typename T, typename Pair>
constexpr bool
check()
{
  return std::is_same<decltype(std::get<N>(std::declval<Pair>())), T>::value;
}

void
test_value_category()
{
  using P = std::pair<int, long>;
  static_assert( check<0, int&, P&>(),
		 "get<0>(pair<T1, T2>&)" );
  static_assert( check<1, long&, P&>(),
		 "get<1>(pair<T1, T2>&)" );
  static_assert( check<0, int&&, P&&>(),
		 "get<0>(pair<T1, T2>&&)" );
  static_assert( check<1, long&&, P&&>(),
		 "get<1>(pair<T1, T2>&&)" );
  static_assert( check<0, const int&, const P&>(),
		 "get<0>(const pair<T1, T2>&)" );
  static_assert( check<1, const long&, const P&>(),
		 "get<1>(const pair<T1, T2>&)" );
  static_assert( check<0, const int&&, const P&&>(),
		 "get<0>(const pair<T1, T2>&&)" );
  static_assert( check<1, const long&&, const P&&>(),
		 "get<1>(const pair<T1, T2>&&)" );

  using PL = std::pair<int&, long&>;
  static_assert( check<0, int&, PL&>(),
		 "get<0>(pair<T1&, T2&>&)" );
  static_assert( check<1, long&, PL&>(),
		 "get<1>(pair<T1&, T2&>&)" );
  static_assert( check<0, int&, PL&&>(),
		 "get<0>(pair<T1&, T2&>&&)" );
  static_assert( check<1, long&, PL&&>(),
		 "get<1>(pair<T1&, T2&>&&)" );
  static_assert( check<0, int&, const PL&>(),
		 "get<0>(const pair<T1&, T2&>&)" );
  static_assert( check<1, long&, const PL&>(),
		 "get<1>(const pair<T1&, T2&>&)" );
  static_assert( check<0, int&, const PL&&>(),
		 "get<0>(const pair<T1&, T2&>&&)" );
  static_assert( check<1, long&, const PL&&>(),
		 "get<1>(const pair<T1&, T2&>&&)" );

  using PR = std::pair<int&&, long&&>;
  static_assert( check<0, int&, P&>(),
		 "get<0>(pair<T1&&, T2&&>&)" );
  static_assert( check<1, long&, P&>(),
		 "get<1>(pair<T1&&, T2&&>&)" );
  static_assert( check<0, int&&, PR&&>(),
		 "get<0>(pair<T1&&, T2&&>&&)" );
  static_assert( check<1, long&&, PR&&>(),
		 "get<1>(pair<T1&&, T2&&>&&)" );
  static_assert( check<0, int&, const PR&>(),
		 "get<0>(const pair<T1&&, T2&&>&)" );
  static_assert( check<1, long&, const PR&>(),
		 "get<1>(const pair<T1&&, T2&&>&)" );
  static_assert( check<0, int&&, const PR&&>(),
		 "get<0>(const pair<T1&&, T2&&>&&)" );
  static_assert( check<1, long&&, const PR&&>(),
		 "get<1>(const pair<T1&&, T2&&>&&)" );
}
