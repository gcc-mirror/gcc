// { dg-do compile { target c++17 } }

#if __cplusplus <= 202302L
# include <variant>
#else
# include <utility>
#endif

void
test01()
{
  static_assert( std::is_nothrow_default_constructible_v<std::monostate> );
  static_assert( std::is_nothrow_copy_constructible_v<std::monostate> );
  static_assert( std::is_nothrow_copy_assignable_v<std::monostate> );
  static_assert( std::is_nothrow_destructible_v<std::monostate> );
}

void
test02()
{
#ifdef __cpp_lib_three_way_comparison
  static_assert( std::is_eq(std::monostate{} <=> std::monostate{}) );
#endif
  static_assert( std::monostate{} == std::monostate{} );
  static_assert( std::monostate{} <= std::monostate{} );
  static_assert( std::monostate{} >= std::monostate{} );
  static_assert( !(std::monostate{} != std::monostate{}) );
  static_assert( !(std::monostate{} < std::monostate{}) );
  static_assert( !(std::monostate{} > std::monostate{}) );
}

void
test03()
{
  std::monostate m;
  std::hash<std::monostate> h;
  static_assert( std::is_same_v<decltype(h(m)), std::size_t> );
}
