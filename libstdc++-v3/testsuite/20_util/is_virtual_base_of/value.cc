// { dg-do compile { target c++26 } }

#include <type_traits>

#if !defined(__cpp_lib_is_virtual_base_of) || __cpp_lib_is_virtual_base_of < 202406L
#error "__cpp_lib_is_virtual_base_of should have been defined to 202406L or more"
#endif

class B { };
class X : virtual public B { };
class Y : virtual public B { };
class Z : public B { };
class AA : public X, public Y, public Z { };

template<typename Base, typename Derived>
constexpr bool test()
{
  constexpr bool t1 = std::is_virtual_base_of<Base, Derived>::value;
  constexpr bool t2 = std::is_virtual_base_of_v<Base, Derived>;
  static_assert(t1 == t2);
  return t1;
}

void test01()
{
  static_assert(!test<B, B>());
  static_assert( test<B, X>());
  static_assert( test<B, Y>());
  static_assert(!test<B, Z>());
  static_assert( test<B, AA>());
  static_assert(!test<X, AA>());
  static_assert(!test<Y, AA>());
  static_assert(!test<Z, AA>());
}
