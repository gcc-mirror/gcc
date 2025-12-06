// PR libstdc++/80564 - bind on SFINAE unfriendly generic lambda
// { dg-do compile { target c++14 } }

#include <functional>

struct A
{
  template<typename T>
  auto operator()(T&)
  { }

  template<typename T>
  auto operator()(T&) const
  { T::fail; }
};

void
test01()
{
  A a;
  std::bind(a, 0)(); // doesn't consider the const overload
  std::bind<void>(a, 0)();
}

void
test02()
{
  auto f = [] (auto& x) { x = 1; };
  int i;
  std::bind(f, i)(); // doesn't try const-invoking the lambda
  std::bind<void>(f, i)();
}

#if __cpp_variadic_using
template<typename... Ts>
struct overloaded : private Ts...
{
  overloaded(Ts... ts) : Ts(ts)... { }
  using Ts::operator()...;
};

void
test03()
{
  A a;
  auto f = std::bind(a, 0);
  overloaded<decltype(f)> g(f);
  g();
}
#endif
