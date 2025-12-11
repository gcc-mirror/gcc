// { dg-do run { target c++17 } }

#include <variant>
#include <testsuite_hooks.h>

struct NonEmpty { int x; };
struct NonTrivial
{
  constexpr NonTrivial() : x(0) {}
  constexpr NonTrivial(int p) : x(p) {}
  ~NonTrivial() {}

  int x;
};

struct TrivialEmpty {};
struct NonTrivialEmpty { ~NonTrivialEmpty() {} };

template<typename T>
struct Compose : T
{
  std::variant<T, int> v;
};

template<typename T>
bool testAlias()
{
  Compose<T> c;
  return static_cast<T*>(&c) == &std::get<T>(c.v);
}

int main()
{
  VERIFY( !testAlias<NonEmpty>() );
  VERIFY( !testAlias<NonTrivial>() );
  VERIFY( !testAlias<TrivialEmpty>() );
#if (__cplusplus >= 202002L) || !defined(_GLIBCXX_USE_VARIANT_CXX17_OLD_ABI)
  VERIFY( !testAlias<NonTrivialEmpty>() );
#else
  VERIFY(  testAlias<NonTrivialEmpty>() );
#endif
}
