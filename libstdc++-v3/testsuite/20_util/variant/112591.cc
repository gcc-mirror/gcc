// { dg-do run { target c++17 } }

#include <variant>
#include <testsuite_hooks.h>

struct NonEmpty { int x; };
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
  VERIFY( !testAlias<TrivialEmpty>() );
#if __cplusplus >= 202002L  
  VERIFY( !testAlias<NonTrivialEmpty>() );
#else  
  VERIFY( testAlias<NonTrivialEmpty>() );
#endif
}
