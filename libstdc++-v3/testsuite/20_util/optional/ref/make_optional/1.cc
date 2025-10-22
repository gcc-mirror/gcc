// { dg-do run { target c++20 }  }

#include <optional>
#include <utility>
#include <testsuite_hooks.h>

struct NonTrivial
{
  constexpr NonTrivial() {}
  constexpr NonTrivial(NonTrivial const&) {};
  constexpr ~NonTrivial() {};
};

template<typename T>
struct Conv
{
  T t;

  constexpr operator T() const noexcept
  { return t; }
};

constexpr bool test()
{
  NonTrivial t;
  const NonTrivial& ct = t;

#if __cplusplus > 202302
  auto o1 = std::make_optional<NonTrivial&>(t);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() == &t );

  auto o2 = std::make_optional<const NonTrivial&>(t);
  VERIFY( o2.has_value() );
  VERIFY( &o2.value() == &t );

  auto o3 = std::make_optional<const NonTrivial&>(ct);
  VERIFY( o3.has_value() );
  VERIFY( &o3.value() == &t );

  Conv<NonTrivial&> rw(t);
  auto o4 = std::make_optional<NonTrivial&>(rw);
  VERIFY( o4.has_value() );
  VERIFY( &o4.value() == &t );

  auto o5 = std::make_optional<NonTrivial&>(std::as_const(rw));
  VERIFY( o5.has_value() );
  VERIFY( &o5.value() == &t );

  auto o6 = std::make_optional<NonTrivial&>(Conv<NonTrivial&>(t));
  VERIFY( o6.has_value() );
  VERIFY( &o6.value() == &t );
#else
  auto o1 = std::make_optional<NonTrivial&>(t);
  VERIFY( o1.has_value() );
  VERIFY( &o1.value() != &t );

  auto o3 = std::make_optional<const NonTrivial&>(ct);
  VERIFY( o3.has_value() );
  VERIFY( &o3.value() != &t );

  auto o2 = std::make_optional<NonTrivial&&>(std::move(t));
  VERIFY( o2.has_value() );
  VERIFY( &o2.value() != &t );
#endif

  return true;
}

int main()
{
  test();
  static_assert(test());
}
