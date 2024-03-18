// { dg-do compile { target c++20 } }

// LWG 3436. std::construct_at should support arrays

#include <memory>
#include <testsuite_hooks.h>

constexpr void
test_array()
{
  int arr[1] { 99 };
  std::construct_at(&arr);
  VERIFY( arr[0] == 0 );

  union U {
    long long x;
    int arr[4];
  } u;
  u.x = -1;

  auto p = std::construct_at(&u.arr);
  VERIFY( (*p)[0] == 0 );
  VERIFY( (*p)[1] == 0 );
  VERIFY( (*p)[2] == 0 );
  VERIFY( (*p)[3] == 0 );

  struct NonTrivial {
    constexpr NonTrivial() : i(99) { }
    int i;
  };

  union U2 {
    char c = 'a';
    NonTrivial arr[2];
  } u2;

  auto p2 = std::construct_at(&u2.arr);
  VERIFY( (*p2)[0].i == 99 );
}

static_assert( [] { test_array(); return true; }() );
