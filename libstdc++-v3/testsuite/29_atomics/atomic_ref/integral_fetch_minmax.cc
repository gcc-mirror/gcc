// { dg-do run { target c++26 } }
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }

#include <atomic>
#include <limits.h>
#include <testsuite_hooks.h>

void
test01()
{
  int value;
  const auto mo = std::memory_order_relaxed;

  {
    std::atomic_ref<int> a(value);

    a = 100;
    auto v = a.fetch_min(50);
    VERIFY( v == 100 );
    VERIFY( a == 50 );

    v = a.fetch_min(75, mo);
    VERIFY( v == 50 );
    VERIFY( a == 50 );

    v = a.fetch_min(25);
    VERIFY( v == 50 );
    VERIFY( a == 25 );

    a = -10;
    v = a.fetch_min(-20);
    VERIFY( v == -10 );
    VERIFY( a == -20 );

    v = a.fetch_min(-5, mo);
    VERIFY( v == -20 );
    VERIFY( a == -20 );

    a = 20;
    v = a.fetch_max(50);
    VERIFY( v == 20 );
    VERIFY( a == 50 );

    v = a.fetch_max(30, mo);
    VERIFY( v == 50 );
    VERIFY( a == 50 );

    v = a.fetch_max(100);
    VERIFY( v == 50 );
    VERIFY( a == 100 );

    a = -50;
    v = a.fetch_max(-20);
    VERIFY( v == -50 );
    VERIFY( a == -20 );

    v = a.fetch_max(-30, mo);
    VERIFY( v == -20 );
    VERIFY( a == -20 );
  }

  VERIFY( value == -20 );
}

void
test02()
{
  unsigned short value;
  const auto mo = std::memory_order_relaxed;

  {
    std::atomic_ref<unsigned short> a(value);

    a = 100;
    auto v = a.fetch_min(50);
    VERIFY( v == 100 );
    VERIFY( a == 50 );

    v = a.fetch_min(75, mo);
    VERIFY( v == 50 );
    VERIFY( a == 50 );

    a = 20;
    v = a.fetch_max(50);
    VERIFY( v == 20 );
    VERIFY( a == 50 );

    v = a.fetch_max(30, mo);
    VERIFY( v == 50 );
    VERIFY( a == 50 );

    v = a.fetch_max(200);
    VERIFY( v == 50 );
    VERIFY( a == 200 );
  }

  VERIFY( value == 200 );
}

void
test03()
{
  int i = INT_MIN;
  std::atomic_ref<int> a(i);
  a.fetch_min(INT_MAX);
  VERIFY( a == INT_MIN );
  a.fetch_max(INT_MAX);
  VERIFY( a == INT_MAX );
}

int
main()
{
  test01();
  test02();
  test03();
}
