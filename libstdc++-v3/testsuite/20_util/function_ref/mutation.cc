// { dg-do run { target c++26 } }

#include <functional>
#include <testsuite_hooks.h>

using std::nontype;
using std::function_ref;

void
test01()
{
  struct F {
    int v;
    int operator()() { return v; }
  };
  F f1{2}, f2{5};

  function_ref<int()> r1(f1);
  function_ref<long()> r2(f1);

  VERIFY( r1() == 2 );
  VERIFY( r2() == 2 );

  f1.v = 10;

  VERIFY( r1() == 10 );
  VERIFY( r2() == 10 );

  r1 = function_ref<int()>(f2);
  r2 = function_ref<long()>(f2);

  VERIFY( r1() == 5 );
  VERIFY( r2() == 5 );

  f2.v = 13;

  VERIFY( r1() == 13 );
  VERIFY( r2() == 13 );

  r1 = function_ref<int()>(f1);
  r2 = function_ref<long()>(f1);

  f1.v = 20;
  VERIFY( r1() == 20 );
  VERIFY( r2() == 20 );
}

void
test02()
{
  struct S 
  {
    int x;
    int f() { return x; };
  };
  S s{10};

  function_ref<int()> r1(nontype<&S::x>, s);
  function_ref<long()> r2(nontype<&S::x>, &s);

  VERIFY( r1() == 10 );
  VERIFY( r2() == 10 );

  s.x = 20;

  VERIFY( r1() == 20 );
  VERIFY( r2() == 20 );

  r1 = function_ref<int()>(nontype<&S::f>, &s);
  r2 = function_ref<long()>(nontype<&S::f>, s);

  VERIFY( r1() == 20 );
  VERIFY( r2() == 20 );

  s.x = 30;

  VERIFY( r1() == 30 );
  VERIFY( r2() == 30 );
}

int main()
{
  test01();
  test02();
}
