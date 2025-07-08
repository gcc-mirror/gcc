// { dg-do run { target c++26 } }
// { dg-require-effective-target hosted }

// N5008 20.3.2.6 Struct owner_equal [util.smartptr.owner.equal]

#include <memory>
#include <algorithm>
#include <testsuite_hooks.h>

struct A { };

struct B { A a[2]; };

int
test01()
{
  // test empty shared_ptr owners compare equivalent
  std::owner_equal eq;
  std::shared_ptr<A> p1;
  std::shared_ptr<A> p2;
  VERIFY( eq(p1, p2) && eq(p2, p1) );
  std::weak_ptr<A> p3;
  VERIFY( eq(p1, p3) && eq(p3, p1) );
  VERIFY( eq(p1, p3) && eq(p3, p1) );
  return 0;
}


// Construction from pointer
int
test02()
{
  std::owner_equal eq;

  std::shared_ptr<A> empty;

  std::shared_ptr<A> a1(new A);
  VERIFY( !eq(empty, a1) && !eq(a1, empty) );

  std::shared_ptr<A> a2(new A);
  VERIFY( !eq(a1, a2) && !eq(a2, a1) );

  std::weak_ptr<A> w1(a1);
  VERIFY( eq(a1, w1) && eq(w1, a1) );

  std::weak_ptr<A> w2(a2);
  VERIFY( !eq(w1, w2) && !eq(w2, w1) );

  a1.reset();
  VERIFY( eq(empty, a1) && eq(a1, empty) );
  VERIFY( !eq(a1, w1) && !eq(w1, a1) );

  a2.reset();
  VERIFY( eq(a2, a1) && eq(a1, a2) );

  return 0;
}

// aliasing
int
test03()
{
  std::owner_equal eq;

  std::shared_ptr<B> b(new B);
  std::shared_ptr<A> a0(b, &b->a[0]);
  std::shared_ptr<A> a1(b, &b->a[1]);
  // values are different but owners are equivalent:
  VERIFY( a0 < a1 && eq(a0, a1) && eq(b, a0) && eq(b, a1) );

  std::weak_ptr<A> w0(a0);
  std::weak_ptr<A> w1(a1);
  VERIFY( eq(w0, w1) && eq(w1, w0) );
  VERIFY( eq(a0, w1) && eq(w1, a0) );
  VERIFY( eq(w0, a1) && eq(a1, w0) );

  return 0;
}

// as binary predicate
int
test04()
{
  std::owner_equal eq;

  std::shared_ptr<B> b(new B);
  std::shared_ptr<A> a0(b, &b->a[0]);
  std::shared_ptr<A> a1(b, &b->a[1]);
  std::shared_ptr<A> c(new A);
  std::weak_ptr<A> a[3]{a0, a1, c};
  std::weak_ptr<A>* p = std::unique(a, a+3, eq);
  VERIFY( p == &a[2] );

  return 0;
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
