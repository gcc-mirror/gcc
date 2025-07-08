// { dg-do run { target c++26 } }
// { dg-require-effective-target hosted }

// N5008 20.3.2.5 Struct owner_hash [util.smartptr.owner.hash]

#include <memory>
#include <algorithm>
#include <testsuite_hooks.h>

struct A { };

struct B { A a[2]; };

int
test01()
{
  // test empty shared_ptr hashes compare equivalent
  std::owner_hash oh;
  std::shared_ptr<A> p1;
  std::shared_ptr<A> p2;
  VERIFY( oh(p1) == oh(p2) );
  std::weak_ptr<A> p3;
  VERIFY( oh(p1) == oh(p3) );
  VERIFY( oh(p1) == oh(p3) );
  return 0;
}


// Construction from pointer
int
test02()
{
  std::owner_hash oh;

  std::shared_ptr<A> empty;

  std::shared_ptr<A> a1(new A);
  VERIFY( oh(empty) != oh(a1) );

  std::shared_ptr<A> a2(new A);
  VERIFY( oh(a1) != oh(a2) );

  std::weak_ptr<A> w1(a1);
  VERIFY( oh(a1) == oh(w1) );

  std::weak_ptr<A> w2(a2);
  VERIFY( oh(w1) != oh(w2) );

  a1.reset();
  VERIFY( oh(empty) == oh(a1) );
  VERIFY( oh(a1) != oh(w1) );

  a2.reset();
  VERIFY( oh(a2) == oh(a1) );

  return 0;
}

// aliasing
int
test03()
{
  std::owner_hash oh;

  std::shared_ptr<B> b(new B);
  std::shared_ptr<A> a0(b, &b->a[0]);
  std::shared_ptr<A> a1(b, &b->a[1]);
  // values are different but owners are ohuivalent:
  VERIFY( a0 < a1 && oh(a0) == oh(a1) && oh(b) == oh(a0) && oh(b) == oh(a1) );

  std::weak_ptr<A> w0(a0);
  std::weak_ptr<A> w1(a1);
  VERIFY( oh(w0) == oh(w1) );
  VERIFY( oh(a0) == oh(w1) );
  VERIFY( oh(w0) == oh(a1) );

  return 0;
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
