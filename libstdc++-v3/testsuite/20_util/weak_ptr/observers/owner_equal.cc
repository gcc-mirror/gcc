// { dg-do run { target c++26 } }
// { dg-require-effective-target hosted }

// N5008 20.3.2.3.6 weak_ptr observers [util.smartptr.weak.obs]

#include <memory>
#include <testsuite_hooks.h>

struct A { };
struct B { };

void
test01()
{
  // test empty weak_ptr owners compare equivalent
  std::weak_ptr<A> p1;
  std::weak_ptr<B> p2;
  VERIFY( p1.owner_equal(p2) && p2.owner_equal(p1) );

  std::shared_ptr<B> p3;
  VERIFY( p1.owner_equal(p3) && p3.owner_equal(p1) );

  static_assert( noexcept(p1.owner_equal(p1)) );
  static_assert( noexcept(p1.owner_equal(p2)) );
  static_assert( noexcept(p1.owner_equal(p3)) );
  static_assert( noexcept(p2.owner_equal(p1)) );
}


void
test02()
{
  std::shared_ptr<A> a0;
  std::weak_ptr<A> w0(a0);

  std::shared_ptr<A> a1(new A);
  std::weak_ptr<A> w1(a1);
  VERIFY(  a1.owner_equal(w1) &&  w1.owner_equal(a1) );
  VERIFY( !w1.owner_equal(w0) && !w0.owner_equal(w1) );
  VERIFY( !w1.owner_equal(a0) && !a0.owner_equal(w1) );

  std::shared_ptr<B> b1(new B);
  VERIFY( !w1.owner_equal(b1) && !b1.owner_equal(w1) );
}

int
main()
{
  test01();
  test02();
  return 0;
}
