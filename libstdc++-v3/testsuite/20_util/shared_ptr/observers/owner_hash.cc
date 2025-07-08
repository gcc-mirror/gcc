// { dg-do run { target c++26 } }
// { dg-require-effective-target hosted }

// N5008 20.3.2.2.6 shared_ptr observers [util.smartptr.shared.obs]

#include <memory>
#include <testsuite_hooks.h>

struct A
{
  int i;
  virtual ~A() { }
};

struct B : A
{
};

void
test01()
{
  // test empty shared_ptr hashes compare equivalent
  std::shared_ptr<A> p1;
  std::shared_ptr<B> p2;
  VERIFY( p1.owner_hash() == p2.owner_hash() );
}


// Construction from pointer
void
test02()
{
  std::shared_ptr<A> a0;

  std::shared_ptr<A> a1(new A);
  VERIFY( a1.owner_hash() != a0.owner_hash() );

  std::shared_ptr<B> b1(new B);
  VERIFY( a1.owner_hash() != b1.owner_hash() );

  std::shared_ptr<A> a2(a1);
  VERIFY( a1.owner_hash() == a2.owner_hash() );
  a2 = b1;
  VERIFY( b1.owner_hash() == a2.owner_hash() );

  std::weak_ptr<A> w1(a1);
  VERIFY( a1.owner_hash() == w1.owner_hash() );
  std::weak_ptr<A> w2(a2);
  VERIFY( b1.owner_hash() == w2.owner_hash() );

  static_assert( noexcept(a1.owner_hash()) );
  static_assert( noexcept(b1.owner_hash()) );
}

// Aliasing
void
test03()
{
  std::shared_ptr<A> p1(new A());
  std::shared_ptr<int> p2(p1, &p1->i);
  VERIFY( p1.owner_hash() == p2.owner_hash() );
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
