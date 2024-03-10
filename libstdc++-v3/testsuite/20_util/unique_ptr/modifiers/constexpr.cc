// { dg-do compile { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

constexpr bool
test_release()
{
  std::unique_ptr<int> p1;
  int* r = p1.release();
  VERIFY( !r );
  VERIFY( !p1 );

  std::unique_ptr<int> p2(new int(2));
  r = p2.release();
  VERIFY( r );
  VERIFY( !p2 );
  delete r;

  std::unique_ptr<int[]> a1;
  r = a1.release();
  VERIFY( !r );
  VERIFY( !a1 );

  std::unique_ptr<int[]> a2(new int[2]{});
  r = a2.release();
  VERIFY( r );
  VERIFY( !a2 );
  delete[] r;

  return true;
}
static_assert( test_release() );

constexpr bool
test_reset()
{
  std::unique_ptr<int> p1;
  p1.reset();
  VERIFY( !p1 );
  p1.reset(nullptr);
  VERIFY( !p1 );
  p1.reset(new int(2));
  VERIFY( *p1 == 2 );
  p1.reset(new int(3));
  VERIFY( *p1 == 3 );
  p1.reset(nullptr);
  VERIFY( !p1 );

  std::unique_ptr<int[]> a1;
  a1.reset();
  VERIFY( !a1 );
  a1.reset(nullptr);
  VERIFY( !a1 );
  a1.reset(new int[]{2,3});
  VERIFY( a1[0] == 2 );
  a1.reset(new int[]{4,5,6});
  VERIFY( a1[1] == 5 );
  a1.reset(nullptr);
  VERIFY( !a1 );

  std::unique_ptr<const int[]> a2;
  a2.reset(new int[2]{});

  return true;
}
static_assert( test_reset() );
