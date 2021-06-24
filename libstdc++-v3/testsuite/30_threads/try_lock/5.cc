// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

#include <mutex>
#include <testsuite_hooks.h>

struct Lockable
{
  static int tries;

  void lock() { }
  void unlock() { }
  bool try_lock() { return ++tries != 2; }
};

int Lockable::tries = 0;

void test01()
{
  Lockable l1, l2, l3;
  std::mutex m1, m2;

  VERIFY( std::try_lock(l1, l2, l3) == 1 );
  VERIFY( Lockable::tries == 2 );

  Lockable::tries = 0;
  VERIFY( std::try_lock(m1, l1, l2, l3) == 2 );
  VERIFY( Lockable::tries == 2 );

  Lockable::tries = 0;
  VERIFY( std::try_lock(l1, l2, l3, m1) == 1 );
  VERIFY( Lockable::tries == 2 );

  Lockable::tries = 0;
  VERIFY( std::try_lock(m1, l1, l2, l3, m2) == 2 );
  VERIFY( Lockable::tries == 2 );
}

int main()
{
  test01();
}
