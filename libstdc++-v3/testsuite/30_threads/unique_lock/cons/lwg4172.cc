// { dg-do run { target c++11 } }

// LWG 4172. unique_lock self-move-assignment is broken

#include <mutex>
#include <testsuite_hooks.h>

void
test_self_move()
{
  struct Lockable
  {
    bool locked = false;
    void lock() { locked = true; }
    void unlock() { locked = false; }
  };

  Lockable x;
  std::unique_lock<Lockable> l(x);
  l = std::move(l);
  VERIFY(x.locked);
}

int main()
{
  test_self_move();
}
