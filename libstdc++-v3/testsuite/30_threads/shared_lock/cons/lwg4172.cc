// { dg-do run { target c++14 } }

// LWG 4172. unique_lock self-move-assignment is broken

#include <shared_mutex>
#include <testsuite_hooks.h>

void
test_self_move()
{
  struct Lockable
  {
    bool locked = false;
    void lock_shared() { locked = true; }
    void unlock_shared() { locked = false; }
    bool try_lock_shared() { if (locked) return false; return locked = true; }
  };

  Lockable x;
  std::shared_lock<Lockable> l(x);
  l = std::move(l);
  VERIFY(x.locked);
}

int main()
{
  test_self_move();
}
