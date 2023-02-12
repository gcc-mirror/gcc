// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

#include <memory>
#include <testsuite_hooks.h>

void
test_self_move()
{
  std::shared_ptr<int> sp(new int(66));
  std::weak_ptr<int> wp(sp);
  wp = std::move(wp); // PR libstdc++/108118
  std::shared_ptr<int> sp2(wp);
  VERIFY(sp2 == sp);
}

int main()
{
  test_self_move();
}
