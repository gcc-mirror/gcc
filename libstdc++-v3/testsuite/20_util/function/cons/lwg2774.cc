// { dg-do run { target c++11 } }
#include <functional>
#include <testsuite_hooks.h>

struct Funk
{
  Funk() = default;
  Funk(const Funk&) { ++copies; }
  Funk(Funk&&) { ++moves; }

  void operator()() const { }

  static int copies;
  static int moves;
};

int Funk::copies = 0;
int Funk::moves = 0;

int main()
{
  Funk e;
  // LWG 2774 means there should be no move here:
  std::function<void()> fc(e);
  VERIFY(Funk::copies == 1);
  VERIFY(Funk::moves == 0);
  // And only one move here:
  std::function<void()> fm(std::move(e));
  VERIFY(Funk::copies == 1);
  VERIFY(Funk::moves == 1);
}
