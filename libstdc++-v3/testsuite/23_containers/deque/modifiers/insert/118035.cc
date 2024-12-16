// { dg-do run }

#include <deque>
#include <testsuite_hooks.h>

struct Sparks
{
  Sparks& operator=(const Sparks& s)
  {
    VERIFY( this != &s ); // This town ain't big enough for the both of us.
    return *this;
  }
};

void
test_pr118035()
{
  std::deque<Sparks> d(3, Sparks());
  Sparks s[1];
  d.insert(d.begin() + 1, s, s);
}

int main()
{
  test_pr118035();
}
