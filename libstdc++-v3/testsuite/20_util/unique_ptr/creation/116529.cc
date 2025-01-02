// { dg-do run { target c++11 } }

// Bug libstdc++/116529 - Construction of unique_ptr with reference type
// is rejected because of auto_ptr constructor

#include <memory>
#include <testsuite_hooks.h>

int count = 0;

struct X
{
  ~X() { ++count; }
};

struct deleter : std::default_delete<X>
{
  using pointer = X*;
};

void
test01()
{
  {
    std::unique_ptr<X&, deleter> up(new X);
    // { dg-bogus "forming pointer to reference" "" { target *-*-* } 0 }
    VERIFY( count == 0 );
  }
  VERIFY( count == 1 );
}

int main()
{
  test01();
}
