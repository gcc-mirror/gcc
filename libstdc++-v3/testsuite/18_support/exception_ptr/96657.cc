// { dg-options "-nodefaultlibs -lsupc++ -lgcc_s -lc" { target sparc*-*-linux-gnu } }
// { dg-do link { target c++11 } }

#include <exception>

void
test01()
{
  // PR libstdc++/96657 undefined references in libsupc++
  std::make_exception_ptr(1);
}

int
main()
{
  test01();
}
