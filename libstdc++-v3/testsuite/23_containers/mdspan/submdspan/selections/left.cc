// { dg-do run { target c++26 } }
#include "testcases.h"

int
main()
{
  test_all<std::layout_left>();
  return 0;
}
