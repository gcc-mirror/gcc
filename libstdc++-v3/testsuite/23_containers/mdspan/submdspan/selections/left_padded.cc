// { dg-do run { target c++26 } }
// { dg-timeout-factor 2 }
#include "testcases.h"

int
main()
{
  test_all<std::layout_left_padded<1>>();
  test_all<std::layout_left_padded<8>>();
  test_all<std::layout_left_padded<dyn>>();
  return 0;
}
