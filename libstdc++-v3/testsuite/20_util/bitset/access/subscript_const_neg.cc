// { dg-do compile }
#include <bitset>

void test_const_subscript_assignment()
{
  const std::bitset<13> bs(0x1555ull);
  for (int i = 0; i < 13; ++i)
    bs[i] = not bs[i];  // { dg-error "lvalue required" }
}

int main()
{
  test_const_subscript_assignment();
}
