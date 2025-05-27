// { dg-do compile { target c++26 } }

#include <memory>

struct Incomplete;

std::polymorphic<Incomplete>*
test_move(std::polymorphic<Incomplete>& i1, std::polymorphic<Incomplete>& i2)
{
  i1 = std::move(i2);
  swap(i1, i2);
  return new std::polymorphic<Incomplete>(std::move(i1));
}
