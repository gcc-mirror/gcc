// { dg-do compile }

#include <vector>

template<class T> struct Holder { T t; }; // { dg-bogus "incomplete type" }
struct Incomplete;

void destroy(std::vector<Holder<Incomplete>*>* p)
{
  p->~vector();
}
