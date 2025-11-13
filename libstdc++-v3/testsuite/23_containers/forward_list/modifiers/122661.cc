// { dg-do compile { target c++11 } }

// Bug 122661 - Incorrect value category handling in forward_list::assign

#include <forward_list>

struct S
{
  S();
  S& operator=(S const&) & = delete;
  S& operator=(S const&) &&;
};

void
test_pr122661()
{
  std::forward_list<S> fl;
  S* iter = nullptr;
  fl.assign(iter, iter);
}
