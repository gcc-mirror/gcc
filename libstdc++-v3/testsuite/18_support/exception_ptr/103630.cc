// { dg-do run }

#include <exception>
#if __cplusplus < 201103L
// std::make_exception_ptr is defined for C++98 as a GNU extension
# include <bits/exception_ptr.h>
#endif

#include <testsuite_hooks.h>

struct B
{
  virtual bool derived() const { return false; }
};

struct D : B
{
  virtual bool derived() const { return true; }
};

int main()
{
  D d;
  std::exception_ptr p = std::make_exception_ptr<B&>(d); // PR libstdc++/103630
#if __cpp_exceptions
  try
  {
    std::rethrow_exception(p);
  }
  catch (const D& d)
  {
    VERIFY(d.derived()); // PR libstdc++/103630
  }
  catch (const B& b)
  {
    VERIFY(!b.derived());
  }
#endif
}
