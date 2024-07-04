// { dg-do run { target c++14 } }
// { dg-require-gthreads "" }
// { dg-additional-options "-pthread" { target pthread } }

#include <shared_mutex>
#include <system_error>
#include <testsuite_hooks.h>

// PR libstdc++/112089 shared_lock::unlock should throw operation_not_permitted

int main()
{
  std::shared_lock<std::shared_timed_mutex> l;
  try
  {
    l.unlock();
    VERIFY( false );
  }
  catch (const std::system_error& e)
  {
    VERIFY( e.code() == std::errc::operation_not_permitted );
  }
}
