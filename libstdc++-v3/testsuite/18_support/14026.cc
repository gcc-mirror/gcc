// PR 14026
// 18.6.4 uncaught_exception

#include <cstdlib>
#include <exception>
#include <testsuite_hooks.h>

static void
no_uncaught ()
{
  if (std::uncaught_exception ())
    abort ();
}

int
main ()
{
  try
    {
      throw 1;
    }
  catch (...)
    {
      try
        {
          throw;
        }
      catch (...)
        {
          no_uncaught ();
        }
    }
  no_uncaught ();
}
