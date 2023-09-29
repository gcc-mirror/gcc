#include <testsuite_hooks.h>
#include <replacement_memory_operators.h>

int main()
{
  VERIFY( __gnu_test::counter::count() == 0 );
  return 0;
}
