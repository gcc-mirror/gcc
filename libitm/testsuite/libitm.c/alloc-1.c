// Test that rolling back allocations works.
#include <stdlib.h>

void __attribute((transaction_pure,noinline)) dont_optimize(void* p)
{
  *(volatile char *) p;
}

int main()
{
  __transaction_atomic {
    void *p = malloc (23);
    dont_optimize (p);
    __transaction_cancel;
  }
  return 0;
}
