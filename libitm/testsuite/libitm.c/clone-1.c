/* Verify that we can look up tm clone of transaction_callable
   and transaction_pure.  */

#include <stdlib.h>
#include <libitm.h>

static int x;

int __attribute__((transaction_pure)) pure(int i)
{
  return i+2;
}

int __attribute__((transaction_callable)) callable(void)
{
  return ++x;
}

int main()
{
  if (_ITM_getTMCloneSafe (&pure) != &pure)
    abort ();

  if (_ITM_getTMCloneSafe (&callable) == NULL)
    abort ();

  return 0;
}
