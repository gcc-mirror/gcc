/* Verify that two sequential runs of a transaction will complete and
   produce correct results.  An early test of the library did in fact
   leave things in an inconsistent state following the commit of the
   first transaction.  */

#include <stdlib.h>

static int x;

static void start (void)
{
  __transaction_atomic { x++; }
}

int main()
{
  start ();
  start ();

  if (x != 2)
    abort ();

  return 0;
}
