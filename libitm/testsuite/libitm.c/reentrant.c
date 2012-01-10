/* { dg-do run } */
/* { dg-options "-pthread" } */

/* Tests that new transactions can be started from both transaction_pure and
   transaction_unsafe code. This also requires proper handling of reentrant
   nesting in the serial_lock implementation. */

#include <stdlib.h>
#include <pthread.h>
#include <libitm.h>

int x = 0;

int __attribute__((transaction_pure)) pure(int i)
{
  __transaction_atomic {
    x++;
  }
  if (_ITM_inTransaction() == outsideTransaction)
    abort();
  return i+1;
}

int __attribute__((transaction_unsafe)) unsafe(int i)
{
  if (_ITM_inTransaction() != inIrrevocableTransaction)
    abort();
  __transaction_atomic {
    x++;
  }
  if (_ITM_inTransaction() != inIrrevocableTransaction)
    abort();
  return i+1;
}

static void *thread (void *dummy __attribute__((unused)))
{
  __transaction_atomic {
    pure(x);
  }
  __transaction_relaxed {
    unsafe(1);
  }
  return 0;
}

int main()
{
  pthread_t pt;
  int r = 0;

  __transaction_atomic {
    r += pure(1) + x;
  }
  __transaction_relaxed {
    r += unsafe(1) + x;
  }
  if (r != 7)
    abort();

  // Spawn a new thread to check that the serial lock is not held.
  pthread_create(&pt, NULL, thread, NULL);
  pthread_join(pt, NULL);
  if (x != 4)
    abort();
  return 0;
}
