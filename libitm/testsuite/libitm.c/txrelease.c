/* This test triggers execution of the code that releases per-thread
   transaction data when a thread exists, potentially repeatedly. However,
   we currently cannot check whether the data has indeed been released.  */

/* { dg-options "-pthread" } */

#include <stddef.h>
#include <stdlib.h>
#include <pthread.h>

static int round = 0;
static pthread_key_t key;

static void
thread_exit_handler(void *dummy __attribute__((unused)))
{
  if (round == 0)
    abort();
  if (round == 1)
    {
      // ??? It would be good if we could check here that the transaction has
      // indeed been released.
      __transaction_atomic { round++; }
      if (pthread_setspecific(key, &round))
	abort();
    }
  // ??? It would be good if we could check here that the transaction has
  // indeed been released (again).
}

static void *thread (void *dummy __attribute__((unused)))
{
  if (pthread_key_create(&key, thread_exit_handler))
    abort();
  if (pthread_setspecific(key, &round))
    abort();
  __transaction_atomic { round++; }
  return NULL;
}

int main()
{
  pthread_t pt;
  pthread_create(&pt, NULL, thread, NULL);
  pthread_join(pt, NULL);
  if (round != 2)
    abort();
  return 0;
}
