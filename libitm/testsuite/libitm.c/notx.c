/* These tests all check whether initialization happens properly even if no
   transaction has been used in the current thread yet.  */

/* { dg-options "-pthread" } */

#include <stdlib.h>
#include <pthread.h>
#include <libitm.h>

static void *test1 (void *dummy __attribute__((unused)))
{
  if (_ITM_inTransaction() != outsideTransaction)
    abort();
  return NULL;
}

static void *test2 (void *dummy __attribute__((unused)))
{
  if (_ITM_getTransactionId() != _ITM_noTransactionId)
    abort();
  return NULL;
}


int main()
{
  pthread_t thread;

  pthread_create(&thread, NULL, test1, NULL);
  pthread_join(thread, NULL);

  pthread_create(&thread, NULL, test2, NULL);
  pthread_join(thread, NULL);

  return 0;
}
