/* Simplest test involving real threads.  Verify we get the correct answer.  */

/* { dg-options "-pthread" } */

#include <stdlib.h>
#include <pthread.h>

static int x;

static void *start (void *dummy __attribute__((unused)))
{
  __transaction_atomic { x++; }
  return NULL;
}

int main()
{
  pthread_t p[10];
  int i;

  for (i = 0; i < 10; ++i)
    pthread_create (p+i, NULL, start, NULL);

  for (i = 0; i < 10; ++i)
    pthread_join  (p[i], NULL);

  if (x != 10)
    abort ();

  return 0;
}
