/* { dg-do run } */
/* { dg-options "-pthread" } */
/* { dg-xfail-if "" { *-*-* } { "*" } { "" } } */
/* Tests static constructors inside of transactional code.  */

#include <pthread.h>
#include <stdlib.h>

int f(int x) __attribute__((noinline,transaction_safe));
int f(int x)
{
  static int y = x;
  return y*x;
}

static void *thread (void *)
{
  int bar;
  __transaction_atomic { bar = f(10); }
  if (bar != 100)
    abort();
  return 0;
}

int main()
{
  int bar;

  // First, initialize y in another thread.
  pthread_t pt;
  pthread_create(&pt, NULL, thread, NULL);
  pthread_join(pt, NULL);

  // Now y should already be initialized.
  __transaction_atomic { bar = f(20); }
  if (bar != 200)
    abort();

  return 0;
}
