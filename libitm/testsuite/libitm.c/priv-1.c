/* Quick stress test for proxy privatization.  */

/* We need to use a TM method that has to enforce privatization safety
   explicitly.  */
/* { dg-set-target-env-var ITM_DEFAULT_METHOD "ml_wt" } */

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

/* Make them likely to be mapped to different orecs.  */
#define ALIGN __attribute__((aligned (256)))
/* Don't make these static to work around PR 68591.  */
int x ALIGN;
int *ptr ALIGN;
int *priv_ptr ALIGN;
int priv_value ALIGN;
int barrier ALIGN = 0;
const int iters = 100;

static void arrive_and_wait (int expected_value)
{
  int now = __atomic_add_fetch (&barrier, 1, __ATOMIC_ACQ_REL);
  while (now < expected_value)
    __atomic_load (&barrier, &now, __ATOMIC_ACQUIRE);
}

static void __attribute__((transaction_pure,noinline)) delay (int i)
{
  for (volatile int v = 0; v < i; v++);
}

/* This tries to catch a case in which proxy privatization safety is not
   ensured by privatization_user.  Specifically, it's access to the value
   of it's transactional snapshot of ptr must read from an uncommitted write
   by writer; thus, writer must still be active but must have read ptr before
   proxy can privatize *ptr by assigning to ptr.
   We try to make this interleaving more likely by delaying the commit of
   writer and the start of proxy.  */
static void *writer (void *dummy __attribute__((unused)))
{
  for (int i = 0; i < iters; i++)
    {
      /* Initialize state in each round.  */
      x = 0;
      ptr = &x;
      priv_ptr = NULL;
      int wrote = 1;
      arrive_and_wait (i * 6 + 3);
      /* Interference by another writer.  Has a conflict with the proxy
	 privatizer.  */
      __transaction_atomic
	{
	  if (ptr != NULL)
	    *ptr = 1;
	  else
	    wrote = 0;
	  delay (2000000);
	}
      arrive_and_wait (i * 6 + 6);
      /* If the previous transaction committed first, wrote == 1 and x == 1;
	 otherwise, if the proxy came first, wrote == 0 and priv_value == 0.
       */
      if (wrote != priv_value)
	abort ();
    }
  return NULL;
}

static void *proxy (void *dummy __attribute__((unused)))
{
  for (int i = 0; i < iters; i++)
    {
      arrive_and_wait (i * 6 + 3);
      delay(1000000);
      __transaction_atomic
	{
	  /* Hand-off to privatization-user and its read-only transaction and
	     subsequent use of privatization.  */
	  priv_ptr = ptr;
	  ptr = NULL;
	}
      arrive_and_wait (i * 6 + 6);
    }
  return NULL;
}

static void *privatization_user (void *dummy __attribute__((unused)))
{
  for (int i = 0; i < iters; i++)
    {
      arrive_and_wait (i * 6 + 3);
      /* Spin until we have gotten a pointer from the proxy.  Then access
	 the value pointed to nontransactionally.  */
      int *p = NULL;
      while (p == NULL)
	__transaction_atomic { p = priv_ptr; }
      priv_value = *p;
      arrive_and_wait (i * 6 + 6);
    }
  return NULL;
}

int main()
{
  pthread_t p[3];

  pthread_create (p+0, NULL, writer, NULL);
  pthread_create (p+1, NULL, proxy, NULL);
  pthread_create (p+2, NULL, privatization_user, NULL);

  for (int i = 0; i < 3; ++i)
    pthread_join  (p[i], NULL);

  return 0;
}
