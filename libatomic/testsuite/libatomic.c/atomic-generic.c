/* Test generic __atomic routines for proper function calling.
   memory model.  */
/* { dg-options "-w" } */
/* { dg-do run } */

/* Test that the generioc atomic builtins execute as expected..
   sync-mem-generic-aux.c supplies a functional external entry point for 
   the 4 generic functions.  */

#include <stdlib.h>
#include <stdbool.h>

extern void abort();

typedef struct test {
  int array[10];
} test_struct;

test_struct zero = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
test_struct ones = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
test_struct a,b;

int size = sizeof (test_struct);
/* Test for consistency on sizes 1, 2, 4, 8, 16 and 32.  */
int
main ()
{
  test_struct c;

  __atomic_store (&a, &zero, __ATOMIC_RELAXED);
  if (memcmp (&a, &zero, size))
    abort ();

  __atomic_exchange (&a, &ones, &c, __ATOMIC_SEQ_CST);
  if (memcmp (&c, &zero, size))
    abort ();
  if (memcmp (&a, &ones, size))
    abort ();

  __atomic_load (&a, &b, __ATOMIC_RELAXED);
  if (memcmp (&b, &ones, size))
    abort ();

  if (!__atomic_compare_exchange (&a, &b, &zero, false, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE))
    abort();
  if (memcmp (&a, &zero, size))
    abort ();

  if (__atomic_compare_exchange (&a, &b, &ones, false, __ATOMIC_SEQ_CST, __ATOMIC_ACQUIRE))
    abort();
  if (memcmp (&b, &zero, size))
    abort ();

  return 0;
}

