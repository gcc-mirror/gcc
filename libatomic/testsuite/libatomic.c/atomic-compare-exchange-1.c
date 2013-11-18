/* Test __atomic routines for existence and proper execution on 1 byte 
   values with each valid memory model.  */
/* { dg-do run } */

/* Test the execution of the __atomic_compare_exchange_n builtin for a char.  */

extern void abort(void);

char v = 0;
char expected = 0;
char max = ~0;
char desired = ~0;
char zero = 0;

#define STRONG 0
#define WEAK 1

int
main ()
{

  if (!__atomic_compare_exchange_n (&v, &expected, max, STRONG , __ATOMIC_RELAXED, __ATOMIC_RELAXED)) 
    abort ();
  if (expected != 0)
    abort ();

  if (__atomic_compare_exchange_n (&v, &expected, 0, STRONG , __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) 
    abort ();
  if (expected != max)
    abort ();

  if (!__atomic_compare_exchange_n (&v, &expected, 0, STRONG , __ATOMIC_RELEASE, __ATOMIC_ACQUIRE)) 
    abort ();
  if (expected != max)
    abort ();
  if (v != 0)
    abort ();

  if (__atomic_compare_exchange_n (&v, &expected, desired, WEAK, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) 
    abort ();
  if (expected != 0)
    abort ();

  if (!__atomic_compare_exchange_n (&v, &expected, desired, STRONG , __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) 
    abort ();
  if (expected != 0)
    abort ();
  if (v != max)
    abort ();

  /* Now test the generic version.  */

  v = 0;

  if (!__atomic_compare_exchange (&v, &expected, &max, STRONG, __ATOMIC_RELAXED, __ATOMIC_RELAXED))
    abort ();
  if (expected != 0)
    abort ();

  if (__atomic_compare_exchange (&v, &expected, &zero, STRONG , __ATOMIC_ACQUIRE, __ATOMIC_RELAXED)) 
    abort ();
  if (expected != max)
    abort ();

  if (!__atomic_compare_exchange (&v, &expected, &zero, STRONG , __ATOMIC_RELEASE, __ATOMIC_ACQUIRE)) 
    abort ();
  if (expected != max)
    abort ();
  if (v != 0)
    abort ();

  if (__atomic_compare_exchange (&v, &expected, &desired, WEAK, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) 
    abort ();
  if (expected != 0)
    abort ();

  if (!__atomic_compare_exchange (&v, &expected, &desired, STRONG , __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) 
    abort ();
  if (expected != 0)
    abort ();
  if (v != max)
    abort ();

  return 0;
}
