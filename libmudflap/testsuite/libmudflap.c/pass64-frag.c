/* PR libmudflap/36397 */
/* { dg-do run } */
/* { dg-options "-O -fmudflap -fno-strict-aliasing -lmudflap" } */

struct A
{
  int a[2];
};

long long int x;

int __attribute__ ((noinline))
baz (long long int *x)
{
  return *x;
}

int __attribute__ ((noinline))
foo (int i)
{
  if (i > 10)
    return baz (&x);
  return ((struct A *) &x)->a[i];
}

int
main (void)
{
  if (sizeof (long long) == 2 * sizeof (int)
      && sizeof (long long) == sizeof (struct A))
    {
      struct A a = { .a[0] = 10, .a[1] = 20 };
      __builtin_memcpy (&x, &a, sizeof (x));
      if (foo (0) != 10 || foo (1) != 20)
        __builtin_abort ();
    }
  return 0;
}
