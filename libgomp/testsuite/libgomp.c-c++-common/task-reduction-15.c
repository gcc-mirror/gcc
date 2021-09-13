/* PR middle-end/101167 */

extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

struct S { int a, b, c[2]; };

void
init (struct S *x)
{
  x->a = 0;
  x->b = 0;
  x->c[0] = 0;
  x->c[1] = 0;
}

void
merge (struct S *x, struct S *y)
{
  x->a += y->a;
  x->b += y->b;
}

#pragma omp declare reduction (+: struct S : merge (&omp_out, &omp_in)) initializer (init (&omp_priv))

void
foo (struct S x)
{
  #pragma omp taskgroup task_reduction (+: x)
  {
    #pragma omp task in_reduction (+: x)
    {
      x.a++;
      x.b++;
    }
    #pragma omp task in_reduction (+: x)
    {
      x.a += 4;
      x.b += 14;
    }
    #pragma omp task in_reduction (+: x)
    {
      x.a += 9;
      x.b += 19;
    }
  }
  if (x.a != 56 || x.b != 86)
    abort ();
}

int
main ()
{
  struct S x = { 42, 52 };
  #pragma omp parallel master num_threads(3)
  foo (x);
  return 0;
}
