/* Based on sollve_vv's tests/5.0/declare_target/test_nested_declare_target.c.  */

#define N 1024
int a[N], b[N], c[N];  
int i = 0;

void
update ()
{ 
  for (i = 0; i < N; i++)
    {
      a[i] += 1;
      b[i] += 2;
      c[i] += 3;
    }
}

#pragma omp declare target 
#pragma omp declare target link(a,c,b,i)
#pragma omp declare target to(update)  
#pragma omp end declare target

int
main ()
{
  for (i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i + 1;
      c[i] = i + 2;
    }

  //__builtin_printf("i=5: A=%d, B=%d, C=%d\n", a[5], b[5], c[5]);
  
  #pragma omp target map(to: i) map(tofrom: a, b, c) 
  {
    update();  /* Device. */
  }

  //__builtin_printf("i=5: A=%d, B=%d, C=%d\n", a[5], b[5], c[5]);

  for (i = 0; i < N; i++)
    if ( a[i] != i + 1 || b[i] != i + 3 || c[i] != i + 5)
      __builtin_abort();

  update();  /* Host. */

  //__builtin_printf("i=5: A=%d, B=%d, C=%d\n", a[5], b[5], c[5]);

  for (i = 0; i < N; i++)
    if ( a[i] != i + 2 || b[i] != i + 5 || c[i] != i + 8)
      __builtin_abort ();
  
  return 0;
}
