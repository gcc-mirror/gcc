/* { dg-do run } */
/* { dg-require-effective-target offload_device_nonshared_as } */

#include <omp.h>
#include <stdlib.h>

int main ()
{
  int a = 100;
  int b = 0;
  int c, d;

  #pragma omp target if(a > 200 && a < 400) map(from: c)
    c = omp_is_initial_device ();

  #pragma omp target data map(to: b) if(a > 200 && a < 400)
    #pragma omp target map(from: b, d)
      {
	b = 100;
	d = omp_is_initial_device ();
      }

  if (b != 100 || !c || d)
    abort ();

  a += 200;
  b = 0;

  #pragma omp target if(a > 200 && a < 400) map(from: c)
    c = omp_is_initial_device ();

  #pragma omp target data map(to: b) if(a > 200 && a < 400)
    #pragma omp target map(from: b, d)
      {
	b = 100;
	d = omp_is_initial_device ();
      }

  if (b != 0 || c || d)
    abort ();

  a += 200;
  b = 0;

  #pragma omp target if(a > 200 && a < 400) map(from: c)
    c = omp_is_initial_device ();

  #pragma omp target data map(to: b) if(a > 200 && a < 400)
    #pragma omp target map(from: b, d)
      {
	b = 100;
	d = omp_is_initial_device ();
      }

  if (b != 100 || !c || d)
    abort ();

  return 0;
}
