/* { dg-do run { target offload_device_nonshared_as } } */

struct S
{
  int a, b, c;
};
typedef struct S S;

int main (void)
{
  S s;
  #pragma omp target data map (alloc: s.a, s.c)
  {
    #pragma omp target enter data map (alloc: s.b)
  }

  return 0;
}
/* { dg-output "Trying to map into device \\\[\[0-9a-fA-FxX\]+..\[0-9a-fA-FxX\]+\\\) structure element when other mapped elements from the same structure weren't mapped together with it" } */
/* { dg-shouldfail "" } */
