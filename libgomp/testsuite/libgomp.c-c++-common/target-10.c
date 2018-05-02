/* { dg-do run } */

#pragma omp declare target
extern int v;
#pragma omp end declare target

int v;

int
main ()
{
  #pragma omp target update to(v)
  return 0;
}
