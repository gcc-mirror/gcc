/* { dg-skip-if "" { *-*-* } } */

#pragma omp declare target
extern int var;
#pragma omp end declare target

void __attribute__((noinline, noclone))
foo (void)
{
  var++;
}

