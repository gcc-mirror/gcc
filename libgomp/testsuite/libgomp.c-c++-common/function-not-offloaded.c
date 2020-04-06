/* { dg-do link } */
/* { dg-excess-errors "unresolved symbol foo, lto1, mkoffload and lto-wrapper fatal errors" { target offload_device_nonshared_as } } */
/* { dg-allow-blank-lines-in-output 1 } */
/* { dg-additional-sources "function-not-offloaded-aux.c" } */

#pragma omp declare target
int var;
#pragma omp end declare target

extern void foo ();

int
main ()
{
#pragma omp target
  foo ();
}
