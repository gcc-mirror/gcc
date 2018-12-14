/* { dg-do link } */

int var; /* { dg-error "'var' requires a 'declare' directive for use in a 'routine' function" } */

#pragma acc routine
void  __attribute__((noinline, noclone))
foo (void)
{
  var++;
}

int
main ()
{
#pragma acc parallel
  foo ();
}
