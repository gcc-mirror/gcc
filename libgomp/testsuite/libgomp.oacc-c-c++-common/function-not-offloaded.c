/* { dg-do link } */
/* { dg-excess-errors "lto1, mkoffload and lto-wrapper fatal errors" { target { openacc_nvidia_accel_selected || openacc_amdgcn_accel_selected } } } */

int var;
#pragma acc declare create (var)

void __attribute__((noinline, noclone))
foo () /* { dg-error "function 'foo' has been referenced in offloaded code but hasn't been marked to be included in the offloaded code" "" { target { openacc_nvidia_accel_selected || openacc_amdgcn_accel_selected } } } */
{
  var++;
}

int
main ()
{
#pragma acc parallel
  foo ();
}
