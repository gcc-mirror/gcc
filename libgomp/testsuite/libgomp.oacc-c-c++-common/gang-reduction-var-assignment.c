/* { dg-xfail-run-if "PR88852" { openacc_host_selected } } */

int
main (void)
{
  int x = 123;

#pragma acc parallel num_gangs(1) reduction (+: x)
  {
    x = 23;
  }
  if (x != 146)
    __builtin_abort();

  return 0;
}
