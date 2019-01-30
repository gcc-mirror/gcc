/* { dg-additional-options "--param=openacc-kernels=parloops" } as this is
   specifically testing "parloops" handling.  */

int
main (void)
{
#pragma acc kernels
  ;
}
