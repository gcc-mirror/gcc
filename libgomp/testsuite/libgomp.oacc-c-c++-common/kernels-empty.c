/* { dg-additional-options "-fopenacc-kernels=parloops" } as this is
   specifically testing "parloops" handling.  */

int
main (void)
{
#pragma acc kernels
  ;
}
