/* { dg-do run } */

int
main (void)
{
  #pragma acc parallel async
  ;

  #pragma acc parallel async
  ;

  #pragma acc wait

  return 0;
}
