/* { dg-do compile } */
/* { dg-additional-options "-w" } */

int
main (void)
{
#pragma acc parallel
  foo ();

  return 0;
}
