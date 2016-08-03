/* { dg-do compile } */
/* { dg-options "-O0" } */

/* ICEd in nvptx backend due to unexpected frame size.  */
#pragma acc routine worker
void
worker_matmul (int *c, int i)
{
  int j;

#pragma acc loop
  for (j = 0; j < 4; j++)
    c[j] = j;
}


int
main ()
{
  int c[4];

#pragma acc parallel 
  {
    worker_matmul (c, 0);
  }
  
  return 0;
}
