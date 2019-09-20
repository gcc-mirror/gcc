/* { dg-do run } */

#include <stdlib.h>

void workers (void)
{
  double res[65536];
  int i;

#pragma acc parallel copyout(res) num_gangs(64) num_workers(64)
  /* { dg-warning "using num_workers \\(32\\), ignoring 64" "" { target openacc_nvidia_accel_selected } .-1 } */
  {
    int i, j;
#pragma acc loop gang
    for (i = 0; i < 256; i++)
      {
#pragma acc loop worker
	for (j = 0; j < 256; j++)
	  {
	    int tmpvar;
	    int &tmpref = tmpvar;
	    tmpref = (i * 256 + j) * 99;
	    res[i * 256 + j] = tmpref;
	  }
      }
  }

  for (i = 0; i < 65536; i++)
    if (res[i] != i * 99)
      abort ();
}

void vectors (void)
{
  double res[65536];
  int i;

#pragma acc parallel copyout(res) num_gangs(64) num_workers(64)
  /* { dg-warning "using num_workers \\(32\\), ignoring 64" "" { target openacc_nvidia_accel_selected } .-1 } */
  {
    int i, j;
#pragma acc loop gang worker
    for (i = 0; i < 256; i++)
      {
#pragma acc loop vector
	for (j = 0; j < 256; j++)
	  {
	    int tmpvar;
	    int &tmpref = tmpvar;
	    tmpref = (i * 256 + j) * 101;
	    res[i * 256 + j] = tmpref;
	  }
      }
  }

  for (i = 0; i < 65536; i++)
    if (res[i] != i * 101)
      abort ();
}

int main (int argc, char *argv[])
{
  workers ();
  vectors ();
  return 0;
}
