#include <assert.h>

/* Test of reduction on loop directive (vector reduction in
   gang-partitioned/worker-partitioned mode, private reduction variable).  */

int
main (int argc, char *argv[])
{
  int i, j, k;
  double ina[1024], inb[1024], out[1024], acc;

  for (j = 0; j < 32; j++)
    for (i = 0; i < 32; i++)
      {
        ina[j * 32 + i] = (i == j) ? 2.0 : 0.0;
	inb[j * 32 + i] = (double) (i + j);
      }

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(acc) copyin(ina, inb) copyout(out)
  {
    #pragma acc loop gang worker
    for (k = 0; k < 32; k++)
      for (j = 0; j < 32; j++)
        {
	  acc = 0;

	  #pragma acc loop vector reduction(+:acc)
	  for (i = 0; i < 32; i++)
	    acc += ina[k * 32 + i] * inb[i * 32 + j];

	  out[k * 32 + j] = acc;
	}
  }

  for (j = 0; j < 32; j++)
    for (i = 0; i < 32; i++)
      assert (out[j * 32 + i] == (i + j) * 2);

  return 0;
}
