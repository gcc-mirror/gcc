#include <assert.h>

int main (void)
{
  int ret;

  #pragma acc parallel num_gangs(1) num_workers(32) copyout(ret)
  {
    int w = 0;

    #pragma acc loop worker
    for (int i = 0; i < 32; i++)
      {
        #pragma acc atomic update
	w++;
      }

    ret = (w == 32);
  }
  assert (ret);

  #pragma acc parallel num_gangs(1) vector_length(32) copyout(ret)
  {
    int v = 0;

    #pragma acc loop vector
    for (int i = 0; i < 32; i++)
      {
        #pragma acc atomic update
	v++;
      }

    ret = (v == 32);
  }
  assert (ret);

  return 0;
}
