/* Ensure that worker-vector state conditional expressions are
   properly handled by the nvptx backend.  */

#include <assert.h>
#include <math.h>


#define N 1024

int A[N][N] ;

void test(int x)
{
#pragma acc parallel  num_gangs(16) num_workers(4) vector_length(32) copyout(A)
  {
#pragma acc loop gang
    for(int j=0;j<N;j++)
      {
	if (x==1)
	  {
#pragma acc loop worker vector
	    for(int i=0;i<N;i++)
	      A[i][j] = 1;
	  }
	else
	  {
#pragma acc loop worker vector
	    for(int i=0;i<N;i++)
	      A[i][j] = -1;
	  }
      }
  }
}


int main(void)
{
  test (0);
  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      assert (A[i][j] == -1);

  test (1);
  for (int i = 0; i < N; i++)
    for (int j = 0; j < N; j++)
      assert (A[i][j] == 1);

  return 0;
}
