/* { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
   aspects of that functionality.  */

#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

#define NUM_WORKERS 16
#ifdef ACC_DEVICE_TYPE_radeon
/* AMD GCN uses the autovectorizer for the vector dimension: the use
   of a function call in vector-partitioned code in this test is not
   currently supported.  */
#define NUM_VECTORS 1
#else
#define NUM_VECTORS 32
#endif
#define WIDTH 64
#define HEIGHT 32

#define WORK_ID(I,N)						\
  (acc_on_device (acc_device_not_host)				\
   ? __builtin_goacc_parlevel_id (GOMP_DIM_WORKER)				\
   : (I % N))
#define VEC_ID(I,N)						\
  (acc_on_device (acc_device_not_host)				\
   ? __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR)				\
   : (I % N))

#pragma acc routine worker
void __attribute__ ((noinline))
  WorkVec (int *ptr, int w, int h, int nw, int nv)
{
#pragma acc loop worker
  for (int i = 0; i < h; i++)
#pragma acc loop vector
    for (int j = 0; j < w; j++)
      ptr[i*w + j] = (WORK_ID (i, nw) << 8) | VEC_ID(j, nv);
}

int DoWorkVec (int nw)
{
  int ary[HEIGHT][WIDTH];
  int err = 0;

  for (int ix = 0; ix != HEIGHT; ix++)
    for (int jx = 0; jx != WIDTH; jx++)
      ary[ix][jx] = 0xdeadbeef;

  printf ("spawning %d ...", nw); fflush (stdout);
  
#pragma acc parallel num_workers(nw) vector_length (NUM_VECTORS) copy (ary)
  /* { dg-warning "region contains vector partitioned code but is not vector partitioned" "" { target openacc_radeon_accel_selected } .-1 } */
  {
    WorkVec ((int *)ary, WIDTH, HEIGHT, nw, NUM_VECTORS);
  }

  for (int ix = 0; ix != HEIGHT; ix++)
    for (int jx = 0; jx != WIDTH; jx++)
      {
	int exp = ((ix % nw) << 8) | (jx % NUM_VECTORS);
	
	if (ary[ix][jx] != exp)
	  {
	    printf ("\nary[%d][%d] = %#x expected %#x", ix, jx,
		    ary[ix][jx], exp);
	    err = 1;
	  }
      }
  printf (err ? " failed\n" : " ok\n");
  
  return err;
}

int main ()
{
  int err = 0;

  for (int W = 1; W <= NUM_WORKERS; W <<= 1)
    err |= DoWorkVec (W);

  return err;
}
