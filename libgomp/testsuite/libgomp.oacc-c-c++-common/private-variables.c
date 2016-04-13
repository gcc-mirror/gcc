#include <assert.h>
#include <openacc.h>

typedef struct {
  int x, y;
} vec2;

typedef struct {
  int x, y, z;
  int attr[13];
} vec3_attr;


/* Test of gang-private variables declared in local scope with parallel
   directive.  */

void local_g_1()
{
  int i, arr[32];

  for (i = 0; i < 32; i++)
    arr[i] = 3;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    int x;

    #pragma acc loop gang(static:1)
    for (i = 0; i < 32; i++)
      x = i * 2;

    #pragma acc loop gang(static:1)
    for (i = 0; i < 32; i++)
      {
	if (acc_on_device (acc_device_host))
	  x = i * 2;
	arr[i] += x;
      }
  }

  for (i = 0; i < 32; i++)
    assert (arr[i] == 3 + i * 2);
}


/* Test of worker-private variables declared in a local scope, broadcasting
   to vector-partitioned mode.  Back-to-back worker loops.  */

void local_w_1()
{
  int i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    int x = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }

	#pragma acc loop worker
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    int x = i | j * 5;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared in a local scope, broadcasting
   to vector-partitioned mode.  Successive vector loops.  */

void local_w_2()
{
  int i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    int x = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	    
	    x = i | j * 5;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared in a local scope, broadcasting
   to vector-partitioned mode.  Aggregate worker variable.  */

void local_w_3()
{
  int i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    vec2 pt;
	    
	    pt.x = i ^ j * 3;
	    pt.y = i | j * 5;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt.x * k;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt.y * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared in a local scope, broadcasting
   to vector-partitioned mode.  Addressable worker variable.  */

void local_w_4()
{
  int i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    vec2 pt, *ptp;
	    
	    ptp = &pt;
	    
	    pt.x = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += ptp->x * k;

	    ptp->y = i | j * 5;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt.y * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared in a local scope, broadcasting
   to vector-partitioned mode.  Array worker variable.  */

void local_w_5()
{
  int i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    int pt[2];
	    
	    pt[0] = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt[0] * k;

	    pt[1] = i | j * 5;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt[1] * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of gang-private variables declared on loop directive.  */

void loop_g_1()
{
  int x = 5, i, arr[32];

  for (i = 0; i < 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    #pragma acc loop gang private(x)
    for (i = 0; i < 32; i++)
      {
	x = i * 2;
	arr[i] += x;
      }
  }

  for (i = 0; i < 32; i++)
    assert (arr[i] == i * 3);
}


/* Test of gang-private variables declared on loop directive, with broadcasting
   to partitioned workers.  */

void loop_g_2()
{
  int x = 5, i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    #pragma acc loop gang private(x)
    for (i = 0; i < 32; i++)
      {
	x = i * 2;

	#pragma acc loop worker
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += x;
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i / 32) * 2);
}


/* Test of gang-private variables declared on loop directive, with broadcasting
   to partitioned vectors.  */

void loop_g_3()
{
  int x = 5, i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    #pragma acc loop gang private(x)
    for (i = 0; i < 32; i++)
      {
	x = i * 2;

	#pragma acc loop vector
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += x;
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i / 32) * 2);
}


/* Test of gang-private addressable variable declared on loop directive, with
   broadcasting to partitioned workers.  */

void loop_g_4()
{
  int x = 5, i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    #pragma acc loop gang private(x)
    for (i = 0; i < 32; i++)
      {
        int *p = &x;

	x = i * 2;

	#pragma acc loop worker
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += x;

	(*p)--;
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i / 32) * 2);
}


/* Test of gang-private array variable declared on loop directive, with
   broadcasting to partitioned workers.  */

void loop_g_5()
{
  int x[8], i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    #pragma acc loop gang private(x)
    for (i = 0; i < 32; i++)
      {
        for (int j = 0; j < 8; j++)
	  x[j] = j * 2;

	#pragma acc loop worker
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += x[j % 8];
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i % 8) * 2);
}


/* Test of gang-private aggregate variable declared on loop directive, with
   broadcasting to partitioned workers.  */

void loop_g_6()
{
  int i, arr[32 * 32];
  vec3_attr pt;

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    #pragma acc loop gang private(pt)
    for (i = 0; i < 32; i++)
      {
        pt.x = i;
	pt.y = i * 2;
	pt.z = i * 4;
	pt.attr[5] = i * 6;

	#pragma acc loop worker
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += pt.x + pt.y + pt.z + pt.attr[5];
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i / 32) * 13);
}


/* Test of vector-private variables declared on loop directive.  */

void loop_v_1()
{
  int x, i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker
	for (j = 0; j < 32; j++)
	  {
	    int k;

	    #pragma acc loop vector private(x)
	    for (k = 0; k < 32; k++)
	      {
		x = i ^ j * 3;
		arr[i * 1024 + j * 32 + k] += x * k;
	      }

	    #pragma acc loop vector private(x)
	    for (k = 0; k < 32; k++)
	      {
		x = i | j * 5;
		arr[i * 1024 + j * 32 + k] += x * k;
	      }
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of vector-private variables declared on loop directive. Array type.  */

void loop_v_2()
{
  int pt[2], i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker
	for (j = 0; j < 32; j++)
	  {
	    int k;

	    #pragma acc loop vector private(pt)
	    for (k = 0; k < 32; k++)
	      {
	        pt[0] = i ^ j * 3;
		pt[1] = i | j * 5;
		arr[i * 1024 + j * 32 + k] += pt[0] * k;
		arr[i * 1024 + j * 32 + k] += pt[1] * k;
	      }
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared on a loop directive.  */

void loop_w_1()
{
  int x = 5, i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker private(x)
	for (j = 0; j < 32; j++)
	  {
	    x = i ^ j * 3;
	    /* Try to ensure 'x' accesses doesn't get optimized into a
	       temporary.  */
	    __asm__ __volatile__ ("");
	    arr[i * 32 + j] += x;
	  }
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + ((i / 32) ^ (i % 32) * 3));
}


/* Test of worker-private variables declared on a loop directive, broadcasting
   to vector-partitioned mode.  */

void loop_w_2()
{
  int x = 5, i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker private(x)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    x = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k);
	}
}


/* Test of worker-private variables declared on a loop directive, broadcasting
   to vector-partitioned mode.  Back-to-back worker loops.  */

void loop_w_3()
{
  int x = 5, i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker private(x)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    x = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }

	#pragma acc loop worker private(x)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    x = i | j * 5;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared on a loop directive, broadcasting
   to vector-partitioned mode.  Successive vector loops.  */

void loop_w_4()
{
  int x = 5, i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker private(x)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    x = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	    
	    x = i | j * 5;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared on a loop directive, broadcasting
   to vector-partitioned mode.  Addressable worker variable.  */

void loop_w_5()
{
  int x = 5, i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker private(x)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    int *p = &x;
	    
	    x = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	    
	    *p = i | j * 5;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared on a loop directive, broadcasting
   to vector-partitioned mode.  Aggregate worker variable.  */

void loop_w_6()
{
  int i, arr[32 * 32 * 32];
  vec2 pt;

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker private(pt)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    
	    pt.x = i ^ j * 3;
	    pt.y = i | j * 5;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt.x * k;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt.y * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of worker-private variables declared on loop directive, broadcasting
   to vector-partitioned mode.  Array worker variable.  */

void loop_w_7()
{
  int i, arr[32 * 32 * 32];
  int pt[2];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  /* "pt" is treated as "present_or_copy" on the parallel directive because it
     is an array variable.  */
  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        /* But here, it is made private per-worker.  */
        #pragma acc loop worker private(pt)
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    
	    pt[0] = i ^ j * 3;

	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt[0] * k;

	    pt[1] = i | j * 5;
	    
	    #pragma acc loop vector
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += pt[1] * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}
}


/* Test of gang-private variables declared on the parallel directive.  */

void parallel_g_1()
{
  int x = 5, i, arr[32];

  for (i = 0; i < 32; i++)
    arr[i] = 3;

  #pragma acc parallel private(x) copy(arr) num_gangs(32) num_workers(8) vector_length(32)
  {
    #pragma acc loop gang(static:1)
    for (i = 0; i < 32; i++)
      x = i * 2;

    #pragma acc loop gang(static:1)
    for (i = 0; i < 32; i++)
      {
	if (acc_on_device (acc_device_host))
	  x = i * 2;
	arr[i] += x;
      }
  }

  for (i = 0; i < 32; i++)
    assert (arr[i] == 3 + i * 2);
}


/* Test of gang-private array variable declared on the parallel directive.  */

void parallel_g_2()
{
  int x[32], i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel private(x) copy(arr) num_gangs(32) num_workers(2) vector_length(32)
  {
    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        int j;
	for (j = 0; j < 32; j++)
	  x[j] = j * 2;
	
	#pragma acc loop worker
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += x[31 - j];
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (31 - (i % 32)) * 2);
}


int main ()
{
  local_g_1();
  local_w_1();
  local_w_2();
  local_w_3();
  local_w_4();
  local_w_5();
  loop_g_1();
  loop_g_2();
  loop_g_3();
  loop_g_4();
  loop_g_5();
  loop_g_6();
  loop_v_1();
  loop_v_2();
  loop_w_1();
  loop_w_2();
  loop_w_3();
  loop_w_4();
  loop_w_5();
  loop_w_6();
  loop_w_7();
  parallel_g_1();
  parallel_g_2();

  return 0;
}
