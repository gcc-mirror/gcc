#include <assert.h>

/* Worker propagation: plain scalar variables.  */

void
worker_bcast_1 (void)
{
  int i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32)
  {
    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
	int j;
	int x = (i ^ 3) * 3;

	#pragma acc loop worker
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += x * j;

	x = (i | 5) * 5;

	#pragma acc loop worker
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += x * j;
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      {
	int idx = i * 32 + j;
	assert (arr[idx] == idx + (i ^ 3) * 3 * j + (i | 5) * 5 * j);
      }
}

#pragma acc routine seq
__attribute__((noinline)) static int
select_var (int s, int x, int y)
{
  if (s)
    return x;
  else
    return y;
}

/* Worker propagation: scalars through function calls.  */

void
worker_bcast_2 (void)
{
  int i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
	int x, y, z;

	x = i * 5;
	y = i * 7;

	#pragma acc loop worker
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += select_var (j & 1, x, y) * j;

	#pragma acc loop worker vector
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += select_var (j & 1, y, x) * j;
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      {
	int idx = i * 32 + j;
	int answer = idx + ((j & 1) ? i * 5 : i * 7) * j
		     + ((j & 1) ? i * 7 : i * 5) * j;
        assert (arr[idx] == answer);
      }
}

#pragma acc routine seq
__attribute__((noinline)) static int
select_addr (int s, int *x, int *y)
{
  if (s)
    return *x;
  else
    return *y;
}

/* Worker propagation: addresses of locals through function calls.  */

void
worker_bcast_3 (void)
{
  int i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32) vector_length(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
        int x, y, z;

	x = i * 5;
	y = i * 7;

	#pragma acc loop worker
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += select_addr (j & 1, &x, &y) * j;

	#pragma acc loop worker vector
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += select_addr (j & 1, &y, &x) * j;
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      {
	int idx = i * 32 + j;
	int answer = idx + ((j & 1) ? i * 5 : i * 7) * j
		     + ((j & 1) ? i * 7 : i * 5) * j;
	assert (arr[idx] == answer);
      }
}

#pragma acc routine seq
__attribute__((noinline)) static int *
select_ptr (int s, int *x, int *y)
{
  if (s)
    return x;
  else
    return y;
}

/* Worker propagation: writes through pointers.  */

void
worker_bcast_4 (void)
{
  int i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc parallel copy(arr) num_gangs(32) num_workers(32)
  {
    int j;

    #pragma acc loop gang
    for (i = 0; i < 32; i++)
      {
	int x, y, z;
	int *p, *q, *r;

	p = &x;
	q = &y;

	x = i * 5;
	y = i * 7;
	r = select_ptr (i & 1, p, q);

	#pragma acc loop worker
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += (x + y + 2 * (*p) + (*q)) * j;

	/* This write can affect either x or y: both should be broadcast into
	   the next loop.  */
	(*r) += 20;

	#pragma acc loop worker
	for (j = 0; j < 32; j++)
	  arr[i * 32 + j] += (x + y + 2 * (*p) + (*q)) * j;
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      {
	int idx = i * 32 + j;
	int x = i * 5, y = i * 7;
	int answer = idx + (3 * x + 2 * y) * j
		     + ((i & 1) ? (3 * (x + 20) + 2 * y)
				: (3 * x + 2 * (y + 20))) * j;
	assert (arr[idx] == answer);
      }
}


int main ()
{
  worker_bcast_1 ();
  worker_bcast_2 ();
  worker_bcast_3 ();
  worker_bcast_4 ();

  return 0;
}
