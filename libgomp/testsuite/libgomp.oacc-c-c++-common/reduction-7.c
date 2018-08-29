/* Tests of reduction on loop directive.  */

#include <assert.h>


/* Test of reduction on loop directive (gangs, non-private reduction
   variable).  */

void g_np_1()
{
  int i, arr[1024], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32)
  {
    #pragma acc loop gang reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  assert (res == hres);

  res = hres = 1;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32)
  {
    #pragma acc loop gang reduction(*:res)
    for (i = 0; i < 12; i++)
      res *= arr[i];
  }

  for (i = 0; i < 12; i++)
    hres *= arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs and vectors, non-private
   reduction variable).  */

void gv_np_1()
{
  int i, arr[1024], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32)
  {
    #pragma acc loop gang vector reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs and workers, non-private
   reduction variable).  */

void gw_np_1()
{
  int i, arr[1024], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32)
  {
    #pragma acc loop gang worker reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs, workers and vectors, non-private
   reduction variable).  */

void gwv_np_1()
{
  int i, arr[1024], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32)
  {
    #pragma acc loop gang worker vector reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs, workers and vectors, non-private
   reduction variable: separate gang and worker/vector loops).  */

void gwv_np_2()
{
  int i, j, arr[32768], res = 0, hres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32)
  {
    #pragma acc loop gang reduction(+:res)
    for (j = 0; j < 32; j++)
      {
        #pragma acc loop worker vector reduction(+:res)
        for (i = 0; i < 1024; i++)
	  res += arr[j * 1024 + i];
      }
    /* "res" is non-private, and is not available until after the parallel
       region.  */
  }

  for (i = 0; i < 32768; i++)
    hres += arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs, workers and vectors, non-private
   reduction variable: separate gang and worker/vector loops).  */

void gwv_np_3()
{
  int i, j;
  double arr[32768], res = 0, hres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       copyin(arr)
  {
    #pragma acc loop gang reduction(+:res)
    for (j = 0; j < 32; j++)
      {
        #pragma acc loop worker vector reduction(+:res)
        for (i = 0; i < 1024; i++)
	  res += arr[j * 1024 + i];
      }
  }

  for (i = 0; i < 32768; i++)
    hres += arr[i];

  assert (res == hres);
}


/* Test of reduction on loop directive (gangs, workers and vectors, multiple
   non-private reduction variables, float type).  */

void gwv_np_4()
{
  int i, j;
  float arr[32768];
  float res = 0, mres = 0, hres = 0, hmres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i % (32768 / 64);

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32)
  {
    #pragma acc loop gang reduction(+:res) reduction(max:mres)
    for (j = 0; j < 32; j++)
      {
	#pragma acc loop worker vector reduction(+:res) reduction(max:mres)
	for (i = 0; i < 1024; i++)
	  {
	    res += arr[j * 1024 + i];
	    if (arr[j * 1024 + i] > mres)
	      mres = arr[j * 1024 + i];
	  }

	#pragma acc loop worker vector reduction(+:res) reduction(max:mres)
	for (i = 0; i < 1024; i++)
	  {
	    res += arr[j * 1024 + (1023 - i)];
	    if (arr[j * 1024 + (1023 - i)] > mres)
	      mres = arr[j * 1024 + (1023 - i)];
	  }
      }
  }

  for (j = 0; j < 32; j++)
    for (i = 0; i < 1024; i++)
      {
        hres += arr[j * 1024 + i];
	hres += arr[j * 1024 + (1023 - i)];
	if (arr[j * 1024 + i] > hmres)
	  hmres = arr[j * 1024 + i];
	if (arr[j * 1024 + (1023 - i)] > hmres)
	  hmres = arr[j * 1024 + (1023 - i)];
      }

  assert (hres <= 16777216);
  assert (res == hres);

  assert (hmres <= 16777216);
  assert (mres == hmres);
}


/* Test of reduction on loop directive (vectors, private reduction
   variable).  */

void v_p_1()
{
  int i, j, arr[1024], out[32], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(res) copyout(out)
  {
    #pragma acc loop gang
    for (j = 0; j < 32; j++)
      {
        res = 0;

	#pragma acc loop vector reduction(+:res)
	for (i = 0; i < 32; i++)
	  res += arr[j * 32 + i];

	out[j] = res;
      }
  }

  for (j = 0; j < 32; j++)
    {
      hres = 0;

      for (i = 0; i < 32; i++)
	hres += arr[j * 32 + i];

      assert (out[j] == hres);
    }
}


/* Test of reduction on loop directive (vector reduction in
   gang-partitioned/worker-partitioned mode, private reduction variable).  */

void v_p_2()
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
}


/* Test of reduction on loop directive (workers, private reduction
   variable).  */

void w_p_1()
{
  int i, j, arr[1024], out[32], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(res) copyout(out)
  {
    #pragma acc loop gang
    for (j = 0; j < 32; j++)
      {
        res = 0;

	#pragma acc loop worker reduction(+:res)
	for (i = 0; i < 32; i++)
	  res += arr[j * 32 + i];

	out[j] = res;
      }
  }

  for (j = 0; j < 32; j++)
    {
      hres = 0;

      for (i = 0; i < 32; i++)
	hres += arr[j * 32 + i];

      assert (out[j] == hres);
    }
}


/* Test of reduction on loop directive (workers and vectors, private reduction
   variable).  */

void wv_p_1()
{
  int i, j, arr[1024], out[32], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(res) copyout(out)
  {
    #pragma acc loop gang
    for (j = 0; j < 32; j++)
      {
        res = 0;

	#pragma acc loop worker vector reduction(+:res)
	for (i = 0; i < 32; i++)
	  res += arr[j * 32 + i];

	out[j] = res;
      }
  }

  for (j = 0; j < 32; j++)
    {
      hres = 0;

      for (i = 0; i < 32; i++)
	hres += arr[j * 32 + i];

      assert (out[j] == hres);
    }
}


/* Test of reduction on loop directive (workers and vectors, private reduction
   variable).  */

void wv_p_2()
{
  int i, j, arr[32768], out[32], res = 0, hres = 0;

  for (i = 0; i < 32768; i++)
    arr[i] = i;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(res) copyout(out)
  {
    #pragma acc loop gang
    for (j = 0; j < 32; j++)
      {
        res = j;

	#pragma acc loop worker reduction(+:res)
	for (i = 0; i < 1024; i++)
	  res += arr[j * 1024 + i];

	#pragma acc loop vector reduction(+:res)
	for (i = 1023; i >= 0; i--)
	  res += arr[j * 1024 + i];

	out[j] = res;
      }
  }

  for (j = 0; j < 32; j++)
    {
      hres = j;

      for (i = 0; i < 1024; i++)
	hres += arr[j * 1024 + i] * 2;

      assert (out[j] == hres);
    }
}


/* Test of reduction on loop directive (workers and vectors, private reduction
   variable: gang-redundant mode).  */

void wv_p_3()
{
  int i, arr[1024], out[32], res = 0, hres = 0;

  for (i = 0; i < 1024; i++)
    arr[i] = i ^ 33;

  #pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
		       private(res) copyin(arr) copyout(out)
  {
    /* Private variables aren't initialized by default in openacc.  */
    res = 0;

    /* "res" should be available at the end of the following loop (and should
       have the same value redundantly in each gang).  */
    #pragma acc loop worker vector reduction(+:res)
    for (i = 0; i < 1024; i++)
      res += arr[i];

    #pragma acc loop gang (static: 1)
    for (i = 0; i < 32; i++)
      out[i] = res;
  }

  for (i = 0; i < 1024; i++)
    hres += arr[i];

  for (i = 0; i < 32; i++)
    assert (out[i] == hres);
}


int main()
{
  g_np_1();
  gv_np_1();
  gw_np_1();
  gwv_np_1();
  gwv_np_2();
  gwv_np_3();
  gwv_np_4();
  v_p_1();
  v_p_2();
  w_p_1();
  wv_p_1();
  wv_p_2();
  wv_p_3();

  return 0;
}
