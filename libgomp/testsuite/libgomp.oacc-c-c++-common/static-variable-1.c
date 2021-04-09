/* "Function scope" (top-level block scope) 'static' variables

   ... inside OpenACC compute construct regions as well as OpenACC 'routine'.

   This is to document/verify aspects of GCC's observed behavior, not
   necessarily as it's (intended to be?) restricted by the OpenACC
   specification.  See also PR84991, PR84992, PR90779 etc., and
   <https://github.com/OpenACC/openacc-spec/issues/372> "C/C++ 'static'
   variables" (only visible to members of the GitHub OpenACC organization).
*/


#undef NDEBUG
#include <assert.h>
#include <string.h>
#include <openacc.h>
#include <gomp-constants.h>


#define IF_DEBUG if (0)


/* Without explicit 'num_gangs'.  */

static void t0_c(void)
{
  IF_DEBUG
    __builtin_printf ("%s\n", __FUNCTION__);

  const int i_limit = 11;
  const int var_init = 16;

  for (int i = 0; i < i_limit; ++i)
    {
      int result = 0;
      int num_gangs_actual = -1;
#pragma acc parallel \
  reduction(max:num_gangs_actual) \
  reduction(max:result)
      {
	num_gangs_actual = 1 + __builtin_goacc_parlevel_id(GOMP_DIM_GANG);

	static int var = var_init;

#pragma acc atomic capture
	result = ++var;

	/* Irrespective of the order in which the gang-redundant threads
	   execute, 'var' has now been incremented 'num_gangs_actual' times, and
	   the final value captured as 'result'.  */
      }
      /* Without an explicit 'num_gangs' clause GCC assigns 'num_gangs(1)'
	 because it doesn't see any use of gang-level parallelism inside the
	 region.  */
      assert(num_gangs_actual == 1);
      assert(result == var_init + num_gangs_actual * (1 + i));
    }
}


/* Call a gang-level routine.  */

static const int t0_r_var_init = 61;

#pragma acc routine gang
__attribute__((noinline))
static int t0_r_r(void)
{
  static int var = t0_r_var_init;

  int tmp;
#pragma acc atomic capture
  tmp = ++var;

  return tmp;
}

static void t0_r(void)
{
  IF_DEBUG
    __builtin_printf ("%s\n", __FUNCTION__);

  const int i_limit = 11;

  for (int i = 0; i < i_limit; ++i)
    {
      int result = 0;
      int num_gangs_actual = -1;
#pragma acc parallel \
  reduction(max:num_gangs_actual) \
  reduction(max:result)
      {
	num_gangs_actual = 1 + __builtin_goacc_parlevel_id(GOMP_DIM_GANG);

	result = t0_r_r();

	/* Irrespective of the order in which the gang-redundant threads
	   execute, 'var' has now been incremented 'num_gangs_actual' times, and
	   the final value captured as 'result'.  */
      }
      /* The number of gangs selected by the implemention ought to but must not
	 be bigger than one.  */
      IF_DEBUG
	__builtin_printf ("%d: num_gangs_actual: %d\n", i, num_gangs_actual);
      assert(num_gangs_actual >= 1);
      assert(result == t0_r_var_init + num_gangs_actual * (1 + i));
    }
}


/* Explicit 'num_gangs'.  */

static void t1_c(void)
{
  IF_DEBUG
    __builtin_printf ("%s\n", __FUNCTION__);

  const int i_limit = 22;
  const int num_gangs_request = 444;
  const int var_init = 5;

  for (int i = 0; i < i_limit; ++i)
    {
      int result = 0;
      int num_gangs_actual = -1;
#pragma acc parallel \
  num_gangs(num_gangs_request) \
  reduction(max:num_gangs_actual) \
  reduction(max:result)
      {
	num_gangs_actual = 1 + __builtin_goacc_parlevel_id(GOMP_DIM_GANG);

	static int var = var_init;

#pragma acc atomic capture
	result = ++var;

	/* Irrespective of the order in which the gang-redundant threads
	   execute, 'var' has now been incremented 'num_gangs_actual' times, and
	   the final value captured as 'result'.  */
      }
      if (acc_get_device_type() == acc_device_host)
	assert(num_gangs_actual == 1);
      else
	assert(num_gangs_actual == num_gangs_request);
      assert(result == var_init + num_gangs_actual * (1 + i));
    }
}


/* Check the same routine called from two compute constructs.  */

static const int t1_r2_var_init = 166;

#pragma acc routine gang
__attribute__((noinline))
static int t1_r2_r(void)
{
  static int var = t1_r2_var_init;

  int tmp;
#pragma acc atomic capture
  tmp = ++var;

  return tmp;
}

static void t1_r2(void)
{
  IF_DEBUG
    __builtin_printf ("%s\n", __FUNCTION__);

  const int i_limit = 71;
  /* The checking assumes the same 'num_gangs' for all compute constructs.  */
  const int num_gangs_request = 333;
  int num_gangs_actual = -1;
  if (acc_get_device_type() == acc_device_host)
    num_gangs_actual = 1;
  else
    {
      /* We're assuming that the implementation is able to accomodate the
	 'num_gangs' requested (which really ought to be true for
	 'num_gangs').  */
      num_gangs_actual = num_gangs_request;
    }

  for (int i = 0; i < i_limit; ++i)
    {
      int result_1 = 0;
#pragma acc parallel \
  num_gangs(num_gangs_request) \
  reduction(max:result_1)
      {
	result_1 = t1_r2_r();

	/* Irrespective of the order in which the gang-redundant threads
	   execute, 'var' has now been incremented 'num_gangs_actual' times, and
	   the final value captured as 'result_1'.  */
      }
      IF_DEBUG
	__builtin_printf ("%d: result_1: %d\n", i, result_1);
      assert(result_1 == t1_r2_var_init + num_gangs_actual * (1 + (i * 3 + 0)));

      int result_2 = 0;
#pragma acc parallel \
  num_gangs(num_gangs_request) \
  reduction(max:result_2)
      {
	result_2 = t1_r2_r() + t1_r2_r();

	/* Irrespective of the order in which the gang-redundant threads
	   execute, 'var' has now been incremented '2 * num_gangs_actual' times.
	   However, the order of the two 't1_r2_r' function calls is not
	   synchronized (between different gang-redundant threads).  We thus
	   cannot verify the actual 'result_2' values in this case.  */
      }
      IF_DEBUG
	__builtin_printf ("%d: result_2: %d\n", i, result_2);
      if (num_gangs_actual == 1)
	/* Per the rationale above, only in this case we can check the actual
	   result.  */
	assert(result_2 == (t1_r2_var_init + num_gangs_actual * (1 + (i * 3 + 1))
			    + t1_r2_var_init + num_gangs_actual * (1 + (i * 3 + 2))));
      /* But we can generally check low and high limits.  */
      {
	/* Must be bigger than '2 * result_1'.  */
	int c = 2 * result_1;
	IF_DEBUG
	  __builtin_printf ("  > %d\n", c);
	assert(result_2 > c);
      }
      {
	/* ..., but limited by the base value for next 'i'.  */
	int c = 2 * (t1_r2_var_init + num_gangs_actual * (0 + ((i + 1) * 3 + 0)));
	IF_DEBUG
	  __builtin_printf ("  < %d\n", c);
	assert(result_2 < c);
      }
    }
}


/* Asynchronous execution.  */

static const int t2_var_init_2 = -55;

#pragma acc routine gang
__attribute__((noinline))
static int t2_r(void)
{
  static int var = t2_var_init_2;

  int tmp;
#pragma acc atomic capture
  tmp = ++var;

  return tmp;
}

static void t2(void)
{
  IF_DEBUG
    __builtin_printf ("%s\n", __FUNCTION__);

  const int i_limit = 12;
  const int num_gangs_request_1 = 14;
  const int var_init_1 = 5;
  int results_1[i_limit][num_gangs_request_1];
  memset (results_1, 0, sizeof results_1);
  const int num_gangs_request_2 = 5;
  int results_2[i_limit][num_gangs_request_2];
  memset (results_2, 0, sizeof results_2);
  const int num_gangs_request_3 = 34;
  const int var_init_3 = 1250;
  int results_3[i_limit][num_gangs_request_3];
  memset (results_3, 0, sizeof results_3);

#pragma acc data \
  copy(results_1, results_2, results_3)
  {
    for (int i = 0; i < i_limit; ++i)
      {
	/* The following 'async' clauses effect asynchronous execution, but
	   using the same async-argument for each compute construct implies that
	   the respective compute constructs' execution is synchronized with
	   itself, meaning that all 'i = 0' execution has finished (on the
	   device) before 'i = 1' is started (on the device), etc.  */

#pragma acc parallel \
  present(results_1) \
  num_gangs(num_gangs_request_1) \
  async(1)
	{
	  static int var = var_init_1;

	  int tmp;
#pragma acc atomic capture
	  tmp = ++var;

	  results_1[i][__builtin_goacc_parlevel_id(GOMP_DIM_GANG)] += tmp;
	}

#pragma acc parallel \
  present(results_2) \
  num_gangs(num_gangs_request_2) \
  async(2)
	{
	  results_2[i][__builtin_goacc_parlevel_id(GOMP_DIM_GANG)] += t2_r();
	}

#pragma acc parallel \
  present(results_3) \
  num_gangs(num_gangs_request_3) \
  async(3)
	{
	  static int var = var_init_3;

	  int tmp;
#pragma acc atomic capture
	  tmp = ++var;

	  results_3[i][__builtin_goacc_parlevel_id(GOMP_DIM_GANG)] += tmp;
	}
      }
#pragma acc wait
  }
  int num_gangs_actual_1;
  int num_gangs_actual_2;
  int num_gangs_actual_3;
  if (acc_get_device_type() == acc_device_host)
    {
      num_gangs_actual_1 = 1;
      num_gangs_actual_2 = 1;
      num_gangs_actual_3 = 1;
    }
  else
    {
      /* We're assuming that the implementation is able to accomodate the
	 'num_gangs' requested (which really ought to be true for
	 'num_gangs').  */
      num_gangs_actual_1 = num_gangs_request_1;
      num_gangs_actual_2 = num_gangs_request_2;
      num_gangs_actual_3 = num_gangs_request_3;
    }

  /* For 'i = 0', 'results_*[i][0..num_gangs_actual_*]' are expected to each
     contain one value of '(1 + var_init_*)..(var_init_* + num_gangs_actual_*)',
     and so on for increasing 'i'.  Their order however is unspecified due to
     the gang-redundant execution.  (Thus checking that their sums match.)  */

  int result_1 = 0;
  int result_2 = 0;
  int result_3 = 0;
  for (int i = 0; i < i_limit; ++i)
    {
      int result_1_ = 0;
      for (int g = 0; g < num_gangs_actual_1; ++g)
	{
	  IF_DEBUG
	    __builtin_printf ("results_1[%d][%d]: %d\n", i, g, results_1[i][g]);
	  result_1_ += results_1[i][g];
	}
      IF_DEBUG
	__builtin_printf ("%d result_1_: %d\n", i, result_1_);
      assert (result_1_ == (((var_init_1 + num_gangs_actual_1 * (1 + i)) * (1 + var_init_1 + num_gangs_actual_1 * (1 + i)) / 2)
			    - ((var_init_1 + num_gangs_actual_1 * (0 + i)) * (1 + var_init_1 + num_gangs_actual_1 * (0 + i)) / 2)));
      result_1 += result_1_;

      int result_2_ = 0;
      for (int g = 0; g < num_gangs_actual_2; ++g)
	{
	  IF_DEBUG
	    __builtin_printf ("results_2[%d][%d]: %d\n", i, g, results_2[i][g]);
	  result_2_ += results_2[i][g];
	}
      IF_DEBUG
	__builtin_printf ("%d result_2_: %d\n", i, result_2_);
      assert (result_2_ == (((t2_var_init_2 + num_gangs_actual_2 * (1 + i)) * (1 + t2_var_init_2 + num_gangs_actual_2 * (1 + i)) / 2)
			    - ((t2_var_init_2 + num_gangs_actual_2 * (0 + i)) * (1 + t2_var_init_2 + num_gangs_actual_2 * (0 + i)) / 2)));
      result_2 += result_2_;

      int result_3_ = 0;
      for (int g = 0; g < num_gangs_actual_3; ++g)
	{
	  IF_DEBUG
	    __builtin_printf ("results_3[%d][%d]: %d\n", i, g, results_3[i][g]);
	  result_3_ += results_3[i][g];
	}
      IF_DEBUG
	__builtin_printf ("%d result_3_: %d\n", i, result_3_);
      assert (result_3_ == (((var_init_3 + num_gangs_actual_3 * (1 + i)) * (1 + var_init_3 + num_gangs_actual_3 * (1 + i)) / 2)
			    - ((var_init_3 + num_gangs_actual_3 * (0 + i)) * (1 + var_init_3 + num_gangs_actual_3 * (0 + i)) / 2)));
      result_3 += result_3_;
    }
  IF_DEBUG
    __builtin_printf ("result_1: %d\n", result_1);
  assert (result_1 == (((var_init_1 + num_gangs_actual_1 * i_limit) * (1 + var_init_1 + num_gangs_actual_1 * i_limit) / 2)
		       - (var_init_1 * (var_init_1 + 1) / 2)));
  IF_DEBUG
    __builtin_printf ("result_2: %d\n", result_2);
  assert (result_2 == (((t2_var_init_2 + num_gangs_actual_2 * i_limit) * (1 + t2_var_init_2 + num_gangs_actual_2 * i_limit) / 2)
		       - (t2_var_init_2 * (t2_var_init_2 + 1) / 2)));
  IF_DEBUG
    __builtin_printf ("result_3: %d\n", result_3);
  assert (result_3 == (((var_init_3 + num_gangs_actual_3 * i_limit) * (1 + var_init_3 + num_gangs_actual_3 * i_limit) / 2)
		       - (var_init_3 * (var_init_3 + 1) / 2)));
}


#pragma acc routine seq
__attribute__((noinline))
static int pr84991_1_r_s(int n)
{
  static const int test[] = {1,2,3,4};
  return test[n];
}

static void pr84991_1(void)
{
  int n[1];
  n[0] = 3;
#pragma acc parallel copy(n)
  {
    n[0] = pr84991_1_r_s(n[0]);
  }
  assert(n[0] == 4);
}


static void pr84992_1(void)
{
  int n[1];
  n[0] = 3;
#pragma acc parallel copy(n)
  {
    static const int test[] = {1,2,3,4};
    n[0] = test[n[0]];
  }
  assert(n[0] == 4);
}


int main(void)
{
  t0_c();

  t0_r();

  t1_c();

  t1_r2();

  t2();

  pr84991_1();

  pr84992_1();

  return 0;
}
