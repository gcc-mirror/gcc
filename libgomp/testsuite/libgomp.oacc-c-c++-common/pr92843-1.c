/* Verify that 'acc_copyout' etc. is a no-op if there's still a structured
   reference count.  */

/* { dg-xfail-run-if "TODO PR92843" { *-*-* } } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <stdlib.h>
#include <openacc.h>


const int c0 = 58;
const int c1 = 81;

static void
assign_array (char *array, size_t size, char value)
{
  for (size_t i = 0; i < size; ++i)
    array[i] = value;
}

static void
verify_array (const char *array, size_t size, char value)
{
  for (size_t i = 0; i < size; ++i)
    assert (array[i] == value);
}


float global_var;
#pragma acc declare create (global_var)

static void
test_acc_declare ()
{
  assert (acc_is_present (&global_var, sizeof global_var));

  global_var = c0;
#pragma acc update device (global_var)

  global_var = c1;
  acc_copyout (&global_var, sizeof global_var);
  assert (acc_is_present (&global_var, sizeof global_var));
  assert (global_var == c1);

  global_var = c1;
  acc_copyout_finalize (&global_var, sizeof global_var);
  assert (acc_is_present (&global_var, sizeof global_var));
  assert (global_var == c1);

  void *global_var_d_p = acc_deviceptr (&global_var);
  assert (global_var_d_p);

  void *d_p = acc_copyin (&global_var, sizeof global_var);
  assert (d_p == global_var_d_p);

  acc_copyout (&global_var, sizeof global_var);
  assert (acc_is_present (&global_var, sizeof global_var));

  d_p = acc_copyin (&global_var, sizeof global_var);
  assert (d_p == global_var_d_p);

  d_p = acc_copyin (&global_var, sizeof global_var);
  assert (d_p == global_var_d_p);

  global_var = c1;
  acc_copyout_finalize (&global_var, sizeof global_var);
  assert (acc_is_present (&global_var, sizeof global_var));
  assert (global_var == c1);

  global_var = c1;
  acc_copyout (&global_var, sizeof global_var);
  assert (acc_is_present (&global_var, sizeof global_var));
  assert (global_var == c1);
}


static void
test_acc_map_data ()
{
  const int N = 801;

  char *h = (char *) malloc (N);
  assert (h);
  void *d = acc_malloc (N);
  assert (d);
  acc_map_data (h, d, N);
  assert (acc_is_present (h, N));

  assign_array (h, N, c0);
#pragma acc update device (h[0:N])

  assign_array (h, N, c1);
#pragma acc exit data copyout (h[0:N])
  assert (acc_is_present (h, N));
  verify_array (h, N, c1);

  assign_array (h, N, c1);
#pragma acc exit data copyout (h[0:N]) finalize
  assert (acc_is_present (h, N));
  verify_array (h, N, c1);

#pragma acc enter data copyin (h[0:N])

  assign_array (h, N, c1);
#pragma acc exit data copyout (h[0:N])
  assert (acc_is_present (h, N));
  verify_array (h, N, c1);

#pragma acc enter data copyin (h[0:N])

#pragma acc enter data copyin (h[0:N])

  assign_array (h, N, c1);
#pragma acc exit data copyout (h[0:N]) finalize
  assert (acc_is_present (h, N));
  verify_array (h, N, c1);

  assign_array (h, N, c1);
#pragma acc exit data copyout (h[0:N])
  assert (acc_is_present (h, N));
  verify_array (h, N, c1);
}


static void
test_acc_data ()
{
#define N 23
  char h[N];

  assign_array (h, N, c0);
#pragma acc data copyin (h)
  {
    assert (acc_is_present (h, sizeof h));

    assign_array (h, N, c1);
    acc_copyout_finalize (h, sizeof h);
    assert (acc_is_present (h, sizeof h));
    verify_array (h, N, c1);

    assign_array (h, N, c1);
    acc_copyout (h, sizeof h);
    assert (acc_is_present (h, sizeof h));
    verify_array (h, N, c1);

    acc_copyin (h, sizeof h);

    assign_array (h, N, c1);
    acc_copyout (h, sizeof h);
    assert (acc_is_present (h, sizeof h));
    verify_array (h, N, c1);

    acc_copyin (h, sizeof h);

    acc_copyin (h, sizeof h);

    assign_array (h, N, c1);
    acc_copyout_finalize (h, sizeof h);
    assert (acc_is_present (h, sizeof h));
    verify_array (h, N, c1);

    assign_array (h, N, c1);
    acc_copyout (h, sizeof h);
    assert (acc_is_present (h, sizeof h));
    verify_array (h, N, c1);
  }
#undef N
}


int
main ()
{
  test_acc_declare ();
  test_acc_map_data ();
  test_acc_data ();

  return 0;
}
