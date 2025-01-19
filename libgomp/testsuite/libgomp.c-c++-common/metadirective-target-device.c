#include <omp.h>
#include <stdlib.h>
#include <stdio.h>

/* PR112779 item (B)  */

/* Check that the target_device selector correctly matches device numbers
   and handles kind=host|nohost|any.  */

static int
check_explicit_device (int d, int expect_host)
{
  int ok = 0;
  if (expect_host)
    {
      #pragma omp metadirective \
	when (target_device={device_num(d), kind("host")} : nothing)	\
	otherwise (error at(execution) message("check_explicit_device host"))
      ok = 1;
    }
  else
    {
      #pragma omp metadirective \
	when (target_device={device_num(d), kind("nohost")} : nothing)	\
	otherwise (error at(execution) message("check_explicit_device nohost"))
      ok = 1;
    }

  return ok;
}

static int
check_implicit_device (int d, int expect_host)
{
  int ok = 0;
  omp_set_default_device (d);

  if (expect_host)
    {
      #pragma omp metadirective \
	when (target_device={kind("host")} : nothing)	\
	otherwise (error at(execution) message("check_implicit_device host"))
      ok = 1;
    }
  else
    {
      #pragma omp metadirective \
	when (target_device={kind("nohost")} : nothing)	\
	otherwise (error at(execution) message("check_implicit_device nohost"))
      ok = 1;
    }
  #pragma omp metadirective						\
    when (target_device={kind("any")} : nothing)	\
    otherwise (error at(execution) message("check_implicit_device any"))
  ok = 1;
  omp_set_default_device (omp_initial_device);

  return ok;
}

int
main (void)
{
  printf ("Checking omp_initial_device\n");
  check_explicit_device (omp_initial_device, 1);
  check_implicit_device (omp_initial_device, 1);
  int n = omp_get_num_devices ();
  printf ("There are %d devices\n", n);
  for (int i = 0; i < n; i++)
    {
      printf ("Checking device %d\n", i);
      check_explicit_device (i, 0);
      check_implicit_device (i, 0);
    }
  return 0;
}
