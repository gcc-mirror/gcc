/* { dg-do run }  */
/* { dg-set-target-env-var GOMP_RUNTIME_USM "enabled" } */

/* Check that a global static variable is consistently updated on the device.

   With GOMP_RUNTIME_USM=enabled, for devices supporting host access
   (shared memory), self maps are used - however, for 'declare target'
   variables, actual data copying still needs to happen.

   Check that this works.  */

// FIXME: There are known issues with TARGET and TARGET DATA
// The code still makes the wrong cases pass (exit code 0) but still prints an ERROR.

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int A[1] = {1};
#pragma omp declare target enter(A)


int
getA ()
{
  return A[0];
}

void
checkA (int expected, int also_ok, const char *dir)
{
  if (expected != A[0])
    printf ("ERROR: %d (expected: %d) %s running on the %s\n", A[0], expected,
	    dir, omp_is_initial_device () ? "HOST" : "DEVICE");
  if (expected != A[0] && also_ok != A[0])
    __builtin_abort ();
}

void
incA ()
{
  ++A[0];
}


int
main ()
{
  // Debug output
  const char *env_var = getenv ("GOMP_RUNTIME_USM");
  bool is_shared_mem = false;
  #pragma omp target map(to: is_shared_mem)
    is_shared_mem = true;
  bool is_usm = (is_shared_mem && env_var
		 && strcasecmp ("enabled", env_var) == 0
		 && omp_get_num_devices () > 0);
  printf ("DEBUG: GOMP_RUNTIME_USM = %s, is_shared_mem = %s -> usm = %s\n",
	  env_var ? env_var : "<unset>", is_shared_mem ? "true" : "false",
	  is_usm ? "true" : "false");


  // Check that 'TARGET UPDATE' works
  A[0] = 2;
  #pragma omp target update to(A)
  #pragma omp target
    checkA (2, 2, "after TARGET");


  // Check that 'TARGET ENTER DATA' works
  A[0] = 3;
  #pragma omp target enter data map(always,to: A)

  #pragma omp target
  {
    checkA (3, 3, "after TARGET ENTER DATA");
    incA ();
  }

  // Check that 'TARGET EXIT DATA' works
  #pragma omp target exit data map(always,from: A)
  checkA (4, 4, "after TARGET EXIT DATA");

  // FIXME: Early return because of the issues below.
  if (is_usm)
    {
      printf ("SKIPPED: RUNTIME USM does not support TARGET and TARGET DATA and "
	      "fails with a runtime error - see testcase\n");
      return 0;
    }
    
  // The following FIXME expect that the code just works without
  // Aborting - if you comment the gomp_fatal error, it will pass as follows

  // Check that 'TARGET DATA' works
  A[0] = 5;
  #pragma omp target data map(always, tofrom: A)
  {
    #pragma omp target
    {
      // FIXME: Should print 5 - with USM it would print the old device value 4, but currently aborts with:
      // libgomp: Sorry, unimplemented: TARGET DATA with ALWAYS modifier for device static variables when GOMP_RUNTIME_USM is enabled
      checkA (5, !is_usm ? 5 : 4 /* FIXME: old device value */,
	     "in TARGET DATA");
      incA ();
    }
  }
  // FIXME: Should print 6 - with USM prints old host value 5
  checkA (6, !is_usm ? 6 : 5 /* FIXME: old host value */,
	  "after TARGET DATA");

  // Check that 'TARGET' works
  A[0] = 7;
  #pragma omp target map(always,tofrom: A)
  {
    // FIXME: Should print 7 - with USM it would print the old device value 5, but currently aborts with:
    // libgomp: Sorry, unimplemented: TARGET with ALWAYS modifier for device static variables when GOMP_RUNTIME_USM is enabled

    // The following accesses the global static variable
    checkA (7, !is_usm ? 7 : 5 /* FIXME: old device value */,
	    "in TARGET");
    incA ();

    // The following accesses the variable as translated by the TARGET construct
    if ((!is_usm && A[0] != 8) || (is_usm && A[0] != 7 /* FIXME: uses host var, not device var */))
      __builtin_abort ();
    ++A[0];
  }
  checkA (9, !is_usm ? 9 : 8 /* FIXME: old host value, incremented once on the device */,
	  "after TARGET");
}
