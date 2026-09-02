/* { dg-do run }  */
/* { dg-set-target-env-var GOMP_RUNTIME_USM "enabled" } */

/* Check that a global static variable is consistently updated on the device.

   This file checks that 'TARGET' data is handled properly when there is no
   ALWAYS clause, i.e. that the device consistently accesses the device data
   and not mixing host and device data.

   Actually, this is handled already by the compiler as such variables are
   not replaced by a reference to the passed argument but directly access
   the global variable. - But still useful to check to confirm that his works.  */

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef USE_SELF_MAPS
#pragma omp requires self_maps
  constexpr bool req_self_maps = true;
#else
  constexpr bool req_self_maps = false;
#endif

int a = 9;
int B[10] = {};
struct S {
  int x,y;
};
struct S s = { .x = 1, .y = 2};
struct S t = { .x = 3, .y = 4};
#pragma omp declare target enter(a, B, s, t)

int c[1] = {};
int d = 0;
#pragma omp declare target enter(c, d)


void check_c_d_incr ()
{
  if (c[0] != 42 + 3)
    __builtin_abort ();
  if (d != 27 + 4)
    __builtin_abort ();
  c[0] += 7;
  d += 5;
}

void check_vals_plus_inc3 ()
{
  bool add_one = req_self_maps || omp_is_initial_device ();
  if (a != 9 + 21 + (int)add_one)
    __builtin_abort ();
  for (int i = 0; i < 3; i++)
    if (B[i] != 0 + 21 * i + (i == 3 && add_one))
  if (s.x != 1 + 21 + (int)add_one)
    __builtin_abort ();
  if (s.y != 2 + 21 + (int)add_one)
    __builtin_abort ();
  if (t.x != 3 + 21 + (int)add_one)
    __builtin_abort ();
  if (t.y != 4 + 21 + (int)add_one)
    __builtin_abort ();

  a += 3;
  for (int i = 0; i < 3; i++)
    B[i] += 3 * i;
  s.x += 3;
  s.y += 3;
  t.x += 3;
  t.y += 3;
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


  // Target update + modifying the data in TARGET

  c[0] = 42;
  d = 27;
  #pragma omp target update to(c, d)

  #pragma omp target map(present, alloc: d)
  {
    c[0] += 3;
    d += 4;
  }

  #pragma omp target
    check_c_d_incr();

  #pragma omp target update from(c, d)
  if (c[0] != 42 + 3 + 7)
    __builtin_abort ();
  if (d != 27 + 4 + 5)
    __builtin_abort ();

  // Some more tests involving structs and similar

  // Those use the initial value - unless the following
  // affects the device value.
  // That is only the case with 'requires self_maps' or
  // when the device is the host (fallback)

  a++;
  B[3]++;
  s.x++;
  s.y++;
  t.x++;
  t.y++;

  #pragma omp target map(from : a) /* to prevent firstprivate - no op as infinite ref count */
  {
    a += 21;
    for (int i = 0; i < 10; i++)
      B[i] += 21*i;
    s.x += 21;
    s.y += 21;
    t.x += 21;
    t.y += 21;
  }

  #pragma omp target
    check_vals_plus_inc3 ();

  // Get the values from the device
  #pragma omp target exit data map(always, from: a, s.x, B[0:5])
  #pragma omp target update from(s.y, B[5:5], t)

  bool add_one = req_self_maps || omp_get_num_devices () == 0;
  if (a != 9 + 21 + 3 + (int)add_one)
    __builtin_abort ();
  for (int i = 0; i < 3; i++)
    if (B[i] != 0 + (21 + 3) * i + (i == 3 && add_one))
  if (s.x != 1 + 21 + 3 + (int)add_one)
    __builtin_abort ();
  if (s.y != 2 + 21 + 3 + (int)add_one)
    __builtin_abort ();
  if (t.x != 3 + 21 + 3 + (int)add_one)
    __builtin_abort ();
  if (t.y != 4 + 21 + 3 + (int)add_one)
    __builtin_abort ();
}
