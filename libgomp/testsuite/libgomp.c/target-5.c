/* { dg-additional-options "-Wno-deprecated-declarations" } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int d_o = omp_get_dynamic ();
  int n_o = omp_get_nested ();
  omp_sched_t s_o;
  int c_o;
  omp_get_schedule (&s_o, &c_o);
  int m_o = omp_get_max_threads ();
  omp_set_dynamic (1);
  omp_set_nested (1);
  omp_set_schedule (omp_sched_static, 2);
  omp_set_num_threads (4);
  int d = omp_get_dynamic ();
  int n = omp_get_nested ();
  omp_sched_t s;
  int c;
  omp_get_schedule (&s, &c);
  int m = omp_get_max_threads ();
  if (!omp_is_initial_device ())
    abort ();
  #pragma omp target if (0)
  {
    omp_sched_t s_c;
    int c_c;
    omp_get_schedule (&s_c, &c_c);
    if (d_o != omp_get_dynamic ()
	|| n_o != omp_get_nested ()
	|| s_o != s_c
	|| c_o != c_c
	|| m_o != omp_get_max_threads ())
      abort ();
    omp_set_dynamic (0);
    omp_set_nested (0);
    omp_set_schedule (omp_sched_dynamic, 4);
    omp_set_num_threads (2);
    if (!omp_is_initial_device ())
      abort ();
  }
  if (!omp_is_initial_device ())
    abort ();
  omp_sched_t s_c;
  int c_c;
  omp_get_schedule (&s_c, &c_c);
  if (d != omp_get_dynamic ()
      || n != omp_get_nested ()
      || s != s_c
      || c != c_c
      || m != omp_get_max_threads ())
    abort ();
  #pragma omp target if (0)
  #pragma omp teams
  {
    omp_sched_t s_c;
    int c_c;
    omp_get_schedule (&s_c, &c_c);
    if (d_o != omp_get_dynamic ()
	|| n_o != omp_get_nested ()
	|| s_o != s_c
	|| c_o != c_c
	|| m_o != omp_get_max_threads ())
      abort ();
    omp_set_dynamic (0);
    omp_set_nested (0);
    omp_set_schedule (omp_sched_dynamic, 4);
    omp_set_num_threads (2);
    if (!omp_is_initial_device ())
      abort ();
  }
  if (!omp_is_initial_device ())
    abort ();
  omp_get_schedule (&s_c, &c_c);
  if (d != omp_get_dynamic ()
      || n != omp_get_nested ()
      || s != s_c
      || c != c_c
      || m != omp_get_max_threads ())
    abort ();
  return 0;
}
