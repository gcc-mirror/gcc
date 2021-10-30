/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int v = 6;

void
bar (long *x, long *y)
{
  *x += 2;
  *y += 3;
}

int
baz (void)
{
  return 5;
}

#pragma omp declare target to (bar, baz, v)

__attribute__((noinline, noclone)) void
foo (int a, int b, long c, long d)
{
  int err;
  if (omp_get_num_teams () != 1)
    abort ();
  /* The OpenMP 4.5 spec says that these expressions are evaluated before
     target region on combined target teams, so those cases are always
     fine.  */
  #pragma omp target map(from: err)
  err = omp_get_num_teams () != 1;
  if (err)
    abort ();
  #pragma omp target map(from: err)
  #pragma omp teams
  {
    err = omp_get_num_teams () < 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1;
  }
  if (err)
    abort ();
  #pragma omp target teams map(from: err)
  {
    err = omp_get_num_teams () < 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1;
  }
  if (err)
    abort ();
  #pragma omp target map(from: err)
  #pragma omp teams num_teams (4)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > 4;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1;
  }
  if (err)
    abort ();
  #pragma omp target teams num_teams (4) map(from: err)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > 4;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1;
  }
  if (err)
    abort ();
  #pragma omp target map(from: err)
  #pragma omp teams thread_limit (7)
  {
    err = omp_get_num_teams () < 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > 7;
  }
  if (err)
    abort ();
  #pragma omp target teams thread_limit (7) map(from: err)
  {
    err = omp_get_num_teams () < 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > 7;
  }
  if (err)
    abort ();
  #pragma omp target map(from: err)
  #pragma omp teams num_teams (4) thread_limit (8)
  {
    {
      err = omp_get_num_teams () < 1 || omp_get_num_teams () > 4;
    }
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > 8;
  }
  if (err)
    abort ();
  #pragma omp target teams num_teams (4) thread_limit (8) map(from: err)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > 4;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > 8;
  }
  if (err)
    abort ();
  #pragma omp target map(from: err)
  #pragma omp teams num_teams (a) thread_limit (b)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > a;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > b;
  }
  if (err)
    abort ();
  #pragma omp target teams num_teams (a) thread_limit (b) map(from: err)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > a;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > b;
  }
  if (err)
    abort ();
  #pragma omp target map(from: err)
  #pragma omp teams num_teams (c + 1) thread_limit (d - 1)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > c + 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > d - 1;
  }
  if (err)
    abort ();
  #pragma omp target teams num_teams (c + 1) thread_limit (d - 1) map(from: err)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > c + 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > d - 1;
  }
  if (err)
    abort ();
  #pragma omp target map (always, to: c, d) map(from: err)
  #pragma omp teams num_teams (c + 1) thread_limit (d - 1)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > c + 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > d - 1;
  }
  if (err)
    abort ();
  #pragma omp target data map (to: c, d)
  {
    #pragma omp target defaultmap (tofrom: scalar)
    bar (&c, &d);
    /* This is one of the cases which can't be generally optimized,
       the c and d are (or could be) already mapped and whether
       their device and original values match is unclear.  */
    #pragma omp target map (to: c, d) map(from: err)
    #pragma omp teams num_teams (c + 1) thread_limit (d - 1)
    {
      err = omp_get_num_teams () < 1 || omp_get_num_teams () > c + 1;
      #pragma omp parallel if(0)
      err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > d - 1;
    }
    if (err)
      abort ();
  }
  /* This can't be optimized, there are function calls inside of
     target involved.  */
  #pragma omp target map(from: err)
  #pragma omp teams num_teams (baz () + 1) thread_limit (baz () - 1)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > baz () + 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > baz () - 1;
  }
  if (err)
    abort ();
  #pragma omp target teams num_teams (baz () + 1) thread_limit (baz () - 1) map(from: err)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > baz () + 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > baz () - 1;
  }
  if (err)
    abort ();
  /* This one can't be optimized, as v might have different value between
     host and target.  */
  #pragma omp target map(from: err)
  #pragma omp teams num_teams (v + 1) thread_limit (v - 1)
  {
    err = omp_get_num_teams () < 1 || omp_get_num_teams () > v + 1;
    #pragma omp parallel if(0)
    err |= omp_get_thread_limit () < 1 || omp_get_thread_limit () > v - 1;
  }
  if (err)
    abort ();
}

int
main ()
{
  foo (3, 5, 7, 9);
  return 0;
}
