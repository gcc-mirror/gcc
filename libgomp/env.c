/* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU OpenMP Library (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file defines the OpenMP internal control variables, and arranges
   for them to be initialized from environment variables at startup.  */

#include "libgomp.h"
#include "libgomp_f.h"
#include <ctype.h>
#include <stdlib.h>
#ifdef STRING_WITH_STRINGS
# include <string.h>
# include <strings.h>
#else
# ifdef HAVE_STRING_H
#  include <string.h>
# else
#  ifdef HAVE_STRINGS_H
#   include <strings.h>
#  endif
# endif
#endif
#include <limits.h>
#include <errno.h>

#ifndef HAVE_STRTOULL
# define strtoull(ptr, eptr, base) strtoul (ptr, eptr, base)
#endif

struct gomp_task_icv gomp_global_icv = {
  .nthreads_var = 1,
  .run_sched_var = GFS_DYNAMIC,
  .run_sched_modifier = 1,
  .dyn_var = false,
  .nest_var = false
};

unsigned short *gomp_cpu_affinity;
size_t gomp_cpu_affinity_len;
unsigned long gomp_max_active_levels_var = INT_MAX;
unsigned long gomp_thread_limit_var = ULONG_MAX;
unsigned long gomp_remaining_threads_count;
#ifndef HAVE_SYNC_BUILTINS
gomp_mutex_t gomp_remaining_threads_lock;
#endif
unsigned long gomp_available_cpus = 1, gomp_managed_threads = 1;
unsigned long long gomp_spin_count_var, gomp_throttled_spin_count_var;
unsigned long *gomp_nthreads_var_list, gomp_nthreads_var_list_len;

/* Parse the OMP_SCHEDULE environment variable.  */

static void
parse_schedule (void)
{
  char *env, *end;
  unsigned long value;

  env = getenv ("OMP_SCHEDULE");
  if (env == NULL)
    return;

  while (isspace ((unsigned char) *env))
    ++env;
  if (strncasecmp (env, "static", 6) == 0)
    {
      gomp_global_icv.run_sched_var = GFS_STATIC;
      env += 6;
    }
  else if (strncasecmp (env, "dynamic", 7) == 0)
    {
      gomp_global_icv.run_sched_var = GFS_DYNAMIC;
      env += 7;
    }
  else if (strncasecmp (env, "guided", 6) == 0)
    {
      gomp_global_icv.run_sched_var = GFS_GUIDED;
      env += 6;
    }
  else if (strncasecmp (env, "auto", 4) == 0)
    {
      gomp_global_icv.run_sched_var = GFS_AUTO;
      env += 4;
    }
  else
    goto unknown;

  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    {
      gomp_global_icv.run_sched_modifier
	= gomp_global_icv.run_sched_var != GFS_STATIC;
      return;
    }
  if (*env++ != ',')
    goto unknown;
  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    goto invalid;

  errno = 0;
  value = strtoul (env, &end, 10);
  if (errno)
    goto invalid;

  while (isspace ((unsigned char) *end))
    ++end;
  if (*end != '\0')
    goto invalid;

  if ((int)value != value)
    goto invalid;

  if (value == 0 && gomp_global_icv.run_sched_var != GFS_STATIC)
    value = 1;
  gomp_global_icv.run_sched_modifier = value;
  return;

 unknown:
  gomp_error ("Unknown value for environment variable OMP_SCHEDULE");
  return;

 invalid:
  gomp_error ("Invalid value for chunk size in "
	      "environment variable OMP_SCHEDULE");
  return;
}

/* Parse an unsigned long environment variable.  Return true if one was
   present and it was successfully parsed.  */

static bool
parse_unsigned_long (const char *name, unsigned long *pvalue, bool allow_zero)
{
  char *env, *end;
  unsigned long value;

  env = getenv (name);
  if (env == NULL)
    return false;

  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    goto invalid;

  errno = 0;
  value = strtoul (env, &end, 10);
  if (errno || (long) value <= 0 - allow_zero)
    goto invalid;

  while (isspace ((unsigned char) *end))
    ++end;
  if (*end != '\0')
    goto invalid;

  *pvalue = value;
  return true;

 invalid:
  gomp_error ("Invalid value for environment variable %s", name);
  return false;
}

/* Parse an unsigned long list environment variable.  Return true if one was
   present and it was successfully parsed.  */

static bool
parse_unsigned_long_list (const char *name, unsigned long *p1stvalue,
			  unsigned long **pvalues,
			  unsigned long *pnvalues)
{
  char *env, *end;
  unsigned long value, *values = NULL;

  env = getenv (name);
  if (env == NULL)
    return false;

  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    goto invalid;

  errno = 0;
  value = strtoul (env, &end, 10);
  if (errno || (long) value <= 0)
    goto invalid;

  while (isspace ((unsigned char) *end))
    ++end;
  if (*end != '\0')
    {
      if (*end == ',')
	{
	  unsigned long nvalues = 0, nalloced = 0;

	  do
	    {
	      env = end + 1;
	      if (nvalues == nalloced)
		{
		  unsigned long *n;
		  nalloced = nalloced ? nalloced * 2 : 16;
		  n = realloc (values, nalloced * sizeof (unsigned long));
		  if (n == NULL)
		    {
		      free (values);
		      gomp_error ("Out of memory while trying to parse"
				  " environment variable %s", name);
		      return false;
		    }
		  values = n;
		  if (nvalues == 0)
		    values[nvalues++] = value;
		}

	      while (isspace ((unsigned char) *env))
		++env;
	      if (*env == '\0')
		goto invalid;

	      errno = 0;
	      value = strtoul (env, &end, 10);
	      if (errno || (long) value <= 0)
		goto invalid;

	      values[nvalues++] = value;
	      while (isspace ((unsigned char) *end))
		++end;
	      if (*end == '\0')
		break;
	      if (*end != ',')
		goto invalid;
	    }
	  while (1);
	  *p1stvalue = values[0];
	  *pvalues = values;
	  *pnvalues = nvalues;
	  return true;
	}
      goto invalid;
    }

  *p1stvalue = value;
  return true;

 invalid:
  free (values);
  gomp_error ("Invalid value for environment variable %s", name);
  return false;
}

/* Parse the OMP_STACKSIZE environment varible.  Return true if one was
   present and it was successfully parsed.  */

static bool
parse_stacksize (const char *name, unsigned long *pvalue)
{
  char *env, *end;
  unsigned long value, shift = 10;

  env = getenv (name);
  if (env == NULL)
    return false;

  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    goto invalid;

  errno = 0;
  value = strtoul (env, &end, 10);
  if (errno)
    goto invalid;

  while (isspace ((unsigned char) *end))
    ++end;
  if (*end != '\0')
    {
      switch (tolower ((unsigned char) *end))
	{
	case 'b':
	  shift = 0;
	  break;
	case 'k':
	  break;
	case 'm':
	  shift = 20;
	  break;
	case 'g':
	  shift = 30;
	  break;
	default:
	  goto invalid;
	}
      ++end;
      while (isspace ((unsigned char) *end))
	++end;
      if (*end != '\0')
	goto invalid;
    }

  if (((value << shift) >> shift) != value)
    goto invalid;

  *pvalue = value << shift;
  return true;

 invalid:
  gomp_error ("Invalid value for environment variable %s", name);
  return false;
}

/* Parse the GOMP_SPINCOUNT environment varible.  Return true if one was
   present and it was successfully parsed.  */

static bool
parse_spincount (const char *name, unsigned long long *pvalue)
{
  char *env, *end;
  unsigned long long value, mult = 1;

  env = getenv (name);
  if (env == NULL)
    return false;

  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    goto invalid;

  if (strncasecmp (env, "infinite", 8) == 0
      || strncasecmp (env, "infinity", 8) == 0)
    {
      value = ~0ULL;
      end = env + 8;
      goto check_tail;
    }

  errno = 0;
  value = strtoull (env, &end, 10);
  if (errno)
    goto invalid;

  while (isspace ((unsigned char) *end))
    ++end;
  if (*end != '\0')
    {
      switch (tolower ((unsigned char) *end))
	{
	case 'k':
	  mult = 1000LL;
	  break;
	case 'm':
	  mult = 1000LL * 1000LL;
	  break;
	case 'g':
	  mult = 1000LL * 1000LL * 1000LL;
	  break;
	case 't':
	  mult = 1000LL * 1000LL * 1000LL * 1000LL;
	  break;
	default:
	  goto invalid;
	}
      ++end;
     check_tail:
      while (isspace ((unsigned char) *end))
	++end;
      if (*end != '\0')
	goto invalid;
    }

  if (value > ~0ULL / mult)
    value = ~0ULL;
  else
    value *= mult;

  *pvalue = value;
  return true;

 invalid:
  gomp_error ("Invalid value for environment variable %s", name);
  return false;
}

/* Parse a boolean value for environment variable NAME and store the
   result in VALUE.  */

static void
parse_boolean (const char *name, bool *value)
{
  const char *env;

  env = getenv (name);
  if (env == NULL)
    return;

  while (isspace ((unsigned char) *env))
    ++env;
  if (strncasecmp (env, "true", 4) == 0)
    {
      *value = true;
      env += 4;
    }
  else if (strncasecmp (env, "false", 5) == 0)
    {
      *value = false;
      env += 5;
    }
  else
    env = "X";
  while (isspace ((unsigned char) *env))
    ++env;
  if (*env != '\0')
    gomp_error ("Invalid value for environment variable %s", name);
}

/* Parse the OMP_WAIT_POLICY environment variable and store the
   result in gomp_active_wait_policy.  */

static int
parse_wait_policy (void)
{
  const char *env;
  int ret = -1;

  env = getenv ("OMP_WAIT_POLICY");
  if (env == NULL)
    return -1;

  while (isspace ((unsigned char) *env))
    ++env;
  if (strncasecmp (env, "active", 6) == 0)
    {
      ret = 1;
      env += 6;
    }
  else if (strncasecmp (env, "passive", 7) == 0)
    {
      ret = 0;
      env += 7;
    }
  else
    env = "X";
  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    return ret;
  gomp_error ("Invalid value for environment variable OMP_WAIT_POLICY");
  return -1;
}

/* Parse the GOMP_CPU_AFFINITY environment varible.  Return true if one was
   present and it was successfully parsed.  */

static bool
parse_affinity (void)
{
  char *env, *end;
  unsigned long cpu_beg, cpu_end, cpu_stride;
  unsigned short *cpus = NULL;
  size_t allocated = 0, used = 0, needed;

  env = getenv ("GOMP_CPU_AFFINITY");
  if (env == NULL)
    return false;

  do
    {
      while (*env == ' ' || *env == '\t')
	env++;

      cpu_beg = strtoul (env, &end, 0);
      cpu_end = cpu_beg;
      cpu_stride = 1;
      if (env == end || cpu_beg >= 65536)
	goto invalid;

      env = end;
      if (*env == '-')
	{
	  cpu_end = strtoul (++env, &end, 0);
	  if (env == end || cpu_end >= 65536 || cpu_end < cpu_beg)
	    goto invalid;

	  env = end;
	  if (*env == ':')
	    {
	      cpu_stride = strtoul (++env, &end, 0);
	      if (env == end || cpu_stride == 0 || cpu_stride >= 65536)
		goto invalid;

	      env = end;
	    }
	}

      needed = (cpu_end - cpu_beg) / cpu_stride + 1;
      if (used + needed >= allocated)
	{
	  unsigned short *new_cpus;

	  if (allocated < 64)
	    allocated = 64;
	  if (allocated > needed)
	    allocated <<= 1;
	  else
	    allocated += 2 * needed;
	  new_cpus = realloc (cpus, allocated * sizeof (unsigned short));
	  if (new_cpus == NULL)
	    {
	      free (cpus);
	      gomp_error ("not enough memory to store GOMP_CPU_AFFINITY list");
	      return false;
	    }

	  cpus = new_cpus;
	}

      while (needed--)
	{
	  cpus[used++] = cpu_beg;
	  cpu_beg += cpu_stride;
	}

      while (*env == ' ' || *env == '\t')
	env++;

      if (*env == ',')
	env++;
      else if (*env == '\0')
	break;
    }
  while (1);

  gomp_cpu_affinity = cpus;
  gomp_cpu_affinity_len = used;
  return true;

 invalid:
  gomp_error ("Invalid value for enviroment variable GOMP_CPU_AFFINITY");
  return false;
}

static void __attribute__((constructor))
initialize_env (void)
{
  unsigned long stacksize;
  int wait_policy;
  bool bind_var = false;

  /* Do a compile time check that mkomp_h.pl did good job.  */
  omp_check_defines ();

  parse_schedule ();
  parse_boolean ("OMP_DYNAMIC", &gomp_global_icv.dyn_var);
  parse_boolean ("OMP_NESTED", &gomp_global_icv.nest_var);
  parse_boolean ("OMP_PROC_BIND", &bind_var);
  parse_unsigned_long ("OMP_MAX_ACTIVE_LEVELS", &gomp_max_active_levels_var,
		       true);
  parse_unsigned_long ("OMP_THREAD_LIMIT", &gomp_thread_limit_var, false);
  if (gomp_thread_limit_var != ULONG_MAX)
    gomp_remaining_threads_count = gomp_thread_limit_var - 1;
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_init (&gomp_remaining_threads_lock);
#endif
  gomp_init_num_threads ();
  gomp_available_cpus = gomp_global_icv.nthreads_var;
  if (!parse_unsigned_long_list ("OMP_NUM_THREADS",
				 &gomp_global_icv.nthreads_var,
				 &gomp_nthreads_var_list,
				 &gomp_nthreads_var_list_len))
    gomp_global_icv.nthreads_var = gomp_available_cpus;
  if (parse_affinity () || bind_var)
    gomp_init_affinity ();
  wait_policy = parse_wait_policy ();
  if (!parse_spincount ("GOMP_SPINCOUNT", &gomp_spin_count_var))
    {
      /* Using a rough estimation of 100000 spins per msec,
	 use 5 min blocking for OMP_WAIT_POLICY=active,
	 3 msec blocking when OMP_WAIT_POLICY is not specificed
	 and 0 when OMP_WAIT_POLICY=passive.
	 Depending on the CPU speed, this can be e.g. 5 times longer
	 or 5 times shorter.  */
      if (wait_policy > 0)
	gomp_spin_count_var = 30000000000LL;
      else if (wait_policy < 0)
	gomp_spin_count_var = 300000LL;
    }
  /* gomp_throttled_spin_count_var is used when there are more libgomp
     managed threads than available CPUs.  Use very short spinning.  */
  if (wait_policy > 0)
    gomp_throttled_spin_count_var = 1000LL;
  else if (wait_policy < 0)
    gomp_throttled_spin_count_var = 100LL;
  if (gomp_throttled_spin_count_var > gomp_spin_count_var)
    gomp_throttled_spin_count_var = gomp_spin_count_var;

  /* Not strictly environment related, but ordering constructors is tricky.  */
  pthread_attr_init (&gomp_thread_attr);
  pthread_attr_setdetachstate (&gomp_thread_attr, PTHREAD_CREATE_DETACHED);

  if (parse_stacksize ("OMP_STACKSIZE", &stacksize)
      || parse_stacksize ("GOMP_STACKSIZE", &stacksize))
    {
      int err;

      err = pthread_attr_setstacksize (&gomp_thread_attr, stacksize);

#ifdef PTHREAD_STACK_MIN
      if (err == EINVAL)
	{
	  if (stacksize < PTHREAD_STACK_MIN)
	    gomp_error ("Stack size less than minimum of %luk",
			PTHREAD_STACK_MIN / 1024ul
			+ (PTHREAD_STACK_MIN % 1024 != 0));
	  else
	    gomp_error ("Stack size larger than system limit");
	}
      else
#endif
      if (err != 0)
	gomp_error ("Stack size change failed: %s", strerror (err));
    }
}


/* The public OpenMP API routines that access these variables.  */

void
omp_set_num_threads (int n)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  icv->nthreads_var = (n > 0 ? n : 1);
}

void
omp_set_dynamic (int val)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  icv->dyn_var = val;
}

int
omp_get_dynamic (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->dyn_var;
}

void
omp_set_nested (int val)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  icv->nest_var = val;
}

int
omp_get_nested (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->nest_var;
}

void
omp_set_schedule (omp_sched_t kind, int modifier)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  switch (kind)
    {
    case omp_sched_static:
      if (modifier < 1)
	modifier = 0;
      icv->run_sched_modifier = modifier;
      break;
    case omp_sched_dynamic:
    case omp_sched_guided:
      if (modifier < 1)
	modifier = 1;
      icv->run_sched_modifier = modifier;
      break;
    case omp_sched_auto:
      break;
    default:
      return;
    }
  icv->run_sched_var = kind;
}

void
omp_get_schedule (omp_sched_t *kind, int *modifier)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  *kind = icv->run_sched_var;
  *modifier = icv->run_sched_modifier;
}

int
omp_get_max_threads (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->nthreads_var;
}

int
omp_get_thread_limit (void)
{
  return gomp_thread_limit_var > INT_MAX ? INT_MAX : gomp_thread_limit_var;
}

void
omp_set_max_active_levels (int max_levels)
{
  if (max_levels >= 0)
    gomp_max_active_levels_var = max_levels;
}

int
omp_get_max_active_levels (void)
{
  return gomp_max_active_levels_var;
}

ialias (omp_set_dynamic)
ialias (omp_set_nested)
ialias (omp_set_num_threads)
ialias (omp_get_dynamic)
ialias (omp_get_nested)
ialias (omp_set_schedule)
ialias (omp_get_schedule)
ialias (omp_get_max_threads)
ialias (omp_get_thread_limit)
ialias (omp_set_max_active_levels)
ialias (omp_get_max_active_levels)
