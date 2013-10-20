/* Copyright (C) 2005-2013 Free Software Foundation, Inc.
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
#include <stdio.h>
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>	/* For PRIu64.  */
#endif
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
  .thread_limit_var = UINT_MAX,
  .run_sched_var = GFS_DYNAMIC,
  .run_sched_modifier = 1,
  .default_device_var = 0,
  .dyn_var = false,
  .nest_var = false,
  .bind_var = omp_proc_bind_false,
  .target_data = NULL
};

unsigned long gomp_max_active_levels_var = INT_MAX;
bool gomp_cancel_var = false;
#ifndef HAVE_SYNC_BUILTINS
gomp_mutex_t gomp_managed_threads_lock;
#endif
unsigned long gomp_available_cpus = 1, gomp_managed_threads = 1;
unsigned long long gomp_spin_count_var, gomp_throttled_spin_count_var;
unsigned long *gomp_nthreads_var_list, gomp_nthreads_var_list_len;
char *gomp_bind_var_list;
unsigned long gomp_bind_var_list_len;
void **gomp_places_list;
unsigned long gomp_places_list_len;

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

/* Parse a positive int environment variable.  Return true if one was
   present and it was successfully parsed.  */

static bool
parse_int (const char *name, int *pvalue, bool allow_zero)
{
  unsigned long value;
  if (!parse_unsigned_long (name, &value, allow_zero))
    return false;
  if (value > INT_MAX)
    {
      gomp_error ("Invalid value for environment variable %s", name);
      return false;
    }
  *pvalue = (int) value;
  return true;
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

/* Parse environment variable set to a boolean or list of omp_proc_bind_t
   enum values.  Return true if one was present and it was successfully
   parsed.  */

static bool
parse_bind_var (const char *name, char *p1stvalue,
		char **pvalues, unsigned long *pnvalues)
{
  char *env;
  char value = omp_proc_bind_false, *values = NULL;
  int i;
  static struct proc_bind_kinds
  {
    const char name[7];
    const char len;
    omp_proc_bind_t kind;
  } kinds[] =
  {
    { "false", 5, omp_proc_bind_false },
    { "true", 4, omp_proc_bind_true },
    { "master", 6, omp_proc_bind_master },
    { "close", 5, omp_proc_bind_close },
    { "spread", 6, omp_proc_bind_spread }
  };

  env = getenv (name);
  if (env == NULL)
    return false;

  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    goto invalid;

  for (i = 0; i < 5; i++)
    if (strncasecmp (env, kinds[i].name, kinds[i].len) == 0)
      {
	value = kinds[i].kind;
	env += kinds[i].len;
	break;
      }
  if (i == 5)
    goto invalid;

  while (isspace ((unsigned char) *env))
    ++env;
  if (*env != '\0')
    {
      if (*env == ',')
	{
	  unsigned long nvalues = 0, nalloced = 0;

	  if (value == omp_proc_bind_false
	      || value == omp_proc_bind_true)
	    goto invalid;

	  do
	    {
	      env++;
	      if (nvalues == nalloced)
		{
		  char *n;
		  nalloced = nalloced ? nalloced * 2 : 16;
		  n = realloc (values, nalloced);
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

	      for (i = 2; i < 5; i++)
		if (strncasecmp (env, kinds[i].name, kinds[i].len) == 0)
		  {
		    value = kinds[i].kind;
		    env += kinds[i].len;
		    break;
		  }
	      if (i == 5)
		goto invalid;

	      values[nvalues++] = value;
	      while (isspace ((unsigned char) *env))
		++env;
	      if (*env == '\0')
		break;
	      if (*env != ',')
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

static bool
parse_one_place (char **envp, bool *negatep, unsigned long *lenp,
		 long *stridep)
{
  char *env = *envp, *start;
  void *p = gomp_places_list ? gomp_places_list[gomp_places_list_len] : NULL;
  unsigned long len = 1;
  long stride = 1;
  int pass;
  bool any_negate = false;
  *negatep = false;
  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '!')
    {
      *negatep = true;
      ++env;
      while (isspace ((unsigned char) *env))
	++env;
    }
  if (*env != '{')
    return false;
  ++env;
  while (isspace ((unsigned char) *env))
    ++env;
  start = env;
  for (pass = 0; pass < (any_negate ? 2 : 1); pass++)
    {
      env = start;
      do
	{
	  unsigned long this_num, this_len = 1;
	  long this_stride = 1;
	  bool this_negate = (*env == '!');
	  if (this_negate)
	    {
	      if (gomp_places_list)
		any_negate = true;
	      ++env;
	      while (isspace ((unsigned char) *env))
		++env;
	    }

	  errno = 0;
	  this_num = strtoul (env, &env, 10);
	  if (errno)
	    return false;
	  while (isspace ((unsigned char) *env))
	    ++env;
	  if (*env == ':')
	    {
	      ++env;
	      while (isspace ((unsigned char) *env))
		++env;
	      errno = 0;
	      this_len = strtoul (env, &env, 10);
	      if (errno || this_len == 0)
		return false;
	      while (isspace ((unsigned char) *env))
		++env;
	      if (*env == ':')
		{
		  ++env;
		  while (isspace ((unsigned char) *env))
		    ++env;
		  errno = 0;
		  this_stride = strtol (env, &env, 10);
		  if (errno)
		    return false;
		  while (isspace ((unsigned char) *env))
		    ++env;
		}
	    }
	  if (this_negate && this_len != 1)
	    return false;
	  if (gomp_places_list && pass == this_negate)
	    {
	      if (this_negate)
		{
		  if (!gomp_affinity_remove_cpu (p, this_num))
		    return false;
		}
	      else if (!gomp_affinity_add_cpus (p, this_num, this_len,
						this_stride, false))
		return false;
	    }
	  if (*env == '}')
	    break;
	  if (*env != ',')
	    return false;
	  ++env;
	}
      while (1);
    }

  ++env;
  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == ':')
    {
      ++env;
      while (isspace ((unsigned char) *env))
	++env;
      errno = 0;
      len = strtoul (env, &env, 10);
      if (errno || len == 0 || len >= 65536)
	return false;
      while (isspace ((unsigned char) *env))
	++env;
      if (*env == ':')
	{
	  ++env;
	  while (isspace ((unsigned char) *env))
	    ++env;
	  errno = 0;
	  stride = strtol (env, &env, 10);
	  if (errno)
	    return false;
	  while (isspace ((unsigned char) *env))
	    ++env;
	}
    }
  if (*negatep && len != 1)
    return false;
  *envp = env;
  *lenp = len;
  *stridep = stride;
  return true;
}

static bool
parse_places_var (const char *name, bool ignore)
{
  char *env = getenv (name), *end;
  bool any_negate = false;
  int level = 0;
  unsigned long count = 0;
  if (env == NULL)
    return false;

  while (isspace ((unsigned char) *env))
    ++env;
  if (*env == '\0')
    goto invalid;

  if (strncasecmp (env, "threads", 7) == 0)
    {
      env += 7;
      level = 1;
    }
  else if (strncasecmp (env, "cores", 5) == 0)
    {
      env += 5;
      level = 2;
    }
  else if (strncasecmp (env, "sockets", 7) == 0)
    {
      env += 7;
      level = 3;
    }
  if (level)
    {
      count = ULONG_MAX;
      while (isspace ((unsigned char) *env))
	++env;
      if (*env != '\0')
	{
	  if (*env++ != '(')
	    goto invalid;
	  while (isspace ((unsigned char) *env))
	    ++env;

	  errno = 0;
	  count = strtoul (env, &end, 10);
	  if (errno)
	    goto invalid;
	  env = end;
	  while (isspace ((unsigned char) *env))
	    ++env;
	  if (*env != ')')
	    goto invalid;
	  ++env;
	  while (isspace ((unsigned char) *env))
	    ++env;
	  if (*env != '\0')
	    goto invalid;
	}

      if (ignore)
	return false;

      return gomp_affinity_init_level (level, count, false);
    }

  count = 0;
  end = env;
  do
    {
      bool negate;
      unsigned long len;
      long stride;
      if (!parse_one_place (&end, &negate, &len, &stride))
	goto invalid;
      if (negate)
	{
	  if (!any_negate)
	    count++;
	  any_negate = true;
	}
      else
	count += len;
      if (count > 65536)
	goto invalid;
      if (*end == '\0')
	break;
      if (*end != ',')
	goto invalid;
      end++;
    }
  while (1);

  if (ignore)
    return false;

  gomp_places_list_len = 0;
  gomp_places_list = gomp_affinity_alloc (count, false);
  if (gomp_places_list == NULL)
    return false;

  do
    {
      bool negate;
      unsigned long len;
      long stride;
      gomp_affinity_init_place (gomp_places_list[gomp_places_list_len]);
      if (!parse_one_place (&env, &negate, &len, &stride))
	goto invalid;
      if (negate)
	{
	  void *p;
	  for (count = 0; count < gomp_places_list_len; count++)
	    if (gomp_affinity_same_place
			(gomp_places_list[count],
			 gomp_places_list[gomp_places_list_len]))
	      break;
	  if (count == gomp_places_list_len)
	    {
	      gomp_error ("Trying to remove a non-existing place from list "
			  "of places");
	      goto invalid;
	    }
	  p = gomp_places_list[count];
	  memmove (&gomp_places_list[count],
		   &gomp_places_list[count + 1],
		   (gomp_places_list_len - count - 1) * sizeof (void *));
	  --gomp_places_list_len;
	  gomp_places_list[gomp_places_list_len] = p;
	}
      else if (len == 1)
	++gomp_places_list_len;
      else
	{
	  for (count = 0; count < len - 1; count++)
	    if (!gomp_affinity_copy_place
			(gomp_places_list[gomp_places_list_len + count + 1],
			 gomp_places_list[gomp_places_list_len + count],
			 stride))
	      goto invalid;
	  gomp_places_list_len += len;
	}
      if (*env == '\0')
	break;
      env++;
    }
  while (1);

  if (gomp_places_list_len == 0)
    {
      gomp_error ("All places have been removed");
      goto invalid;
    }
  if (!gomp_affinity_finalize_place_list (false))
    goto invalid;
  return true;

 invalid:
  free (gomp_places_list);
  gomp_places_list = NULL;
  gomp_places_list_len = 0;
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
parse_affinity (bool ignore)
{
  char *env, *end, *start;
  int pass;
  unsigned long cpu_beg, cpu_end, cpu_stride;
  size_t count = 0, needed;

  env = getenv ("GOMP_CPU_AFFINITY");
  if (env == NULL)
    return false;

  start = env;
  for (pass = 0; pass < 2; pass++)
    {
      env = start;
      if (pass == 1)
	{
	  if (ignore)
	    return false;

	  gomp_places_list_len = 0;
	  gomp_places_list = gomp_affinity_alloc (count, true);
	  if (gomp_places_list == NULL)
	    return false;
	}
      do
	{
	  while (isspace ((unsigned char) *env))
	    ++env;

	  errno = 0;
	  cpu_beg = strtoul (env, &end, 0);
	  if (errno || cpu_beg >= 65536)
	    goto invalid;
	  cpu_end = cpu_beg;
	  cpu_stride = 1;

	  env = end;
	  if (*env == '-')
	    {
	      errno = 0;
	      cpu_end = strtoul (++env, &end, 0);
	      if (errno || cpu_end >= 65536 || cpu_end < cpu_beg)
		goto invalid;

	      env = end;
	      if (*env == ':')
		{
		  errno = 0;
		  cpu_stride = strtoul (++env, &end, 0);
		  if (errno || cpu_stride == 0 || cpu_stride >= 65536)
		    goto invalid;

		  env = end;
		}
	    }

	  needed = (cpu_end - cpu_beg) / cpu_stride + 1;
	  if (pass == 0)
	    count += needed;
	  else
	    {
	      while (needed--)
		{
		  void *p = gomp_places_list[gomp_places_list_len];
		  gomp_affinity_init_place (p);
		  if (gomp_affinity_add_cpus (p, cpu_beg, 1, 0, true))
		    ++gomp_places_list_len;
		  cpu_beg += cpu_stride;
		}
	    }

	  while (isspace ((unsigned char) *env))
	    ++env;

	  if (*env == ',')
	    env++;
	  else if (*env == '\0')
	    break;
	}
      while (1);
    }

  if (gomp_places_list_len == 0)
    {
      free (gomp_places_list);
      gomp_places_list = NULL;
      return false;
    }
  return true;

 invalid:
  gomp_error ("Invalid value for enviroment variable GOMP_CPU_AFFINITY");
  return false;
}


static void
handle_omp_display_env (unsigned long stacksize, int wait_policy)
{
  const char *env;
  bool display = false;
  bool verbose = false;
  int i;

  env = getenv ("OMP_DISPLAY_ENV");
  if (env == NULL)
    return;

  while (isspace ((unsigned char) *env))
    ++env;
  if (strncasecmp (env, "true", 4) == 0)
    {
      display = true;
      env += 4;
    }
  else if (strncasecmp (env, "false", 5) == 0)
    {
      display = false;
      env += 5;
    }
  else if (strncasecmp (env, "verbose", 7) == 0)
    {
      display = true;
      verbose = true;
      env += 7;
    }
  else
    env = "X";
  while (isspace ((unsigned char) *env))
    ++env;
  if (*env != '\0')
    gomp_error ("Invalid value for environment variable OMP_DISPLAY_ENV");

  if (!display)
    return;

  fputs ("\nOPENMP DISPLAY ENVIRONMENT BEGIN\n", stderr);

  fputs ("  _OPENMP = '201307'\n", stderr);
  fprintf (stderr, "  OMP_DYNAMIC = '%s'\n",
	   gomp_global_icv.dyn_var ? "TRUE" : "FALSE");
  fprintf (stderr, "  OMP_NESTED = '%s'\n",
	   gomp_global_icv.nest_var ? "TRUE" : "FALSE");

  fprintf (stderr, "  OMP_NUM_THREADS = '%lu", gomp_global_icv.nthreads_var);
  for (i = 1; i < gomp_nthreads_var_list_len; i++)
    fprintf (stderr, ",%lu", gomp_nthreads_var_list[i]);
  fputs ("'\n", stderr);

  fprintf (stderr, "  OMP_SCHEDULE = '");
  switch (gomp_global_icv.run_sched_var)
    {
    case GFS_RUNTIME:
      fputs ("RUNTIME", stderr);
      break;
    case GFS_STATIC:
      fputs ("STATIC", stderr);
      break;
    case GFS_DYNAMIC:
      fputs ("DYNAMIC", stderr);
      break;
    case GFS_GUIDED:
      fputs ("GUIDED", stderr);
      break;
    case GFS_AUTO:
      fputs ("AUTO", stderr);
      break;
    }
  fputs ("'\n", stderr);

  fputs ("  OMP_PROC_BIND = '", stderr);
  switch (gomp_global_icv.bind_var)
    {
    case omp_proc_bind_false:
      fputs ("FALSE", stderr);
      break;
    case omp_proc_bind_true:
      fputs ("TRUE", stderr);
      break;
    case omp_proc_bind_master:
      fputs ("MASTER", stderr);
      break;
    case omp_proc_bind_close:
      fputs ("CLOSE", stderr);
      break;
    case omp_proc_bind_spread:
      fputs ("SPREAD", stderr);
      break;
    }
  for (i = 1; i < gomp_bind_var_list_len; i++)
    switch (gomp_bind_var_list[i])
      {
      case omp_proc_bind_master:
	fputs (",MASTER", stderr);
	break;
      case omp_proc_bind_close:
	fputs (",CLOSE", stderr);
	break;
      case omp_proc_bind_spread:
	fputs (",SPREAD", stderr);
	break;
      }
  fputs ("'\n", stderr);
  fputs ("  OMP_PLACES = '", stderr);
  for (i = 0; i < gomp_places_list_len; i++)
    {
      fputs ("{", stderr);
      gomp_affinity_print_place (gomp_places_list[i]);
      fputs (i + 1 == gomp_places_list_len ? "}" : "},", stderr);
    }
  fputs ("'\n", stderr);

  fprintf (stderr, "  OMP_STACKSIZE = '%lu'\n", stacksize);

  /* GOMP's default value is actually neither active nor passive.  */
  fprintf (stderr, "  OMP_WAIT_POLICY = '%s'\n",
	   wait_policy > 0 ? "ACTIVE" : "PASSIVE");
  fprintf (stderr, "  OMP_THREAD_LIMIT = '%u'\n",
	   gomp_global_icv.thread_limit_var);
  fprintf (stderr, "  OMP_MAX_ACTIVE_LEVELS = '%lu'\n",
	   gomp_max_active_levels_var);

  fprintf (stderr, "  OMP_CANCELLATION = '%s'\n",
	   gomp_cancel_var ? "TRUE" : "FALSE");
  fprintf (stderr, "  OMP_DEFAULT_DEVICE = '%d'\n",
	   gomp_global_icv.default_device_var);

  if (verbose)
    {
      fputs ("  GOMP_CPU_AFFINITY = ''\n", stderr);
      fprintf (stderr, "  GOMP_STACKSIZE = '%lu'\n", stacksize);
#ifdef HAVE_INTTYPES_H
      fprintf (stderr, "  GOMP_SPINCOUNT = '%"PRIu64"'\n",
	       (uint64_t) gomp_spin_count_var);
#else
      fprintf (stderr, "  GOMP_SPINCOUNT = '%lu'\n",
	       (unsigned long) gomp_spin_count_var);
#endif
    }

  fputs ("OPENMP DISPLAY ENVIRONMENT END\n", stderr);
}


static void __attribute__((constructor))
initialize_env (void)
{
  unsigned long thread_limit_var, stacksize;
  int wait_policy;

  /* Do a compile time check that mkomp_h.pl did good job.  */
  omp_check_defines ();

  parse_schedule ();
  parse_boolean ("OMP_DYNAMIC", &gomp_global_icv.dyn_var);
  parse_boolean ("OMP_NESTED", &gomp_global_icv.nest_var);
  parse_boolean ("OMP_CANCELLATION", &gomp_cancel_var);
  parse_int ("OMP_DEFAULT_DEVICE", &gomp_global_icv.default_device_var, true);
  parse_unsigned_long ("OMP_MAX_ACTIVE_LEVELS", &gomp_max_active_levels_var,
		       true);
  if (parse_unsigned_long ("OMP_THREAD_LIMIT", &thread_limit_var, false))
    {
      gomp_global_icv.thread_limit_var
	= thread_limit_var > INT_MAX ? UINT_MAX : thread_limit_var;
    }
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_init (&gomp_managed_threads_lock);
#endif
  gomp_init_num_threads ();
  gomp_available_cpus = gomp_global_icv.nthreads_var;
  if (!parse_unsigned_long_list ("OMP_NUM_THREADS",
				 &gomp_global_icv.nthreads_var,
				 &gomp_nthreads_var_list,
				 &gomp_nthreads_var_list_len))
    gomp_global_icv.nthreads_var = gomp_available_cpus;
  bool ignore = false;
  if (parse_bind_var ("OMP_PROC_BIND",
		      &gomp_global_icv.bind_var,
		      &gomp_bind_var_list,
		      &gomp_bind_var_list_len)
      && gomp_global_icv.bind_var == omp_proc_bind_false)
    ignore = true;
  /* Make sure OMP_PLACES and GOMP_CPU_AFFINITY env vars are always
     parsed if present in the environment.  If OMP_PROC_BIND was set
     explictly to false, don't populate places list though.  If places
     list was successfully set from OMP_PLACES, only parse but don't process
     GOMP_CPU_AFFINITY.  If OMP_PROC_BIND was not set in the environment,
     default to OMP_PROC_BIND=true if OMP_PLACES or GOMP_CPU_AFFINITY
     was successfully parsed into a places list, otherwise to
     OMP_PROC_BIND=false.  */
  if (parse_places_var ("OMP_PLACES", ignore))
    {
      if (gomp_global_icv.bind_var == omp_proc_bind_false)
	gomp_global_icv.bind_var = true;
      ignore = true;
    }
  if (parse_affinity (ignore))
    {
      if (gomp_global_icv.bind_var == omp_proc_bind_false)
	gomp_global_icv.bind_var = true;
      ignore = true;
    }
  if (gomp_global_icv.bind_var != omp_proc_bind_false)
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

  handle_omp_display_env (stacksize, wait_policy);
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
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->thread_limit_var > INT_MAX ? INT_MAX : icv->thread_limit_var;
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

int
omp_get_cancellation (void)
{
  return gomp_cancel_var;
}

omp_proc_bind_t
omp_get_proc_bind (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->bind_var;
}

void
omp_set_default_device (int device_num)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  icv->default_device_var = device_num >= 0 ? device_num : 0;
}

int
omp_get_default_device (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->default_device_var;
}

int
omp_get_num_devices (void)
{
  return gomp_get_num_devices ();
}

int
omp_get_num_teams (void)
{
  /* Hardcoded to 1 on host, MIC, HSAIL?  Maybe variable on PTX.  */
  return 1;
}

int
omp_get_team_num (void)
{
  /* Hardcoded to 0 on host, MIC, HSAIL?  Maybe variable on PTX.  */
  return 0;
}

int
omp_is_initial_device (void)
{
  /* Hardcoded to 1 on host, should be 0 on MIC, HSAIL, PTX.  */
  return 1;
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
ialias (omp_get_cancellation)
ialias (omp_get_proc_bind)
ialias (omp_set_default_device)
ialias (omp_get_default_device)
ialias (omp_get_num_devices)
ialias (omp_get_num_teams)
ialias (omp_get_team_num)
ialias (omp_is_initial_device)
