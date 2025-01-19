/* Copyright (C) 2005-2025 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

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

/* This file defines the OpenMP internal control variables and arranges
   for them to be initialized from environment variables at startup.  */

#define _GNU_SOURCE
#include "libgomp.h"
#include "gomp-constants.h"
#include <limits.h>
#ifndef LIBGOMP_OFFLOADED_ONLY
#include "libgomp_f.h"
#include "oacc-int.h"
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
#include <errno.h>
#include "thread-stacksize.h"

#ifndef HAVE_STRTOULL
# define strtoull(ptr, eptr, base) strtoul (ptr, eptr, base)
#endif
#endif /* LIBGOMP_OFFLOADED_ONLY */

#include "secure_getenv.h"
#include "environ.h"
#include "spincount.h"

/* Default values of ICVs according to the OpenMP standard,
   except for default-device-var.  */
const struct gomp_default_icv gomp_default_icv_values = {
  .nthreads_var = 1,
  .thread_limit_var = UINT_MAX,
  .run_sched_var = GFS_DYNAMIC,
  .run_sched_chunk_size = 1,
  .default_device_var = INT_MIN,
  .max_active_levels_var = 1,
  .bind_var = omp_proc_bind_false,
  .nteams_var = 0,
  .teams_thread_limit_var = 0,
  .dyn_var = false
};

struct gomp_task_icv gomp_global_icv = {
  .nthreads_var = gomp_default_icv_values.nthreads_var,
  .thread_limit_var = gomp_default_icv_values.thread_limit_var,
  .run_sched_var = gomp_default_icv_values.run_sched_var,
  .run_sched_chunk_size = gomp_default_icv_values.run_sched_chunk_size,
  .default_device_var = gomp_default_icv_values.default_device_var,
  .dyn_var = gomp_default_icv_values.dyn_var,
  .max_active_levels_var = gomp_default_icv_values.max_active_levels_var,
  .bind_var = gomp_default_icv_values.bind_var,
  .target_data = NULL
};

/* List for initial "_DEV", "_ALL", and "_DEV_X" ICVs like OMP_NUM_TEAMS_DEV,
   OMP_NUM_TEAMS_ALL, or OMP_NUM_TEAMS_DEV_42.  */
struct gomp_icv_list *gomp_initial_icv_list = NULL;

/* List for "_DEV_X" ICVs like OMP_NUM_TEAMS_DEV_42.  This list contains all
   device-specific ICVs that are copied from host to device and back.  */
struct gomp_offload_icv_list *gomp_offload_icv_list = NULL;

bool gomp_cancel_var = false;
enum gomp_target_offload_t gomp_target_offload_var
  = GOMP_TARGET_OFFLOAD_DEFAULT;
int gomp_max_task_priority_var = 0;
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
uintptr_t gomp_def_allocator = omp_default_mem_alloc;
char *gomp_def_allocator_envvar = NULL;
int gomp_debug_var;
unsigned int gomp_num_teams_var;
int gomp_nteams_var;
int gomp_teams_thread_limit_var;
bool gomp_display_affinity_var;
char *gomp_affinity_format_var = "level %L thread %i affinity %A";
size_t gomp_affinity_format_len;
char *goacc_device_type;
int goacc_device_num;
int goacc_default_dims[GOMP_DIM_MAX];

#ifndef LIBGOMP_OFFLOADED_ONLY

static int wait_policy = -1;
static unsigned long stacksize = GOMP_DEFAULT_STACKSIZE;

static void
print_env_var_error (const char *env, const char *val)
{
  gomp_error ("Invalid value for environment variable %.*s: %s",
	      (int) (val - env - 1), env, val);
}

/* Parse the OMP_SCHEDULE environment variable.  */
static bool
parse_schedule (const char *env, const char *val, void *const params[])
{
  enum gomp_schedule_type *schedule = (enum gomp_schedule_type *) params[0];
  int *chunk_size = (int *) params[1];
  char *end;
  unsigned long value;
  int monotonic = 0;

  if (val == NULL)
    return false;

  while (isspace ((unsigned char) *val))
    ++val;
  if (strncasecmp (val, "monotonic", 9) == 0)
    {
      monotonic = 1;
      val += 9;
    }
  else if (strncasecmp (val, "nonmonotonic", 12) == 0)
    {
      monotonic = -1;
      val += 12;
    }
  if (monotonic)
    {
      while (isspace ((unsigned char) *val))
	++val;
      if (*val != ':')
	goto unknown;
      ++val;
      while (isspace ((unsigned char) *val))
	++val;
    }
  if (strncasecmp (val, "static", 6) == 0)
    {
      *schedule = GFS_STATIC;
      val += 6;
    }
  else if (strncasecmp (val, "dynamic", 7) == 0)
    {
      *schedule = GFS_DYNAMIC;
      val += 7;
    }
  else if (strncasecmp (val, "guided", 6) == 0)
    {
      *schedule = GFS_GUIDED;
      val += 6;
    }
  else if (strncasecmp (val, "auto", 4) == 0)
    {
      *schedule = GFS_AUTO;
      val += 4;
    }
  else
    goto unknown;

  if (monotonic == 1
      || (monotonic == 0 && *schedule == GFS_STATIC))
    *schedule |= GFS_MONOTONIC;

  while (isspace ((unsigned char) *val))
    ++val;
  if (*val == '\0')
    {
      *chunk_size = (*schedule & ~GFS_MONOTONIC) != GFS_STATIC;
      return true;
    }
  if (*val++ != ',')
    goto unknown;
  while (isspace ((unsigned char) *val))
    ++val;
  if (*val == '\0')
    goto invalid;

  errno = 0;
  value = strtoul (val, &end, 10);
  if (errno || end == val)
    goto invalid;

  while (isspace ((unsigned char) *end))
    ++end;
  if (*end != '\0')
    goto invalid;

  if ((int)value != value)
    goto invalid;

  if (value == 0 && (*schedule & ~GFS_MONOTONIC) != GFS_STATIC)
    value = 1;
  *chunk_size = value;
  return true;

 unknown:
  print_env_var_error (env, val);
  return false;

 invalid:
  char name[val - env];
  memcpy (name, env, val - env - 1);
  name[val - env - 1] = '\0';
  gomp_error ("Invalid value for chunk size in "
	      "environment variable %s: %s", name, val);
  return false;
}

/* Parse an unsigned long environment variable.  Return true if one was
   present and it was successfully parsed.  If SECURE, use secure_getenv to the
   environment variable.  */

static bool
parse_unsigned_long_1 (const char *env, const char *val, unsigned long *pvalue,
		       bool allow_zero)
{
  char *end;
  unsigned long value;

  if (val == NULL)
    return false;

  while (isspace ((unsigned char) *val))
    ++val;
  if (*val == '\0')
    goto invalid;

  errno = 0;
  value = strtoul (val, &end, 10);
  if (errno || end == val || (long) value <= 0 - allow_zero)
    goto invalid;

  while (isspace ((unsigned char) *end))
    ++end;
  if (*end != '\0')
    goto invalid;

  *pvalue = value;
  return true;

 invalid:
  print_env_var_error (env, val);
  return false;
}

/* As parse_unsigned_long_1, but always use getenv.  */

static bool
parse_unsigned_long (const char *env, const char *val, void *const params[])
{
  unsigned long upper = (uintptr_t) params[2];
  unsigned long pvalue = 0;
  bool ret = parse_unsigned_long_1 (env, val, &pvalue, (bool) params[1]);
  if (!ret)
    return false;

  if (upper == 0)
    *(unsigned long *) params[0] = pvalue;
  else
    {
      if (pvalue > upper)
	pvalue = upper;
      if (upper <= UCHAR_MAX)
	*(unsigned char *) params[0] = pvalue;
      else if (upper <= UINT_MAX)
	*(unsigned int *) params[0] = pvalue;
      else
	*(unsigned long *) params[0] = pvalue;
    }

  return ret;
}

/* Parse a positive int environment variable.  Return true if one was
   present and it was successfully parsed.  If SECURE, use secure_getenv to the
   environment variable.  */

static bool
parse_int_1 (const char *env, const char *val, int *pvalue, bool allow_zero)
{
  unsigned long value;
  if (!parse_unsigned_long_1 (env, val, &value, allow_zero))
    return false;
  if (value > INT_MAX)
    {
      print_env_var_error (env, val);
      return false;
    }
  *pvalue = (int) value;
  return true;
}

static bool
parse_int (const char *env, const char *val, void *const params[])
{
  return parse_int_1 (env, val, (int *) params[0], (bool) params[1]);
}

/* As parse_int_1, but use getenv_secure.  */

static bool
parse_int_secure (const char *env, int *pvalue, bool allow_zero)
{
  return parse_int_1 (env, secure_getenv (env), pvalue, allow_zero);
}

/* Parse an unsigned long list environment variable.  Return true if one was
   present and it was successfully parsed.  */

static bool
parse_unsigned_long_list (const char *env, const char *val,
			  void *const params[])
{
  unsigned long *p1stvalue = (unsigned long *) params[0];
  unsigned long **pvalues = (unsigned long **) params[1];
  unsigned long *pnvalues = (unsigned long *) params[2];
  char *end;
  unsigned long value, *values = NULL;

  if (val == NULL)
    return false;

  while (isspace ((unsigned char) *val))
    ++val;
  if (*val == '\0')
    goto invalid;

  errno = 0;
  value = strtoul (val, &end, 10);
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
	      val = end + 1;
	      if (nvalues == nalloced)
		{
		  unsigned long *n;
		  nalloced = nalloced ? nalloced * 2 : 16;
		  n = realloc (values, nalloced * sizeof (unsigned long));
		  if (n == NULL)
		    {
		      free (values);
		      char name[val - env];
		      memcpy (name, env, val - env - 1);
		      name[val - env - 1] = '\0';
		      gomp_error ("Out of memory while trying to parse"
				  " environment variable %s", name);
		      return false;
		    }
		  values = n;
		  if (nvalues == 0)
		    values[nvalues++] = value;
		}

	      while (isspace ((unsigned char) *val))
		++val;
	      if (*val == '\0')
		goto invalid;

	      errno = 0;
	      value = strtoul (val, &end, 10);
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
  else
    {
      *pnvalues = 0;
      *pvalues = NULL;
    }

  *p1stvalue = value;
  return true;

 invalid:
  free (values);
  print_env_var_error (env, val);
  return false;
}

static bool
parse_target_offload (const char *env, const char *val, void *const params[])
{
  int new_offload = -1;

  if (val == NULL)
    return false;

  while (isspace ((unsigned char) *val))
    ++val;
  if (strncasecmp (val, "default", 7) == 0)
    {
      val += 7;
      new_offload = GOMP_TARGET_OFFLOAD_DEFAULT;
    }
  else if (strncasecmp (val, "mandatory", 9) == 0)
    {
      val += 9;
      new_offload = GOMP_TARGET_OFFLOAD_MANDATORY;
    }
  else if (strncasecmp (val, "disabled", 8) == 0)
    {
      val += 8;
      new_offload = GOMP_TARGET_OFFLOAD_DISABLED;
    }
  while (isspace ((unsigned char) *val))
    ++val;
  if (new_offload != -1 && *val == '\0')
    {
      *(enum gomp_target_offload_t *) params[0] = new_offload;
      return true;
    }

  print_env_var_error (env, val);
  return false;
}

/* Parse environment variable set to a boolean or list of omp_proc_bind_t
   enum values.  Return true if one was present and it was successfully
   parsed.  */

static bool
parse_bind_var (const char *env, const char *val, void *const params[])
{
  char *p1stvalue = (char *) params[0];
  char **pvalues = (char **) params[1];
  unsigned long *pnvalues = (unsigned long *) params[2];
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
    { "primary", 7, omp_proc_bind_primary },
    { "close", 5, omp_proc_bind_close },
    { "spread", 6, omp_proc_bind_spread }
  };

  if (val == NULL)
    return false;

  while (isspace ((unsigned char) *val))
    ++val;
  if (*val == '\0')
    goto invalid;

  for (i = 0; i < 6; i++)
    if (strncasecmp (val, kinds[i].name, kinds[i].len) == 0)
      {
	value = kinds[i].kind;
	val += kinds[i].len;
	break;
      }
  if (i == 6)
    goto invalid;

  while (isspace ((unsigned char) *val))
    ++val;
  if (*val != '\0')
    {
      if (*val == ',')
	{
	  unsigned long nvalues = 0, nalloced = 0;

	  if (value == omp_proc_bind_false
	      || value == omp_proc_bind_true)
	    goto invalid;

	  do
	    {
	      val++;
	      if (nvalues == nalloced)
		{
		  char *n;
		  nalloced = nalloced ? nalloced * 2 : 16;
		  n = realloc (values, nalloced);
		  if (n == NULL)
		    {
		      free (values);
		      char name[val - env];
		      memcpy (name, env, val - env - 1);
		      name[val - env - 1] = '\0';
		      gomp_error ("Out of memory while trying to parse"
				  " environment variable %s", name);
		      return false;
		    }
		  values = n;
		  if (nvalues == 0)
		    values[nvalues++] = value;
		}

	      while (isspace ((unsigned char) *val))
		++val;
	      if (*val == '\0')
		goto invalid;

	      for (i = 2; i < 6; i++)
		if (strncasecmp (val, kinds[i].name, kinds[i].len) == 0)
		  {
		    value = kinds[i].kind;
		    val += kinds[i].len;
		    break;
		  }
	      if (i == 6)
		goto invalid;

	      values[nvalues++] = value;
	      while (isspace ((unsigned char) *val))
		++val;
	      if (*val == '\0')
		break;
	      if (*val != ',')
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
  print_env_var_error (env, val);
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
  bool has_braces = true;
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
    {
      char *end;
      unsigned long this_num;

      errno = 0;
      this_num = strtoul (env, &end, 10);
      if (errno || end == env)
	return false;
      env = end - 1;
      has_braces = false;
      if (gomp_places_list
	  && !gomp_affinity_add_cpus (p, this_num, 1, 1, false))
	return false;
    }
  else
    {
      ++env;
      while (isspace ((unsigned char) *env))
	++env;
    }
  start = env;
  for (pass = 0; pass < (any_negate ? 2 : has_braces); pass++)
    {
      env = start;
      do
	{
	  unsigned long this_num, this_len = 1;
	  long this_stride = 1;
	  bool this_negate = (*env == '!');
	  char *end;
	  if (this_negate)
	    {
	      if (gomp_places_list)
		any_negate = true;
	      ++env;
	      while (isspace ((unsigned char) *env))
		++env;
	    }

	  errno = 0;
	  this_num = strtoul (env, &end, 10);
	  if (errno || end == env)
	    return false;
	  env = end;
	  while (isspace ((unsigned char) *env))
	    ++env;
	  if (*env == ':')
	    {
	      ++env;
	      if (this_negate)
		return false;
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
		  this_stride = strtol (env, &end, 10);
		  if (errno || end == env)
		    return false;
		  env = end;
		  while (isspace ((unsigned char) *env))
		    ++env;
		}
	    }
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
      char *end;
      if (*negatep)
	return false;
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
	  stride = strtol (env, &end, 10);
	  if (errno || end == env)
	    return false;
	  env = end;
	  while (isspace ((unsigned char) *env))
	    ++env;
	}
    }
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
  else if (strncasecmp (env, "ll_caches", 9) == 0)
    {
      env += 9;
      level = 4;
    }
  else if (strncasecmp (env, "numa_domains", 12) == 0)
    {
      env += 12;
      level = 5;
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
	  if (errno || end == env)
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
parse_stacksize (const char *env, const char *val, void *const params[])
{
  char *end;
  unsigned long value, shift = 10;

  if (val == NULL)
    return false;

  while (isspace ((unsigned char) *val))
    ++val;
  if (*val == '\0')
    goto invalid;

  errno = 0;
  value = strtoul (val, &end, 10);
  if (errno || end == val)
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

  *(unsigned long *) params[0] = value << shift;
  return true;

 invalid:
  print_env_var_error (env, val);
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
  if (errno || end == env)
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
   result in VALUE.  Return true if one was present and it was
   successfully parsed.  */
static bool
parse_boolean (const char *env, const char *val, void *const params[])
{
  bool *value = (bool *) params[0];

  if (val == NULL)
    return false;

  while (isspace ((unsigned char) *val))
    ++val;
  if (strncasecmp (val, "true", 4) == 0)
    {
      *value = true;
      val += 4;
    }
  else if (strncasecmp (val, "false", 5) == 0)
    {
      *value = false;
      val += 5;
    }
  else
    val = "X";
  while (isspace ((unsigned char) *val))
    ++val;
  if (*val != '\0')
    {
      print_env_var_error (env, val);
      return false;
    }
  return true;
}

/* Parse the OMP_WAIT_POLICY environment variable and return the value.  */

static bool
parse_wait_policy (const char *env, const char *val, void *const params[])
{
  int *pvalue = (int *) params[0];
  int ret = -1;

  if (val == NULL)
  {
    *pvalue = -1;
    return false;
  }

  while (isspace ((unsigned char) *val))
    ++val;
  if (strncasecmp (val, "active", 6) == 0)
    {
      ret = 1;
      val += 6;
    }
  else if (strncasecmp (val, "passive", 7) == 0)
    {
      ret = 0;
      val += 7;
    }
  else
    val = "X";
  while (isspace ((unsigned char) *val))
    ++val;
  if (*val == '\0')
    {
      *pvalue = ret;
      return true;
    }
  print_env_var_error (env, val);
  *pvalue = -1;
  return false;
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
	  if (errno || end == env || cpu_beg >= 65536)
	    goto invalid;
	  cpu_end = cpu_beg;
	  cpu_stride = 1;

	  env = end;
	  if (*env == '-')
	    {
	      errno = 0;
	      cpu_end = strtoul (++env, &end, 0);
	      if (errno || end == env || cpu_end >= 65536 || cpu_end < cpu_beg)
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

/* Parse the OMP_ALLOCATOR environment variable and return the value.  */
static bool
parse_allocator (const char *env, const char *val, void *const params[])
{
  const char *orig_val = val;
  uintptr_t *ret = (uintptr_t *) params[0];
  *ret = omp_default_mem_alloc;
  bool memspace = false;
  size_t ntraits = 0;
  omp_alloctrait_t *traits;

  if (val == NULL)
    return false;

  while (isspace ((unsigned char) *val))
    ++val;
  if (0)
    ;
#define C(v, m) \
  else if (strncasecmp (val, #v, sizeof (#v) - 1) == 0)	\
    {							\
      *ret = v;						\
      val += sizeof (#v) - 1;				\
      memspace = m;					\
    }
  C (omp_default_mem_alloc, false)
  C (omp_large_cap_mem_alloc, false)
  C (omp_const_mem_alloc, false)
  C (omp_high_bw_mem_alloc, false)
  C (omp_low_lat_mem_alloc, false)
  C (omp_cgroup_mem_alloc, false)
  C (omp_pteam_mem_alloc, false)
  C (omp_thread_mem_alloc, false)
  C (ompx_gnu_pinned_mem_alloc, false)
  C (omp_default_mem_space, true)
  C (omp_large_cap_mem_space, true)
  C (omp_const_mem_space, true)
  C (omp_high_bw_mem_space, true)
  C (omp_low_lat_mem_space, true)
#undef C
  else
    goto invalid;
  if (memspace && *val == ':')
    {
      ++val;
      const char *cp = val;
      while (*cp != '\0')
	{
	  if (*cp == '=')
	    ++ntraits;
	  ++cp;
	}
      traits = gomp_alloca (ntraits * sizeof (omp_alloctrait_t));
      size_t n = 0;
      while (*val != '\0')
	{
#define C(v) \
	  else if (strncasecmp (val, #v "=", sizeof (#v)) == 0)	\
	    {							\
	      val += sizeof (#v);				\
	      traits[n].key = omp_atk_ ## v;
#define V(v) \
	    else if (strncasecmp (val, #v, sizeof (#v) - 1) == 0)	\
	      {								\
		val += sizeof (#v) - 1;					\
		traits[n].value = omp_atv_ ## v;			\
	      }
	  if (0)
	    ;
	  C (sync_hint)
	      if (0)
		;
	      V (contended)
	      V (uncontended)
	      V (serialized)
	      V (private)
	      else
		goto invalid;
	    }
	  C (alignment)
	      char *end;
	      errno = 0;
	      traits[n].value = strtol (val, &end, 10);
	      if (errno || end == val || traits[n].value <= 0)
		goto invalid;
	      val = end;
	    }
	  C (access)
	      if (0)
		;
	      V (all)
	      V (cgroup)
	      V (pteam)
	      V (thread)
	      else
		goto invalid;
	    }
	  C (pool_size)
	      char *end;
	      errno = 0;
	      traits[n].value = strtol (val, &end, 10);
	      if (errno || end == val || traits[n].value <= 0)
		goto invalid;
	      val = end;
	    }
	  C (fallback)
	      if (0)
		;
	      V (default_mem_fb)
	      V (null_fb)
	      V (abort_fb)
	      V (allocator_fb)
	      else
		goto invalid;
	    }
	  /* Ignore fb_data, which expects an allocator handle.  */
	  C (pinned)
	      if (0)
		;
	      V (true)
	      V (false)
	      else
		goto invalid;
	    }
	  C (partition)
	      if (0)
		;
	      V (environment)
	      V (nearest)
	      V (blocked)
	      V (interleaved)
	      else
		goto invalid;
	    }
	  else
	    goto invalid;
	  if (*val != ',')
	    break;
	  ++val;
	  ++n;
	  if (*val == '\0')
	    goto invalid;
	}
#undef C
#undef V
    }
  else if (memspace)
    switch (*ret)
      {
	case omp_default_mem_space: *ret = omp_default_mem_alloc; break;
	case omp_large_cap_mem_space: *ret = omp_large_cap_mem_alloc; break;
	case omp_const_mem_space: *ret = omp_const_mem_alloc; break;
	case omp_high_bw_mem_space: *ret = omp_high_bw_mem_alloc; break;
	case omp_low_lat_mem_space: *ret = omp_low_lat_mem_alloc; break;
	default: __builtin_unreachable ();
      }
  while (isspace ((unsigned char) *val))
    ++val;
  if (*val == '\0')
    {
      if (ntraits)
	{
	  *ret = omp_init_allocator (*ret, ntraits, traits);
	  if (*ret == omp_null_allocator)
	    {
	      gomp_error ("Allocator of environment variable %.*s cannot be "
			  "created, using omp_default_mem_alloc instead",
			  (int) (orig_val - env - 1), env);
	      *ret = omp_default_mem_alloc;
	    }
	  else
	    gomp_def_allocator_envvar = strdup (orig_val);
	}
      return true;
    }
invalid:
  int len = (orig_val - env - 1);
  if (*val == '\0')
    gomp_error ("Missing value at the end of environment variable %s", env);
  else
    gomp_error ("Invalid value for environment variable %.*s when parsing: %s",
		len, env, val);
  *ret = omp_default_mem_alloc;
  return false;
}

static void
parse_acc_device_type (void)
{
  const char *env = getenv ("ACC_DEVICE_TYPE");

  if (env && *env != '\0')
    goacc_device_type = strdup (env);
  else
    goacc_device_type = NULL;
}

static void
parse_gomp_openacc_dim (void)
{
  /* The syntax is the same as for the -fopenacc-dim compilation option.  */
  const char *var_name = "GOMP_OPENACC_DIM";
  const char *env_var = getenv (var_name);
  const char *pos = env_var;
  int i;

  if (!env_var)
    return;

  for (i = 0; *pos && i != GOMP_DIM_MAX; i++)
    {
      char *eptr;
      long val;

      if (i && *pos++ != ':')
	break;

      if (*pos == ':')
	continue;

      errno = 0;
      val = strtol (pos, &eptr, 10);
      if (errno || eptr == pos || val < 0 || (unsigned)val != val)
	break;

      goacc_default_dims[i] = (int)val;
      pos = (const char *) eptr;
    }
}

/* Helper function for omp_display_env which prints the values of run_sched_var.
   'device' can be 'host', 'dev', 'all' or a particular device number.  */

static void
print_schedule (enum gomp_schedule_type run_sched_var, int run_sched_chunk_size,
		const char *device)
{
  fprintf (stderr, "  [%s] OMP_SCHEDULE = '", device);
  if ((run_sched_var & GFS_MONOTONIC))
    {
      if (run_sched_var != (GFS_MONOTONIC | GFS_STATIC))
	fputs ("MONOTONIC:", stderr);
    }
  else if (run_sched_var == GFS_STATIC)
    fputs ("NONMONOTONIC:", stderr);
  switch (run_sched_var & ~GFS_MONOTONIC)
    {
    case GFS_RUNTIME:
      fputs ("RUNTIME", stderr);
      if (run_sched_chunk_size != 1)
	fprintf (stderr, ",%d", run_sched_chunk_size);
      break;
    case GFS_STATIC:
      fputs ("STATIC", stderr);
      if (run_sched_chunk_size != 0)
	fprintf (stderr, ",%d", run_sched_chunk_size);
      break;
    case GFS_DYNAMIC:
      fputs ("DYNAMIC", stderr);
      if (run_sched_chunk_size != 1)
	fprintf (stderr, ",%d", run_sched_chunk_size);
      break;
    case GFS_GUIDED:
      fputs ("GUIDED", stderr);
      if (run_sched_chunk_size != 1)
	fprintf (stderr, ",%d", run_sched_chunk_size);
      break;
    case GFS_AUTO:
      fputs ("AUTO", stderr);
      break;
    }
  fputs ("'\n", stderr);
}

/* Helper function for omp_display_env which prints the values of proc_bind_var.
   'device' can be 'host', 'dev', 'all', or a particular device number.  */

static void
print_proc_bind (char proc_bind_var, unsigned long len, char **list,
		 const char *device)
{
  fprintf (stderr, "  [%s] OMP_PROC_BIND = '", device);
  switch (proc_bind_var)
    {
    case omp_proc_bind_false:
      fputs ("FALSE", stderr);
      break;
    case omp_proc_bind_true:
      fputs ("TRUE", stderr);
      break;
    case omp_proc_bind_master:
      fputs ("MASTER", stderr); /* TODO: Change to PRIMARY for OpenMP 5.1.  */
      break;
    case omp_proc_bind_close:
      fputs ("CLOSE", stderr);
      break;
    case omp_proc_bind_spread:
      fputs ("SPREAD", stderr);
      break;
    }
  for (int i = 1; i < len; i++)
    switch ((*list)[i])
      {
      case omp_proc_bind_master:
	fputs (",MASTER", stderr); /* TODO: Change to PRIMARY for OpenMP 5.1. */
	break;
      case omp_proc_bind_close:
	fputs (",CLOSE", stderr);
	break;
      case omp_proc_bind_spread:
	fputs (",SPREAD", stderr);
	break;
      }
  fputs ("'\n", stderr);
}

enum gomp_parse_type
{
  PARSE_INT = 1,
  PARSE_BOOL = 2,
  PARSE_UINT = 3,
  PARSE_ULONG = 4,
  PARSE_UCHAR = 5,
  PARSE_SCHEDULE =6,
  PARSE_BIND = 7
};

/* The following table contains items that help parsing environment variables
   and fill corresponding ICVs with values.  FLAG_VARS contain all ICVS which
   are affected by the environment variable.  FLAGS determine what variant of
   environment variable is allowed.  */

#define ENTRY(NAME) NAME, sizeof (NAME) - 1
static const struct envvar
{
  const char *name;
  int name_len;
  uint8_t flag_vars[3];
  uint8_t flag;
  bool (*parse_func) (const char *, const char *, void *const[]);
} envvars[] = {
  { ENTRY ("SCHEDULE"),
    { GOMP_ICV_SCHEDULE, GOMP_ICV_SCHEDULE_CHUNK_SIZE },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_schedule },
  { ENTRY ("NUM_TEAMS"),
    { GOMP_ICV_NTEAMS },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_int },
  { ENTRY ("DYNAMIC"),
    { GOMP_ICV_DYNAMIC },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_boolean },
  { ENTRY ("TEAMS_THREAD_LIMIT"),
    { GOMP_ICV_TEAMS_THREAD_LIMIT },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_int },
  { ENTRY ("THREAD_LIMIT"),
    { GOMP_ICV_THREAD_LIMIT },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_unsigned_long },
  { ENTRY ("NUM_THREADS"),
    { GOMP_ICV_NTHREADS, GOMP_ICV_NTHREADS_LIST, GOMP_ICV_NTHREADS_LIST_LEN },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_unsigned_long_list },
  { ENTRY ("PROC_BIND"),
    { GOMP_ICV_BIND, GOMP_ICV_BIND_LIST, GOMP_ICV_BIND_LIST_LEN },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_bind_var },
  { ENTRY ("MAX_ACTIVE_LEVELS"),
    { GOMP_ICV_MAX_ACTIVE_LEVELS },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_unsigned_long },
  { ENTRY ("WAIT_POLICY"),
    { GOMP_ICV_WAIT_POLICY },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_wait_policy },
  { ENTRY ("STACKSIZE"),
    { GOMP_ICV_STACKSIZE },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_stacksize },
  { ENTRY ("CANCELLATION"), { GOMP_ICV_CANCELLATION }, 0, &parse_boolean },
  { ENTRY ("DISPLAY_AFFINITY"), { GOMP_ICV_DISPLAY_AFFINITY }, 0,
    &parse_boolean },
  { ENTRY ("TARGET_OFFLOAD"), { GOMP_ICV_TARGET_OFFLOAD }, 0,
    &parse_target_offload },
  { ENTRY ("MAX_TASK_PRIORITY"), { GOMP_ICV_MAX_TASK_PRIORITY }, 0,
    &parse_int },
  { ENTRY ("ALLOCATOR"), { GOMP_ICV_ALLOCATOR }, 0, &parse_allocator },
  { ENTRY ("DEFAULT_DEVICE"), { GOMP_ICV_DEFAULT_DEVICE },
    GOMP_ENV_SUFFIX_DEV | GOMP_ENV_SUFFIX_ALL | GOMP_ENV_SUFFIX_DEV_X,
    &parse_int }
};
#undef ENTRY
#define OMP_VAR_CNT (sizeof (envvars) / sizeof (envvars[0]))

/* The following table is used to apply the hierarchy of ICV variants for host
   variables, e.g. nteams_var is set to OMP_NUM_TEAMS_ALL if OMP_NUM_TEAMS is
   undefined.  */

static const struct host_envvar
{
  unsigned char flag_var;
  void *dest[3];
  int type_code;
} host_envvars[] = {
  { GOMP_ICV_NTEAMS, { &gomp_nteams_var }, PARSE_INT },
  { GOMP_ICV_DYNAMIC, { &gomp_global_icv.dyn_var }, PARSE_BOOL },
  { GOMP_ICV_DEFAULT_DEVICE, { &gomp_global_icv.default_device_var },
    PARSE_INT },
  { GOMP_ICV_TEAMS_THREAD_LIMIT, { &gomp_teams_thread_limit_var }, PARSE_INT },
  { GOMP_ICV_SCHEDULE,
    { &gomp_global_icv.run_sched_var, &gomp_global_icv.run_sched_chunk_size },
    PARSE_SCHEDULE },
  { GOMP_ICV_THREAD_LIMIT, { &gomp_global_icv.thread_limit_var }, PARSE_UINT },
  { GOMP_ICV_NTHREADS,
    { &gomp_global_icv.nthreads_var, &gomp_nthreads_var_list,
      &gomp_nthreads_var_list_len }, PARSE_ULONG },
  { GOMP_ICV_BIND,
    { &gomp_global_icv.bind_var, &gomp_bind_var_list, &gomp_bind_var_list_len },
    PARSE_BIND },
  { GOMP_ICV_MAX_ACTIVE_LEVELS, { &gomp_global_icv.max_active_levels_var },
    PARSE_UCHAR },
};
#define OMP_HOST_VAR_CNT (sizeof (host_envvars) / sizeof (host_envvars[0]))

#define INT_MAX_STR_LEN 10

bool
gomp_get_icv_flag (uint32_t value, enum gomp_icvs icv)
{
  return value & (1 << (icv - 1));
}

static void
gomp_set_icv_flag (uint32_t *value, enum gomp_icvs icv)
{
  *value |= 1 << (icv - 1);
}

static void
print_device_specific_icvs (int icv_code)
{
  struct gomp_icv_list *list = gomp_initial_icv_list;
  int i;
  char dev_num[INT_MAX_STR_LEN + 1];

  while (list != NULL)
    {
      if (list->device_num < 0)
	{
	  list = list->next;
	  continue;
	}

      switch (icv_code)
	{
	case GOMP_ICV_NTEAMS:
	  if (gomp_get_icv_flag (list->flags, GOMP_ICV_NTEAMS))
	    fprintf (stderr, "  [%d] OMP_NUM_TEAMS = '%d'\n",
		     list->device_num, list->icvs.nteams_var);
	  break;
	case GOMP_ICV_DYNAMIC:
	  if (gomp_get_icv_flag (list->flags, GOMP_ICV_DYNAMIC))
	    fprintf (stderr, "  [%d] OMP_DYNAMIC = '%s'\n",
		     list->device_num, list->icvs.dyn_var ? "TRUE" : "FALSE");
	  break;
	case GOMP_ICV_TEAMS_THREAD_LIMIT:
	  if (gomp_get_icv_flag (list->flags, GOMP_ICV_TEAMS_THREAD_LIMIT))
	    fprintf (stderr, "  [%d] OMP_TEAMS_THREAD_LIMIT = '%u'\n",
		     list->device_num, list->icvs.teams_thread_limit_var);
	  break;
	case GOMP_ICV_SCHEDULE:
	  if (!(gomp_get_icv_flag (list->flags, GOMP_ICV_SCHEDULE)))
	    break;
	  sprintf (dev_num, "%d", list->device_num);
	  print_schedule (list->icvs.run_sched_var,
			  list->icvs.run_sched_chunk_size,
			  dev_num);
	  break;
	case GOMP_ICV_THREAD_LIMIT:
	  if (gomp_get_icv_flag (list->flags, GOMP_ICV_THREAD_LIMIT))
	    fprintf (stderr, "  [%d] OMP_THREAD_LIMIT = '%d'\n",
		     list->device_num, list->icvs.thread_limit_var);
	  break;
	case GOMP_ICV_NTHREADS:
	  if (!(gomp_get_icv_flag (list->flags, GOMP_ICV_NTHREADS)))
	    break;
	  fprintf (stderr, "  [%d] OMP_NUM_THREADS = '%lu", list->device_num,
		   list->icvs.nthreads_var);
	  for (i = 1; i < list->icvs.nthreads_var_list_len; i++)
	    fprintf (stderr, ",%lu", list->icvs.nthreads_var_list[i]);
	  fputs ("'\n", stderr);
	  break;
	case GOMP_ICV_MAX_ACTIVE_LEVELS:
	  fprintf (stderr, "  [%d] OMP_MAX_ACTIVE_LEVELS = '%u'\n",
		   list->device_num, list->icvs.max_active_levels_var);
	  break;
	case GOMP_ICV_BIND:
	  if (!(gomp_get_icv_flag (list->flags, GOMP_ICV_BIND)))
	    break;
	  sprintf (dev_num, "%d", list->device_num);
	  print_proc_bind (list->icvs.bind_var, list->icvs.bind_var_list_len,
			   &list->icvs.bind_var_list, dev_num);
	  break;
	case GOMP_ICV_WAIT_POLICY:
	  if (gomp_get_icv_flag (list->flags, GOMP_ICV_WAIT_POLICY))
	    fprintf (stderr, "  [%d] OMP_WAIT_POLICY = '%s'\n",
		     list->device_num,
		     list->icvs.wait_policy > 0 ? "ACTIVE" : "PASSIVE");
	  break;
	case GOMP_ICV_STACKSIZE:
	  if (gomp_get_icv_flag (list->flags, GOMP_ICV_STACKSIZE))
	    fprintf (stderr, "  [%d] OMP_STACKSIZE = '%lu'\n",
		     list->device_num, list->icvs.stacksize);
	  break;
	}
      list = list->next;
    }
}

void
omp_display_env (int verbose)
{
  int i;
  struct gomp_icv_list *dev
    = gomp_get_initial_icv_item (GOMP_DEVICE_NUM_FOR_DEV);
  struct gomp_icv_list *all
    = gomp_get_initial_icv_item (GOMP_DEVICE_NUM_FOR_ALL);
  struct gomp_icv_list *none
    = gomp_get_initial_icv_item (GOMP_DEVICE_NUM_FOR_NO_SUFFIX);

  if (none->icvs.default_device_var == INT_MIN)
    /* This implies OMP_TARGET_OFFLOAD=mandatory.  */
    gomp_init_targets_once ();

  fputs ("\nOPENMP DISPLAY ENVIRONMENT BEGIN\n", stderr);

  fputs ("  _OPENMP = '201511'\n", stderr);

  fprintf (stderr, "  [host] OMP_DYNAMIC = '%s'\n",
	   none->icvs.dyn_var ? "TRUE" : "FALSE");
  if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_DYNAMIC))
    fprintf (stderr, "  [all] OMP_DYNAMIC = '%s'\n",
	     all->icvs.dyn_var ? "TRUE" : "FALSE");
  if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_DYNAMIC))
    fprintf (stderr, "  [device] OMP_DYNAMIC = '%s'\n",
	     dev->icvs.dyn_var ? "TRUE" : "FALSE");
  print_device_specific_icvs (GOMP_ICV_DYNAMIC);

  /* The OMP_NESTED environment variable has been deprecated.  */
  fprintf (stderr, "  [host] OMP_NESTED = '%s'\n",
	   none->icvs.max_active_levels_var > 1 ? "TRUE" : "FALSE");

  fprintf (stderr, "  [host] OMP_NUM_THREADS = '%lu",
	   none->icvs.nthreads_var);
  for (i = 1; i < none->icvs.nthreads_var_list_len; i++)
    fprintf (stderr, ",%lu", none->icvs.nthreads_var_list[i]);
  fputs ("'\n", stderr);
  if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_NTHREADS))
    {
      fprintf (stderr, "  [all] OMP_NUM_THREADS = '%lu",
	       all->icvs.nthreads_var);
      for (i = 1; i < all->icvs.nthreads_var_list_len; i++)
	fprintf (stderr, ",%lu", all->icvs.nthreads_var_list[i]);
      fputs ("'\n", stderr);
    }
  if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_NTHREADS))
    {
      fprintf (stderr, "  [device] OMP_NUM_THREADS = '%lu",
	       dev->icvs.nthreads_var);
      for (i = 1; i < dev->icvs.nthreads_var_list_len; i++)
	fprintf (stderr, ",%lu", dev->icvs.nthreads_var_list[i]);
      fputs ("'\n", stderr);
    }
  print_device_specific_icvs (GOMP_ICV_NTHREADS);


  print_schedule (none->icvs.run_sched_var,
		  none->icvs.run_sched_chunk_size, "host");
  if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_SCHEDULE))
    print_schedule (all->icvs.run_sched_var,
		    all->icvs.run_sched_chunk_size, "all");
  if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_SCHEDULE))
    print_schedule (dev->icvs.run_sched_var,
		    dev->icvs.run_sched_chunk_size, "device");
  print_device_specific_icvs (GOMP_ICV_SCHEDULE);

  print_proc_bind (none->icvs.bind_var,
		   none->icvs.bind_var_list_len,
		   &none->icvs.bind_var_list, "host");
  if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_BIND))
    print_proc_bind (all->icvs.bind_var,
		     all->icvs.bind_var_list_len,
		     &all->icvs.bind_var_list, "all");
  if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_BIND))
    print_proc_bind (dev->icvs.bind_var,
		     dev->icvs.bind_var_list_len,
		     &dev->icvs.bind_var_list, "device");
  print_device_specific_icvs (GOMP_ICV_BIND);

  fputs ("  [host] OMP_PLACES = '", stderr);
  for (i = 0; i < gomp_places_list_len; i++)
    {
      fputs ("{", stderr);
      gomp_affinity_print_place (gomp_places_list[i]);
      fputs (i + 1 == gomp_places_list_len ? "}" : "},", stderr);
    }
  fputs ("'\n", stderr);

  fprintf (stderr, "  [host] OMP_STACKSIZE = '%lu'\n",
	   none->icvs.stacksize);
  if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_STACKSIZE))
    fprintf (stderr, "  [all] OMP_STACKSIZE = '%lu'\n",
	     all->icvs.stacksize);
  if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_STACKSIZE))
    fprintf (stderr, "  [device] OMP_STACKSIZE = '%lu'\n",
	     dev->icvs.stacksize);
  print_device_specific_icvs (GOMP_ICV_STACKSIZE);

  /* GOMP's default value is actually neither active nor passive.  */
  fprintf (stderr, "  [host] OMP_WAIT_POLICY = '%s'\n",
	   none->icvs.wait_policy > 0 ? "ACTIVE" : "PASSIVE");
  if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_WAIT_POLICY))
    fprintf (stderr, "  [all] OMP_WAIT_POLICY = '%s'\n",
	     all->icvs.wait_policy > 0 ? "ACTIVE" : "PASSIVE");
  if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_WAIT_POLICY))
    fprintf (stderr, "  [device] OMP_WAIT_POLICY = '%s'\n",
	     dev->icvs.wait_policy > 0 ? "ACTIVE" : "PASSIVE");
  print_device_specific_icvs (GOMP_ICV_WAIT_POLICY);

  fprintf (stderr, "  [host] OMP_THREAD_LIMIT = '%u'\n",
	   none->icvs.thread_limit_var);
  if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_THREAD_LIMIT))
    fprintf (stderr, "  [all] OMP_THREAD_LIMIT = '%d'\n",
	     all->icvs.thread_limit_var);
  if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_THREAD_LIMIT))
    fprintf (stderr, "  [device] OMP_THREAD_LIMIT = '%d'\n",
	     dev->icvs.thread_limit_var);
  print_device_specific_icvs (GOMP_ICV_THREAD_LIMIT);

  fprintf (stderr, "  [host] OMP_MAX_ACTIVE_LEVELS = '%u'\n",
	   none->icvs.max_active_levels_var);
  if (all != NULL && gomp_get_icv_flag (all->flags,
			 GOMP_ICV_MAX_ACTIVE_LEVELS))
    fprintf (stderr, "  [all] OMP_MAX_ACTIVE_LEVELS = '%u'\n",
	     all->icvs.max_active_levels_var);
  if (dev != NULL && gomp_get_icv_flag (dev->flags,
			 GOMP_ICV_MAX_ACTIVE_LEVELS))
    fprintf (stderr, "  [device] OMP_MAX_ACTIVE_LEVELS = '%u'\n",
	     dev->icvs.max_active_levels_var);
  print_device_specific_icvs (GOMP_ICV_MAX_ACTIVE_LEVELS);


  fprintf (stderr, "  [host] OMP_NUM_TEAMS = '%d'\n",
	   none->icvs.nteams_var);
  if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_NTEAMS))
    fprintf (stderr, "  [all] OMP_NUM_TEAMS = '%d'\n",
	     all->icvs.nteams_var);
  if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_NTEAMS))
    fprintf (stderr, "  [device] OMP_NUM_TEAMS = '%d'\n",
	     dev->icvs.nteams_var);
  print_device_specific_icvs (GOMP_ICV_NTEAMS);

  fprintf (stderr, "  [host] OMP_TEAMS_THREAD_LIMIT = '%u'\n",
	   none->icvs.teams_thread_limit_var);
  if (all != NULL && gomp_get_icv_flag (all->flags,
			 GOMP_ICV_TEAMS_THREAD_LIMIT))
    fprintf (stderr, "  [all] OMP_TEAMS_THREAD_LIMIT = '%u'\n",
	     all->icvs.teams_thread_limit_var);
  if (dev != NULL && gomp_get_icv_flag (dev->flags,
			 GOMP_ICV_TEAMS_THREAD_LIMIT))
    fprintf (stderr, "  [device] OMP_TEAMS_THREAD_LIMIT = '%u'\n",
	     dev->icvs.teams_thread_limit_var);
  print_device_specific_icvs (GOMP_ICV_TEAMS_THREAD_LIMIT);

  fprintf (stderr, "  [all] OMP_CANCELLATION = '%s'\n",
	   gomp_cancel_var ? "TRUE" : "FALSE");

  fprintf (stderr, "  [all] OMP_DEFAULT_DEVICE = '%d'\n",
	   none->icvs.default_device_var);

  fprintf (stderr, "  [all] OMP_MAX_TASK_PRIORITY = '%d'\n",
	   gomp_max_task_priority_var);
  fprintf (stderr, "  [all] OMP_DISPLAY_AFFINITY = '%s'\n",
	   gomp_display_affinity_var ? "TRUE" : "FALSE");
  fprintf (stderr, "  [host] OMP_AFFINITY_FORMAT = '%s'\n",
	   gomp_affinity_format_var);
  fprintf (stderr, "  [host] OMP_ALLOCATOR = '");
  switch (gomp_def_allocator)
    {
#define C(v) case v: fputs (#v, stderr); break;
    C (omp_default_mem_alloc)
    C (omp_large_cap_mem_alloc)
    C (omp_const_mem_alloc)
    C (omp_high_bw_mem_alloc)
    C (omp_low_lat_mem_alloc)
    C (omp_cgroup_mem_alloc)
    C (omp_pteam_mem_alloc)
    C (omp_thread_mem_alloc)
#undef C
    /* For an OMP_ALLOCATOR with traits, '' will be output.  */
    default:
      if (gomp_def_allocator_envvar)
	fputs (gomp_def_allocator_envvar, stderr);
      break;
    }
  fputs ("'\n", stderr);

  fputs ("  [all] OMP_TARGET_OFFLOAD = '", stderr);
  switch (gomp_target_offload_var)
    {
    case GOMP_TARGET_OFFLOAD_DEFAULT:
      fputs ("DEFAULT", stderr);
      break;
    case GOMP_TARGET_OFFLOAD_MANDATORY:
      fputs ("MANDATORY", stderr);
      break;
    case GOMP_TARGET_OFFLOAD_DISABLED:
      fputs ("DISABLED", stderr);
      break;
    }
  fputs ("'\n", stderr);

  if (verbose)
    {
      fputs ("  [host] GOMP_CPU_AFFINITY = ''\n", stderr);
      fprintf (stderr, "  [host] GOMP_STACKSIZE = '%lu'\n", stacksize);
#ifdef HAVE_INTTYPES_H
      fprintf (stderr, "  [host] GOMP_SPINCOUNT = '%"PRIu64"'\n",
	       (uint64_t) gomp_spin_count_var);
#else
      fprintf (stderr, "  [host] GOMP_SPINCOUNT = '%lu'\n",
	       (unsigned long) gomp_spin_count_var);
#endif
    }

  fputs ("OPENMP DISPLAY ENVIRONMENT END\n", stderr);
}
ialias (omp_display_env)

static void
handle_omp_display_env (void)
{
  const char *env;
  bool display = false;
  bool verbose = false;

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

  if (display)
    ialias_call (omp_display_env) (verbose);
}

/* Helper function for initialize_env.  Extracts the device number from
   an environment variable name.  ENV is the complete environment variable.
   DEV_NUM_PTR points to the start of the device number in the environment
   variable string.  DEV_NUM_LEN is the returned length of the device num
   string.  */

static bool
get_device_num (char *env, char *dev_num_ptr, int *dev_num, int *dev_num_len)
{
  char *end;
  unsigned long val = strtoul (dev_num_ptr, &end, 10);
  if (val > INT_MAX
      || *end != '='
      || (dev_num_ptr[0] == '0' && end != dev_num_ptr + 1)
      || (dev_num_ptr[0] < '0' || dev_num_ptr[0] > '9'))
    {
      gomp_error ("Invalid device number in %s", env);
      return false;
    }
  *dev_num = val;
  *dev_num_len = end - dev_num_ptr;
  return true;
}

static void
get_icv_member_addr (struct gomp_initial_icvs *icvs, int icv_code,
		     void *icv_addr[3])
{
  if (icv_code == 0 || icv_addr == NULL)
    return;

  icv_addr[0] = icv_addr[1] = icv_addr[2] = NULL;

  switch (icv_code)
    {
    case GOMP_ICV_NTEAMS:
      icv_addr[0] = &icvs->nteams_var;
      /* icv_addr[1] = (void *) false; */
      break;
    case GOMP_ICV_DYNAMIC:
      icv_addr[0] = &(*icvs).dyn_var;
      break;
    case GOMP_ICV_TEAMS_THREAD_LIMIT:
      icv_addr[0] = &icvs->teams_thread_limit_var;
      /* icv_addr[1] = (void *) false; */
      break;
    case GOMP_ICV_SCHEDULE:
      icv_addr[0] = &icvs->run_sched_var;
      icv_addr[1] = &icvs->run_sched_chunk_size;
      break;
    case GOMP_ICV_THREAD_LIMIT:
      icv_addr[0] = &icvs->thread_limit_var;
      /* icv_addr[1] = (void *) false; */
      icv_addr[2] = (void *) UINT_MAX;
      break;
    case GOMP_ICV_NTHREADS:
      icv_addr[0] = &icvs->nthreads_var;
      icv_addr[1] = &icvs->nthreads_var_list;
      icv_addr[2] = &icvs->nthreads_var_list_len;
      break;
    case GOMP_ICV_MAX_ACTIVE_LEVELS:
      icv_addr[0] = &icvs->max_active_levels_var;
      icv_addr[1] = (void *) true;
      icv_addr[2] = (void *) gomp_supported_active_levels;
      break;
    case GOMP_ICV_BIND:
      icv_addr[0] = &icvs->bind_var;
      icv_addr[1] = &icvs->bind_var_list;
      icv_addr[2] = &icvs->bind_var_list_len;
      break;
    case GOMP_ICV_WAIT_POLICY:
      icv_addr[0] = &icvs->wait_policy;
      break;
    case GOMP_ICV_STACKSIZE:
      icv_addr[0] = &icvs->stacksize;
      break;
    case GOMP_ICV_CANCELLATION:
      icv_addr[0] = &gomp_cancel_var;
      break;
    case GOMP_ICV_DISPLAY_AFFINITY:
      icv_addr[0] = &gomp_display_affinity_var;
      break;
    case GOMP_ICV_TARGET_OFFLOAD:
      icv_addr[0] = &gomp_target_offload_var;
      break;
    case GOMP_ICV_MAX_TASK_PRIORITY:
      icv_addr[0] = &gomp_max_task_priority_var;
      break;
    case GOMP_ICV_ALLOCATOR:
      icv_addr[0] = &gomp_def_allocator;
      break;
    case GOMP_ICV_DEFAULT_DEVICE:
      icv_addr[0] = &icvs->default_device_var;
      icv_addr[1] = (void *) true;
      break;
    }
}

struct gomp_icv_list *
gomp_get_initial_icv_item (int dev_num)
{
  struct gomp_icv_list *l = gomp_initial_icv_list;
  while (l != NULL && l->device_num != dev_num)
    l = l->next;

  return l;
}

static void
initialize_icvs (struct gomp_initial_icvs *icvs)
{
  icvs->nthreads_var_list = NULL;
  icvs->bind_var_list = NULL;
  icvs->nthreads_var = gomp_default_icv_values.nthreads_var;
  icvs->nthreads_var_list_len = 0;
  icvs->bind_var_list_len = 0;
  icvs->stacksize = 0;
  icvs->thread_limit_var = gomp_default_icv_values.thread_limit_var;
  icvs->run_sched_var = gomp_default_icv_values.run_sched_var;
  icvs->run_sched_chunk_size = gomp_default_icv_values.run_sched_chunk_size;
  icvs->default_device_var = gomp_default_icv_values.default_device_var;
  icvs->dyn_var = gomp_default_icv_values.dyn_var;
  icvs->max_active_levels_var = gomp_default_icv_values.max_active_levels_var;
  icvs->bind_var = gomp_default_icv_values.bind_var;
  icvs->nteams_var = gomp_default_icv_values.nteams_var;
  icvs->teams_thread_limit_var = gomp_default_icv_values.teams_thread_limit_var;
  icvs->wait_policy = -1;
}

/* Helper function for initialize_env to add a device specific ICV value
   to gomp_initial_icv_list.  */

static uint32_t *
add_initial_icv_to_list (int dev_num, int icv_code, void *icv_addr[3])
{
  struct gomp_icv_list *last = NULL, *l = gomp_initial_icv_list;
  while (l != NULL && l->device_num != dev_num)
    {
      last = l;
      l = l->next;
    }

  if (l == NULL)
    {
      l = ((struct gomp_icv_list *)
	   gomp_malloc_cleared (sizeof (struct gomp_icv_list)));
      l->device_num = dev_num;
      initialize_icvs (&l->icvs);
      if (dev_num < 0)
	{
	  l->next = gomp_initial_icv_list;
	  gomp_initial_icv_list = l;
	}
      else
	{
	  l->next = NULL;
	  if (last == NULL)
	    gomp_initial_icv_list = l;
	  else
	    last->next = l;
	}
    }

  get_icv_member_addr (&l->icvs, icv_code, icv_addr);

  return &l->flags;
}

/* Return true if STR string starts with PREFIX.  */

static inline bool
startswith (const char *str, const char *prefix)
{
  return strncmp (str, prefix, strlen (prefix)) == 0;
}

static void __attribute__((destructor))
cleanup_env (void)
{
  if (gomp_def_allocator_envvar != NULL)
    {
      free (gomp_def_allocator_envvar);
      omp_destroy_allocator (gomp_def_allocator);
    }
}

static void __attribute__((constructor))
initialize_env (void)
{
  char **env;
  int omp_var, dev_num = 0, dev_num_len = 0, i;
  bool ignore = false;
  char *env_val;
  void *params[3];
  uint32_t *flag_var_addr = NULL;
  unsigned pos;
  struct gomp_icv_list *all, *none;

  /* Do a compile time check that mkomp_h.pl did good job.  */
  omp_check_defines ();

#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_init (&gomp_managed_threads_lock);
#endif
  gomp_init_num_threads ();
  gomp_available_cpus = gomp_global_icv.nthreads_var;

  /* Initial values for host environment variables should always exist even if
     there is no explicitly set host environment variable.  Moreover, they are
     set to the initial global values.  */
  add_initial_icv_to_list (GOMP_DEVICE_NUM_FOR_NO_SUFFIX, 0, NULL);
  none = gomp_get_initial_icv_item (GOMP_DEVICE_NUM_FOR_NO_SUFFIX);
  initialize_icvs (&none->icvs);

  if (environ)
    for (env = environ; *env != 0; env++)
      {
	if (!startswith (*env, "OMP_"))
	  continue;

       /* Name of the environment variable without suffix "OMP_".  */
       char *name = *env + sizeof ("OMP_") - 1;
       for (omp_var = 0; omp_var < OMP_VAR_CNT; omp_var++)
	  {
	    if (startswith (name, envvars[omp_var].name))
	      {
		pos = envvars[omp_var].name_len;
		if (name[pos] == '=')
		  {
		    pos++;
		    flag_var_addr
		      = add_initial_icv_to_list (GOMP_DEVICE_NUM_FOR_NO_SUFFIX,
						 envvars[omp_var].flag_vars[0],
						 params);
		  }
		else if (startswith (&name[pos], "_DEV=")
			 && envvars[omp_var].flag & GOMP_ENV_SUFFIX_DEV)
		  {
		    pos += 5;
		    flag_var_addr
		      = add_initial_icv_to_list (GOMP_DEVICE_NUM_FOR_DEV,
						 envvars[omp_var].flag_vars[0],
						 params);
		  }
		else if (startswith (&name[pos], "_ALL=")
			 && envvars[omp_var].flag & GOMP_ENV_SUFFIX_ALL)
		  {
		    pos += 5;
		    flag_var_addr
		      = add_initial_icv_to_list (GOMP_DEVICE_NUM_FOR_ALL,
						 envvars[omp_var].flag_vars[0],
						 params);
		  }
		else if (startswith (&name[pos], "_DEV_")
			 && envvars[omp_var].flag & GOMP_ENV_SUFFIX_DEV_X)
		  {
		    pos += 5;
		    if (!get_device_num (*env, &name[pos], &dev_num,
					 &dev_num_len))
		      break;

		    pos += dev_num_len + 1;
		    flag_var_addr
		      = add_initial_icv_to_list (dev_num,
						 envvars[omp_var].flag_vars[0],
						 params);
		  }
		else
		  {
		    gomp_error ("Invalid environment variable in %s", *env);
		    break;
		  }
		env_val = &name[pos];

		if (envvars[omp_var].parse_func (*env, env_val, params))
		  {
		    for (i = 0; i < 3; ++i)
		      if (envvars[omp_var].flag_vars[i])
			gomp_set_icv_flag (flag_var_addr,
					   envvars[omp_var].flag_vars[i]);
		      else
			break;
		  }

		break;
	      }
	  }
      }

  all = gomp_get_initial_icv_item (GOMP_DEVICE_NUM_FOR_ALL);
  for (omp_var = 0; omp_var < OMP_HOST_VAR_CNT; omp_var++)
    {
      if (none != NULL
	  && gomp_get_icv_flag (none->flags, host_envvars[omp_var].flag_var))
	get_icv_member_addr (&none->icvs,
			     host_envvars[omp_var].flag_var, params);
      else if (all != NULL
	       && gomp_get_icv_flag (all->flags,
				     host_envvars[omp_var].flag_var))
	get_icv_member_addr (&all->icvs, host_envvars[omp_var].flag_var,
			     params);
      else
	continue;

      switch (host_envvars[omp_var].type_code)
	{
	case PARSE_INT:
	  for (i = 0; i < 3; ++i)
	    if (host_envvars[omp_var].dest[i] != NULL && params[i] != NULL)
	      *(int *) (host_envvars[omp_var].dest[i]) = *(int *) params[i];
	  break;
	case PARSE_BOOL:
	  for (i = 0; i < 3; ++i)
	    if (host_envvars[omp_var].dest[i] != NULL && params[i] != NULL)
	      *(bool *) (host_envvars[omp_var].dest[i]) = *(bool *) params[i];
	  break;
	case PARSE_UINT:
	  for (i = 0; i < 3; ++i)
	    if (host_envvars[omp_var].dest[i] != NULL && params[i] != NULL)
	      *(unsigned int *) (host_envvars[omp_var].dest[i])
		= *(unsigned int *) params[i];
	  break;
	case PARSE_ULONG:
	  for (i = 0; i < 3; ++i)
	    if (host_envvars[omp_var].dest[i] != NULL && params[i] != NULL)
	      *(unsigned long *) (host_envvars[omp_var].dest[i])
		= *(unsigned long *) params[i];
	  break;
	case PARSE_UCHAR:
	  for (i = 0; i < 3; ++i)
	    if (host_envvars[omp_var].dest[i] != NULL && params[i] != NULL)
	      *(unsigned char *) (host_envvars[omp_var].dest[i])
		= *(unsigned char *) params[i];
	  break;
	case PARSE_SCHEDULE:
	  *(enum gomp_schedule_type *) (host_envvars[omp_var].dest[0])
	    = *(enum gomp_schedule_type *) params[0];
	  *(int *) (host_envvars[omp_var].dest[1]) = *(int *) params[1];
	  break;
	case PARSE_BIND:
	  *(char *) (host_envvars[omp_var].dest[0]) = *(char *) params[0];
	  *(char **) (host_envvars[omp_var].dest[1]) = *(char **) params[1];
	  *(unsigned long *) (host_envvars[omp_var].dest[2])
	    = *(unsigned long *) params[2];
	  break;
	}
    }

  if (((none != NULL && gomp_get_icv_flag (none->flags, GOMP_ICV_BIND))
       || (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_BIND)))
      && gomp_global_icv.bind_var == omp_proc_bind_false)
    ignore = true;

  if (!((none != NULL
	 && gomp_get_icv_flag (none->flags, GOMP_ICV_MAX_ACTIVE_LEVELS))
       || (all != NULL
	   && gomp_get_icv_flag (all->flags, GOMP_ICV_MAX_ACTIVE_LEVELS))))
    {
      bool nested = true;
      const char *env = getenv ("OMP_NESTED");

      /* OMP_NESTED is deprecated in OpenMP 5.0.  */
      if (parse_boolean ("OMP_NESTED", env, (void *[]) {&nested}))
	gomp_global_icv.max_active_levels_var
	  = nested ? gomp_supported_active_levels : 1;
      else if (gomp_nthreads_var_list_len > 1 || gomp_bind_var_list_len > 1)
	gomp_global_icv.max_active_levels_var = gomp_supported_active_levels;
    }

  if (gomp_global_icv.default_device_var == INT_MIN
      && gomp_target_offload_var != GOMP_TARGET_OFFLOAD_MANDATORY)
    none->icvs.default_device_var = gomp_global_icv.default_device_var = 0;

  /* Process GOMP_* variables and dependencies between parsed ICVs.  */
  parse_int_secure ("GOMP_DEBUG", &gomp_debug_var, true);

  /* Make sure OMP_PLACES and GOMP_CPU_AFFINITY env vars are always
     parsed if present in the environment.  If OMP_PROC_BIND was set
     explicitly to false, don't populate places list though.  If places
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

  {
    const char *env = getenv ("OMP_AFFINITY_FORMAT");
    if (env != NULL)
      gomp_set_affinity_format (env, strlen (env));
  }

  if (none != NULL && gomp_get_icv_flag (none->flags, GOMP_ICV_WAIT_POLICY))
    wait_policy = none->icvs.wait_policy;
  else if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_WAIT_POLICY))
    wait_policy = all->icvs.wait_policy;

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
	{
	  gomp_spin_count_var = 300000LL;
	  do_adjust_default_spincount ();
	}
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

  if (!(none != NULL && gomp_get_icv_flag (none->flags, GOMP_ICV_STACKSIZE)))
    {
      const char *env = getenv ("GOMP_STACKSIZE");
      if (env != NULL
	  && parse_stacksize ("GOMP_STACKSIZE", env,
			      (void *[3]) {&none->icvs.stacksize}))
	gomp_set_icv_flag (&none->flags, GOMP_ICV_STACKSIZE);
    }
  if (none != NULL && gomp_get_icv_flag (none->flags, GOMP_ICV_STACKSIZE))
    stacksize = none->icvs.stacksize;
  else if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_STACKSIZE))
    stacksize = all->icvs.stacksize;

  if ((none != NULL && gomp_get_icv_flag (none->flags, GOMP_ICV_STACKSIZE))
      || (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_STACKSIZE))
      || GOMP_DEFAULT_STACKSIZE)
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

  handle_omp_display_env ();

  /* OpenACC.  */

  if (!parse_int ("ACC_DEVICE_NUM", getenv ("ACC_DEVICE_NUM"),
		  (void *[]) {&goacc_device_num, (void *) true}))
    goacc_device_num = 0;

  parse_acc_device_type ();
  parse_gomp_openacc_dim ();

  goacc_runtime_initialize ();

  goacc_profiling_initialize ();
}
#endif /* LIBGOMP_OFFLOADED_ONLY */
