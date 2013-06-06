/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/**
 * @file gupcr_env.c
 * GUPC Runtime environment variables handling
 */

/**
 * @addtogroup GUPCUTILS GUPCR Utility Functions
 * @{
 */

/**

 UPC_DEBUG

	If set, specifies a list of "facilities" that
	will have debugging output logged.

 UPC_DEBUGFILE

	Path of log file where UPC runtime debug logs are written.

 UPC_FIRSTTOUCH

	Not used. Reserved for future use.

 UPC_FORCETOUCH

	Force the thread to touch every memory page in its own shared
	memory space on startup. This ensures the correct NUMA memory
	allocation. By default it is "YES".

 UPC_LOG

	If set, specifies a list of "facilities" that
	will be logged.

 UPC_LOGFILE

	Path of log file where UPC runtime logs are written.

 UPC_NO_WARN

	If set, the UPC_NO_WARN variable causes startup warnings (such as
	those displayed when debugging or tracing is enabled) to be omitted.

 UPC_NODE_LOCAL_MEM

	If set to "NO", then disable node local memory optimization.

 UPC_NODES

	Not used. Reserved for future use.

 UPC_QUIET

	UPC_QUIET causes all non-application-generated output to be omitted
	(including both warnings and the initial display of UPC thread
	layout).

 UPC_POLITE

	Yield the processor frequently while spin-locking.

 UPC_SHARED_HEAP_SIZE

	UPC_SHARED_HEAP_SIZE sets the amount of shared heap (per UPC thread)
	for your program.

 UPC_STATS

	If set, specifies a list of "facilities" for
	which UPC runtime statistics will be collected.

 UPC_STATSFILE

	Path of log file where UPC runtime statistics are written.

 UPC_TRACE

	If set, specifies a list of "facilities" that
	will be traced.

 UPC_TRACEFILE

	Path of log file where UPC trace logs are written.

 The set of facilities are:

	ADDR		UPC casts to local and access to PTS's.
	ALL		All the facilities
	ALLOC		UPC dynamic memory allocation
	ATOMIC		UPC atomic operations
	BARRIER 	UPC barrier/notify/wait operations
	BROADCAST	UPC runtime internal broadcast operations
	COLL		UPC collectives
	INFO		General information, program info.
	LOCKS		UPC lock operations
	MEM		UPC shared memory accesses
	MISC		Miscellaneous functions
	PORTALS		Portals operations
	SYSTEM		System calls

 For all environment variables above that set a filename path,
 each appearance of a single '%' will be substituted with the process
 pid.  Two '%'s together escape a single %.  Non-existent intermediate
 directories will be created.  As a special case, if the filename
 is "stdout" or "stderr", then output will be directed to the
 specified file descriptor.  A filename with no '%' indicates
 that the file will be shared across all processes.

*/

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"

static const struct gupcr_fc_tbl_struct
{
  const char *name;
  gupcr_facility_t mask;
}
gupcr_facility_table[] =
{
  {"addr", FC_ADDR},
  {"all", FC_ALL},
  {"alloc", FC_ALLOC},
  {"atomic", FC_ATOMIC},
  {"barrier", FC_BARRIER},
  {"broadcast", FC_BROADCAST},
  {"coll", FC_COLL},
  {"info", FC_INFO},
  {"locks", FC_LOCK},
  {"mem", FC_MEM},
  {"misc", FC_MISC},
  {"portals", FC_PORTALS},
  {"system", FC_SYSTEM}
};

#define GUPCR_FC_TBL_SIZE (sizeof (gupcr_facility_table) \
                           / sizeof (struct gupcr_fc_tbl_struct))
typedef enum
{
  ENV_NONE = 0,
  ENV_UPC_DEBUG,
  ENV_UPC_DEBUGFILE,
  ENV_UPC_FIRSTTOUCH,
  ENV_UPC_FORCETOUCH,
  ENV_UPC_LOG,
  ENV_UPC_LOGFILE,
  ENV_UPC_NO_WARN,
  ENV_UPC_NODE_LOCAL_MEM,
  ENV_UPC_NODES,
  ENV_UPC_POLITE,
  ENV_UPC_REQUIRE_SHARED_SIZE,
  ENV_UPC_QUIET,
  ENV_UPC_SHARED_HEAP_SIZE,
  ENV_UPC_STATS,
  ENV_UPC_STATSFILE,
  ENV_UPC_TRACE,
  ENV_UPC_TRACEFILE
} gupcr_env_kind;

static const struct gupcr_env_var_struct
{
  const char *name;
  gupcr_env_kind kind;
}
gupcr_env_var_table[] =
{
  {"UPC_DEBUG", ENV_UPC_DEBUG},
  {"UPC_DEBUGFILE", ENV_UPC_DEBUGFILE},
  {"UPC_FIRSTTOUCH", ENV_UPC_FIRSTTOUCH},
  {"UPC_FORCETOUCH", ENV_UPC_FORCETOUCH},
  {"UPC_LOG", ENV_UPC_LOG},
  {"UPC_LOGFILE", ENV_UPC_LOGFILE},
  {"UPC_NO_WARN", ENV_UPC_NO_WARN},
  {"UPC_NODE_LOCAL_MEM", ENV_UPC_NODE_LOCAL_MEM},
  {"UPC_NODES", ENV_UPC_NODES},
  {"UPC_POLITE", ENV_UPC_POLITE},
  {"UPC_REQUIRE_SHARED_SIZE", ENV_UPC_REQUIRE_SHARED_SIZE},
  {"UPC_QUIET", ENV_UPC_QUIET},
  {"UPC_SHARED_HEAP_SIZE", ENV_UPC_SHARED_HEAP_SIZE},
  {"UPC_STATS", ENV_UPC_STATS},
  {"UPC_STATSFILE", ENV_UPC_STATSFILE},
  {"UPC_TRACE", ENV_UPC_TRACE},
  {"UPC_TRACEFILE", ENV_UPC_TRACEFILE}
};

#define GUPCR_ENV_VAR_TBL_SIZE (sizeof (gupcr_env_var_table) \
                                / sizeof (struct gupcr_env_var_struct))

/* Look up the name given by FACILITY and return the facility mask value
   associated with that name.  */

static gupcr_facility_t
gupcr_facility_mask_for_name (const char *const facility)
{
  unsigned i;
  for (i = 0; i < GUPCR_FC_TBL_SIZE; ++i)
    {
      if (!strcasecmp (gupcr_facility_table[i].name, facility))
	return gupcr_facility_table[i].mask;
    }
  return FC_NONE;
}

/* Extract the environment variable name appearing before the
   first '=' sign in ENV_VAR_ARG; look it up in the list of
   known "UPC_" environment variables and return an
   integer value that is used to identify this particular
   environment variable name.  */

static gupcr_env_kind
gupcr_env_kind_for_var (const char *const env_var_arg)
{
  gupcr_env_kind env_kind = ENV_NONE;
  unsigned i;
  char *env_var_dup, *env_var;
  gupcr_strdup (env_var_dup, env_var_arg);
  env_var = strtok (env_var_dup, "=");
  gupcr_assert (env_var != NULL);
  for (i = 0; i < GUPCR_ENV_VAR_TBL_SIZE; ++i)
    {
      if (!strcmp (gupcr_env_var_table[i].name, env_var))
	return gupcr_env_var_table[i].kind;
    }
  gupcr_free (env_var_dup);
  return env_kind;
}

/* Process the comma separated list of facility names that
   appear after the '=' sign.  Return a mask value indicating
   which facility names were specified.  */

gupcr_facility_t
gupcr_env_facility_list (const char *const env_var_arg)
{
  gupcr_facility_t facility_mask = FC_NONE;
  char *env_var_dup, *env_var, *facility_name;
  gupcr_strdup (env_var_dup, env_var_arg);
  if ((env_var = strtok (env_var_dup, "=")))
    {
      while ((facility_name = strtok (NULL, ",")))
	{
	  gupcr_facility_t facility;
	  facility = gupcr_facility_mask_for_name (facility_name);
	  if (!facility)
	    gupcr_error ("invalid facility name `%s' found in "
			 "environment variable: `%s'",
			 facility_name, env_var_arg);
	  facility_mask |= facility;
	}
    }
  else
    gupcr_error ("invalid UPC environment variable syntax: `%s'", env_var);
  gupcr_free (env_var_dup);
  return facility_mask;
}

/* Return a malloc'd copy of ENV_VAR_STR_ARG with
   the current pid substituted for each occurrence of a '%'.
   Two '%'s next to each other are equivalent to a single '%'.  */

const char *
gupcr_env_filename (const char *const env_var_arg)
{
  char *env_var_dup, *env_var, *filename_arg;
  char *filename = NULL;
  gupcr_strdup (env_var_dup, env_var_arg);
  if ((env_var = strtok (env_var_dup, "=")))
    {
      if ((filename_arg = strtok (NULL, "")))
	{
	  const char *const pid = gupcr_get_pid_as_string ();
	  const char *cp;
	  char *fp;
	  size_t filename_len;
	  size_t pid_len = strlen (pid);
	  /* Calculate the required string size.  */
	  for (cp = filename_arg, filename_len = 0; *cp; ++cp)
	    {
	      if (cp[0] == '%' && cp[1] == '%')
		cp += 1, ++filename_len;
	      else if (cp[0] == '%')
		filename_len += pid_len;
	      else
		++filename_len;
	    }
	  /* Allocate the string; copy ENV_VAR_STR_ARG and
	     make '%' substitutions.   */
	  gupcr_malloc (filename, filename_len + 1);
	  for (fp = filename, cp = filename_arg; *cp; ++cp)
	    {
	      if (cp[0] == '%' && cp[1] == '%')
		cp += 1, *fp++ = '%';
	      else if (cp[0] == '%')
		strcpy (fp, pid), fp += pid_len;
	      else
		*fp++ = *cp;
	    }
	  *fp = '\0';
	}
      else
	gupcr_error ("missing file name in UPC environment "
		     "variable: `%s'", env_var_arg);
    }
  else
    gupcr_error ("invalid UPC environment variable syntax: `%s'",
		 env_var_arg);
  gupcr_free (env_var_dup);
  return filename;
}

static long long
gupcr_env_size (const char *const env_var_arg, long long int val_max)
{
  long long size = 0;
  char *env_var, *env_var_name, *size_str;
  gupcr_strdup (env_var, env_var_arg);
  if ((env_var_name = strtok (env_var, "=")))
    {
      if ((size_str = strtok (NULL, "")))
	{
	  int status;
	  size = gupcr_strtoll (size_str, 0, val_max, &status);
	  if (status)
	    {
	      gupcr_error ("invalid size specifier in UPC environment "
			   "variable: `%s'", env_var_arg);
	      gupcr_strtoll_error (size_str, 0, val_max, status);
	    }
	}
      else
	gupcr_error ("missing size specifier in UPC environment "
		     "variable: `%s'", env_var_arg);
    }
  else
    gupcr_error ("invalid UPC environment variable syntax: `%s'",
		 env_var_arg);
  gupcr_free (env_var);
  return size;
}

static int
gupcr_env_boolean (const char *const env_var_arg)
{
  int value = 0;
  char *env_var, *env_var_name, *switch_str;
  gupcr_strdup (env_var, env_var_arg);
  if ((env_var_name = strtok (env_var, "=")))
    {
      if ((switch_str = strtok (NULL, "")))
	{
	  if (!strcmp (switch_str, "NO") || \
	      !strcmp (switch_str, "no") || \
	      !strcmp (switch_str, "0"))
	    value = 0;
	  else if (!strcmp (switch_str, "YES") || \
		   !strcmp (switch_str, "yes") || \
		   !strcmp (switch_str, "1"))
	    value = 1;
	  else
	    {
	      gupcr_error ("invalid value specifier in UPC environment "
			   "variable: `%s'", env_var_arg);
	    }
	}
      else
	gupcr_error ("missing value specifier in UPC environment "
		     "variable: `%s'", env_var_arg);
    }
  else
    gupcr_error ("invalid UPC environment variable syntax: `%s'",
		 env_var_arg);
  gupcr_free (env_var);
  return value;
}

/* Process all variables in the environment that begin with "UPC_".
   Make various calls back into "gupcr_utils.c"  to implement
   the actions associated with each given environment variable.  */

void
gupcr_env_init (void)
{
  /* System environment, see:  environ (7).  */
  extern char **environ;
  const char *env_var;
  unsigned i;
  for (i = 0; (env_var = environ[i]); ++i)
    {
      if (!strncmp (env_var, "UPC_", 4))
	{
	  const int env_kind = gupcr_env_kind_for_var (env_var);
	  gupcr_facility_t facility_mask;
	  const char *filename;
	  size_t heap_size;
	  switch (env_kind)
	    {
	    case ENV_UPC_DEBUG:
	      facility_mask = gupcr_env_facility_list (env_var);
	      if (facility_mask)
		gupcr_set_debug_facility (facility_mask);
	      break;
	    case ENV_UPC_DEBUGFILE:
	      filename = gupcr_env_filename (env_var);
	      if (filename)
		gupcr_set_debug_filename (filename);
	      break;
	    case ENV_UPC_FIRSTTOUCH:
	      /* no-op */
	      break;
	    case ENV_UPC_FORCETOUCH:
	      gupcr_set_forcetouch (gupcr_env_boolean (env_var));
	      break;
	    case ENV_UPC_LOG:
	      facility_mask = gupcr_env_facility_list (env_var);
	      if (facility_mask)
		gupcr_set_log_facility (facility_mask);
	      break;
	    case ENV_UPC_LOGFILE:
	      filename = gupcr_env_filename (env_var);
	      if (filename)
		gupcr_set_log_filename (filename);
	      break;
	    case ENV_UPC_NO_WARN:
	      gupcr_no_warn ();
	      break;
	    case ENV_UPC_NODE_LOCAL_MEM:
	      gupcr_set_node_local_memory (gupcr_env_boolean (env_var));
	      break;
	    case ENV_UPC_NODES:
	      /* no-op */
	      break;
	    case ENV_UPC_POLITE:
	      /* no-op */
	      break;
	    case ENV_UPC_QUIET:
	      gupcr_be_quiet ();
	      break;
	    case ENV_UPC_SHARED_HEAP_SIZE:
	      heap_size = (size_t) gupcr_env_size (env_var,
						   GUPCR_MAX_HEAP_SIZE);
	      gupcr_set_shared_heap_size (heap_size);
	      break;
	    case ENV_UPC_STATS:
	      facility_mask = gupcr_env_facility_list (env_var);
	      gupcr_set_stats_facility (facility_mask);
	      break;
	    case ENV_UPC_STATSFILE:
	      filename = gupcr_env_filename (env_var);
	      if (filename)
		gupcr_set_stats_filename (filename);
	      break;
	    case ENV_UPC_TRACE:
	      facility_mask = gupcr_env_facility_list (env_var);
	      gupcr_set_trace_facility (facility_mask);
	      break;
	    case ENV_UPC_TRACEFILE:
	      filename = gupcr_env_filename (env_var);
	      if (filename)
		gupcr_set_trace_filename (filename);
	      break;
	    case ENV_UPC_REQUIRE_SHARED_SIZE:
	      /* no-op */
	      break;
	    case ENV_NONE:
	      gupcr_warn ("unknown UPC environment variable: %s", env_var);
	      break;
	    default:
	      gupcr_fatal_error ("env var. case value out of range");
	    }
	}
    }
}

/** @} */
