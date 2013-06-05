/* Copyright (C) 2012-2013
   Free Software Foundation, Inc.
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
 * @file gupcr_utils.c
 * GUPC Runtime utility routines
 */

/**
 * @addtogroup GUPCUTILS GUPCR Utility Functions
 * @{
 */

#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_portals.h"
#include "gupcr_runtime.h"
#include "gupcr_utils.h"

#define PATH_SEP '/'

typedef struct gupcr_open_file_struct *gupcr_open_file_ref;
typedef struct gupcr_open_file_struct
{
  gupcr_open_file_ref next;
  FILE *file_ptr;
  const char *pathname;
} gupcr_open_file_t;

static void gupcr_close_all_open_files (void);
static void gupcr_debug_fini (void);
static void gupcr_debug_init (void);
static FILE *gupcr_find_open_file (const char *pathname);
static void gupcr_open_file_list_add (FILE * file, const char *pathname);
static void gupcr_set_short_pgm_name (void);
static void gupcr_stats_fini (void);
static void gupcr_stats_init (void);
static void gupcr_trace_fini (void);
static void gupcr_trace_init (void);
static void gupcr_write_log (FILE * file, const char *filename,
			     const char *fmt, va_list ap);

static int gupcr_is_init;
static int gupcr_error_count;
static const char *gupcr_pgm_name = "";
static char gupcr_short_pgm_name[13];
static int gupcr_pid;
static char gupcr_pid_string[13];
static int gupcr_inform_user = 1;
static int gupcr_warn_user = 1;
static size_t gupcr_shared_heap_size;
static int gupcr_node_local_memory = 1;
static int gupcr_forcetouch = 1;

static gupcr_open_file_ref gupcr_open_files_list;
static int gupcr_debug_enabled;
static const char *gupcr_debug_filename = "stderr";
static FILE *gupcr_debug_file;
static int gupcr_log_enabled;
static const char *gupcr_log_filename = "stderr";
static FILE *gupcr_log_file;
static int gupcr_stats_enabled;
static const char *gupcr_stats_filename = "stderr";
static FILE *gupcr_stats_file __attribute ((unused));
static int gupcr_trace_enabled;
static const char *gupcr_trace_filename = "stderr";
static FILE *gupcr_trace_file;

gupcr_facility_t gupcr_debug_facility_mask;
gupcr_facility_t gupcr_log_facility_mask;
gupcr_facility_t gupcr_stats_facility_mask;
gupcr_facility_t gupcr_trace_facility_mask;

const char *
gupcr_strsignal (int sig)
{
  return strsignal (sig);
}

const char *
gupcr_strerror (void)
{
  return strerror (errno);
}

void
gupcr_signal_enable (int signal, void (*handler) (int))
{
  struct sigaction action;

  action.sa_handler = handler;
  sigemptyset (&action.sa_mask);
  action.sa_flags = 0;
  sigaction (signal, &action, NULL);
}

void
gupcr_signal_disable (int signal)
{
  struct sigaction action;

  action.sa_handler = SIG_IGN;
  sigemptyset (&action.sa_mask);
  action.sa_flags = 0;
  sigaction (signal, &action, NULL);
}

static void
gupcr_write_log (FILE * file, const char *filename,
		 const char *fmt, va_list ap)
{
  const double timestamp = gupcr_clock ();
  static double prev_timestamp;
  double delta_timestamp;
  char buf[1024];
  char *bp = buf;
  va_list args;
  delta_timestamp = timestamp - prev_timestamp;
  prev_timestamp = timestamp;
  bp += sprintf (bp, "%lld ", (long long) (timestamp * 1.0e6));
  bp += sprintf (bp, "%#.3g%s ",
		 ((delta_timestamp > 1.0) ? 1.0
		  : ((delta_timestamp > 0.001) ? 1.0e3 : 1.0e6))
		 * delta_timestamp,
		 ((delta_timestamp > 1.0) ? "s"
		  : ((delta_timestamp > 0.001) ? "m" : "u")));
  if (MYTHREAD >= 0 && MYTHREAD < THREADS)
    bp += sprintf (bp, "%d ", MYTHREAD);
  else
    bp += sprintf (bp, "??? ");
  va_copy (args, ap);
  bp += vsprintf (bp, fmt, args);
  va_end (args);
  if (fputs (buf, file) == EOF)
    gupcr_abort_with_msg ("UPC runtime write to `%s' failed: %s",
			  filename, gupcr_strerror ());
}

void
gupcr_abort (void)
{
  gupcr_utils_fini ();
  abort ();
}

/* NOTE: registered as an exit routine */
void
gupcr_exit ()
{
  if (gupcr_finalize_ok)
    gupcr_fini ();
  else
    {
      gupcr_runtime_fini ();
      gupcr_utils_fini ();
    }
}

void
gupcr_shutdown (int exit_code)
{
  exit (exit_code);
}

void
gupcr_abort_with_msg (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vfprintf (stderr, fmt, args);
  va_end (args);
  gupcr_abort ();
}

const char *
gupcr_get_pid_as_string (void)
{
  return gupcr_pid_string;
}

const char *
gupcr_get_pgm_name (void)
{
  return gupcr_pgm_name;
}

void
gupcr_set_pgm_name (const char *pgm_name)
{
  gupcr_pgm_name = pgm_name;
}

void
gupcr_init_complete (void)
{
  gupcr_is_init = 1;
  if (gupcr_error_count)
    gupcr_abort ();
}

void
gupcr_be_quiet (void)
{
  gupcr_inform_user = 0;
  gupcr_warn_user = 0;
}

void
gupcr_no_warn (void)
{
  gupcr_warn_user = 0;
}

void
gupcr_set_shared_heap_size (size_t heap_size)
{
  gupcr_shared_heap_size = heap_size;
}

size_t
gupcr_get_shared_heap_size (void)
{
  return gupcr_shared_heap_size;
}

void
gupcr_set_node_local_memory (int value)
{
  gupcr_node_local_memory = value;
}

int
gupcr_is_node_local_memory_enabled (void)
{
  return gupcr_node_local_memory;
}

void
gupcr_set_forcetouch (int value)
{
  gupcr_forcetouch = value;
}

int
gupcr_is_forcetouch_enabled (void)
{
  return gupcr_forcetouch;
}

/** Node local unique name.  */
#define GUPCR_LOCAL_NAME_FMT "-%06d-%06d"
#define GUPCR_LOCAL_NAME_FMT_SIZE 14

void
gupcr_unique_local_name (char *fname, const char *prefix, int thread,
			 int tmpenv)
{
  const char *tmpdir = NULL;
  char *n = fname;
  *n = 0;
  if (tmpenv)
    {
      /* Honor TMDIR and/or TMP environment variables.  */
      if ((getuid () == geteuid ()) && (getgid () == getegid ()))
	{
	  tmpdir = getenv ("TMPDIR");
	  if (!tmpdir)
	    tmpdir = getenv ("TMP");
	}
      if (!tmpdir)
	tmpdir = "/tmp";
      if (strlen (tmpdir) > FILENAME_MAX)
	gupcr_fatal_error ("tmp path too long: %s", tmpdir);
      strcpy (n, tmpdir);
      n = n + strlen (n);
      strcat (n, "/");
      n = n + 1;
    }
  if (strlen (n) + strlen (prefix) + GUPCR_LOCAL_NAME_FMT_SIZE > FILENAME_MAX)
    gupcr_fatal_error ("unique local name too long");
  sprintf (n, "%s" GUPCR_LOCAL_NAME_FMT, prefix, thread,
	   gupcr_get_rank_pid (thread));
}

/* Convert buffer to string (max of 16 characters) */
const char *
gupcr_get_buf_as_hex (char *bufstr, const void *buf, size_t size)
{
  size_t cnt = (size > 16) ? 16 : size;
  bufstr[0] = 0;
  if (cnt > 0)
    {
      char *tmp = bufstr;
      size_t i;
      for (i=0; i<cnt-1; ++i)
	{
	  sprintf (tmp, "%02x ", ((unsigned char *)buf)[i]);
	  tmp += 3;
	}
      sprintf (tmp, "%02x", ((unsigned char *)buf)[cnt-1]);
    }
  return bufstr;
}

void
gupcr_error_print (const char *fmt, ...)
{
  va_list args;
  gupcr_assert (MYTHREAD >= 0);
  /* If the runtime has not initialized yet, print the
     error message on thread 0 only.  */
  if (gupcr_is_init || MYTHREAD == 0)
    {
      va_start (args, fmt);
      vfprintf (stderr, fmt, args);
      va_end (args);
    }
  /* Abort is deferred until the runtime has initialized
     so that all non-fatal error diagnostics are printed
     before the program aborts.  */
  if (gupcr_is_init)
    gupcr_abort ();
  ++gupcr_error_count;
}

void
gupcr_warn_print (const char *fmt, ...)
{
  if (gupcr_warn_user)
    {
      va_list args;
      va_start (args, fmt);
      vfprintf (stderr, fmt, args);
      va_end (args);
    }
}

void
gupcr_info_print (const char *fmt, ...)
{
  if (gupcr_inform_user)
    {
      va_list args;
      va_start (args, fmt);
      vfprintf (stderr, fmt, args);
      va_end (args);
    }
}

void
gupcr_debug_print (const char *fmt, ...)
{
  if (gupcr_debug_enabled)
    {
      va_list args;
      if (!gupcr_debug_file)
	{
	  gupcr_assert (gupcr_debug_filename != NULL);
	  gupcr_debug_file = gupcr_fopen ("debug", gupcr_debug_filename, "w");
	}
      va_start (args, fmt);
      gupcr_write_log (gupcr_debug_file, gupcr_debug_filename, fmt, args);
      va_end (args);
    }
}

void
gupcr_set_debug_facility (gupcr_facility_t facility_mask)
{
  gupcr_debug_facility_mask = facility_mask;
  gupcr_debug_enabled = (facility_mask != 0);
}

void
gupcr_set_debug_filename (const char *filename)
{
  gupcr_assert (filename != NULL);
  gupcr_debug_filename = filename;
}

void
gupcr_log_print (const char *fmt, ...)
{
  if (gupcr_log_enabled)
    {
      va_list args;
      if (!gupcr_log_file)
	{
	  gupcr_assert (gupcr_log_filename != NULL);
	  gupcr_log_file = gupcr_fopen ("debug", gupcr_log_filename, "w");
	}
      va_start (args, fmt);
      gupcr_write_log (gupcr_log_file, gupcr_log_filename, fmt, args);
      va_end (args);
    }
}

void
gupcr_set_log_facility (gupcr_facility_t facility_mask)
{
  gupcr_log_facility_mask = facility_mask;
  gupcr_log_enabled = (facility_mask != 0);
}

void
gupcr_set_log_filename (const char *filename)
{
  gupcr_assert (filename != NULL);
  gupcr_log_filename = filename;
}

void
gupcr_set_stats_facility (gupcr_facility_t facility_mask)
{
  gupcr_stats_facility_mask = facility_mask;
  gupcr_stats_enabled = (facility_mask != 0);
}

void
gupcr_set_stats_filename (const char *filename)
{
  gupcr_assert (filename != NULL);
  gupcr_stats_filename = filename;
}

void
gupcr_trace_print (const char *fmt, ...)
{
  if (gupcr_trace_enabled)
    {
      va_list args;
      if (!gupcr_trace_file)
	{
	  gupcr_assert (gupcr_trace_filename != NULL);
	  gupcr_trace_file = gupcr_fopen ("trace", gupcr_trace_filename, "w");
	}
      va_start (args, fmt);
      gupcr_write_log (gupcr_trace_file, gupcr_trace_filename, fmt, args);
      va_end (args);
    }
}

void
gupcr_set_trace_facility (gupcr_facility_t facility_mask)
{
  gupcr_trace_facility_mask = facility_mask;
  gupcr_trace_enabled = (facility_mask != 0);
}

void
gupcr_set_trace_filename (const char *filename)
{
  gupcr_assert (filename != NULL);
  gupcr_trace_filename = filename;
}

static void
gupcr_set_short_pgm_name (void)
{
  char *pgm_name, *filename, *cp;
  gupcr_strdup (pgm_name, gupcr_pgm_name);
  filename = basename (pgm_name);
  gupcr_assert ((sizeof (gupcr_short_pgm_name) - 1) == 12);
  strncpy (gupcr_short_pgm_name, filename, 12);
  gupcr_short_pgm_name[12] = '\0';
  for (cp = gupcr_short_pgm_name; *cp; ++cp)
    {
      if (!(isalnum (*cp) || *cp == '_' || *cp == '.'))
	*cp = '_';
    }
  gupcr_free (pgm_name);
}

static FILE *
gupcr_find_open_file (const char *pathname)
{
  gupcr_open_file_ref f;
  for (f = gupcr_open_files_list; f; f = f->next)
    {
      if (!strcmp (f->pathname, pathname))
	return f->file_ptr;
    }
  return NULL;
}

static void
gupcr_open_file_list_add (FILE * file, const char *pathname)
{
  gupcr_open_file_ref open_file;
  gupcr_assert (file != NULL);
  gupcr_assert (pathname != NULL && pathname[0]);
  gupcr_malloc (open_file, sizeof (gupcr_open_file_t));
  open_file->next = gupcr_open_files_list;
  gupcr_strdup (open_file->pathname, pathname);
  open_file->file_ptr = file;
  gupcr_open_files_list = open_file;
}

FILE *
gupcr_fopen (const char *purpose, const char *pathname, const char *mode)
{
  FILE *file, *already_open;
  already_open = gupcr_find_open_file (pathname);
  if (already_open)
    file = already_open;
  else
    {
      if (!strcmp (mode, "w"))
	/* Create any intervening directories.  */
	gupcr_mkpath (pathname);
      file = fopen (pathname, mode);
      if (!file)
	gupcr_error ("UPC runtime cannot open %s file `%s' for %s: %s",
		     purpose, pathname,
		     !strcmp (mode, "r") ? "read" : "write",
		     errno ? gupcr_strerror () : "unknown reason");
      gupcr_open_file_list_add (file, pathname);
    }
  return file;
}

static void
gupcr_close_all_open_files (void)
{
  gupcr_open_file_ref f, f_next;
  for (f = gupcr_open_files_list; f; f = f_next)
    {
      f_next = f->next;
      gupcr_assert (f->file_ptr != NULL);
      (void) fclose (f->file_ptr);
      gupcr_free ((void *) f->pathname);
      gupcr_free (f);
    }
}

void
gupcr_mkpath (const char *const path)
{
  char *path_dup, *dirpath;
  gupcr_strdup (path_dup, path);
  dirpath = dirname (path_dup);
  if (strcmp (dirpath, ".") != 0 && strcmp (dirpath, "/") != 0)
    {
      char *dir, *p;
      int nlevels;
      for (p = dirpath, nlevels = 0;
	   (dir = strtok (p, "/")); p = NULL, ++nlevels)
	{
	  gupcr_mkdir_unless_exists (dir);
	  gupcr_syscall (chdir, (dir));
	}
      while (nlevels--)
	gupcr_syscall (chdir, (".."));
    }
  gupcr_free (path_dup);
}

/* Parse the numeric string given by STR_ARG, and
   multiply it by any scaling factor ('K', 'M', or 'G')
   if present and return the result.  If the conversion
   is successful then *STATUS will be 0.  Otherwise,
	-1	There are no valid digits in the input string.
	-2	There are extra characters in the multiplier suffix.
	-3	The multiplier suffix specifies an invalid character.
	-4	The number is out-of-range.  */

long long
gupcr_strtoll (const char *const str,
	       long long int min_val, long long int max_val, int *status)
{
  long long result = 0;
  *status = 0;
  if (str && str[0])
    {
      char *suffix;
      errno = 0;
      result = strtoll (str, &suffix, 0);
      if (errno == ERANGE)
	/* underflow/overflow */
	*status = -4;
      else if (suffix[0])
	{
	  if (suffix == str)
	    *status = -1;
	  else if (suffix[1] && suffix[2])
	    *status = -2;
	  else if (suffix[1] && suffix[1] != 'B' && suffix[1] != 'b')
	    *status = -2;
	  else
	    {
	      int shift_count = 0;
	      long long int tmp_result = result;
	      switch (suffix[0])
		{
		case 'k':
		case 'K':
		  shift_count = 10;
		  break;
		case 'm':
		case 'M':
		  shift_count = 20;
		  break;
		case 'g':
		case 'G':
		  shift_count = 30;
		  break;
		case 't':
		case 'T':
		  shift_count = 40;
		  break;
		default:
		  *status = -3;
		  break;
		}
	      if (!*status && shift_count)
		{
		  const int is_neg = (tmp_result < 0);
		  unsigned int i;
		  tmp_result = is_neg ? -tmp_result : tmp_result;
		  if (tmp_result >= 0)
		    {
		      for (i = shift_count; i > 0; --i)
			{
			  tmp_result <<= 1;
			  if (tmp_result < 0 && !(is_neg && i == 1))
			    {
			      *status = -4;
			      break;
			    }
			}
		    }
		  else
		    *status = -4;
		  if (*status)
		    result = is_neg ? LLONG_MIN : LLONG_MAX;
		  else
		    result = is_neg ? -tmp_result : tmp_result;
		}
	    }
	}
      if (!*status && (result < min_val || result > max_val))
	*status = -4;
    }
  else
    *status = -1;
  return result;
}

/* Given a non-zero status returned from calling gupcr_strtoll()
   with the STR argument, print an informative error message.  */

void
gupcr_strtoll_error (const char *const str,
		     long long int val_min, long long int val_max, int status)
{
  gupcr_assert (status < 0 && status >= -4);
  switch (status)
    {
    case -1:
      gupcr_error ("there are no valid digits in " "the value: `%s'", str);
      break;
    case -2:
      gupcr_error ("there are extra characters in "
		   "the multiplier suffix: `%s'", str);
      break;
    case -3:
      gupcr_error ("invalid multiplier suffix; the suffix "
		   "may be K, M, G, or T: `%s'", str);
      break;
    case -4:
      gupcr_error ("the value `%s' is out of range; it must be "
		   "in the range %lld .. %lld", str, val_min, val_max);
      break;
    default:
      gupcr_fatal_error ("unreachable statement");
      break;
    }
}

static void
gupcr_debug_init (void)
{
  if (gupcr_debug_enabled)
    {
      gupcr_log (FC_ALL, "GUPCR debug enabled");
    }
}

static void
gupcr_stats_init (void)
{
  if (gupcr_stats_enabled)
    {
      gupcr_log (FC_ALL, "GUPCR stats enabled");
    }
}

static void
gupcr_trace_init (void)
{
  if (gupcr_trace_enabled)
    {
      gupcr_log (FC_ALL, "GUPCR trace enabled");
    }
}

static void
gupcr_debug_fini (void)
{
}

static void
gupcr_stats_fini (void)
{
}

static void
gupcr_trace_fini (void)
{
}

void
gupcr_utils_init (void)
{
  gupcr_pid = getpid ();
  sprintf (gupcr_pid_string, "%d", gupcr_pid);
  gupcr_shared_heap_size = GUPCR_DEFAULT_PER_THREAD_HEAP_SIZE;
  gupcr_set_short_pgm_name ();
  gupcr_inform_user = 1;
  gupcr_warn_user = 1;
  if (stdin)
    gupcr_open_file_list_add (stdin, "stdin");
  if (stdout)
    gupcr_open_file_list_add (stdout, "stdout");
  if (stderr)
    gupcr_open_file_list_add (stderr, "stderr");
  gupcr_env_init ();
  gupcr_debug_init ();
  gupcr_stats_init ();
  gupcr_trace_init ();
  gupcr_clock_init ();
}

void
gupcr_utils_fini (void)
{
  gupcr_trace_fini ();
  gupcr_stats_fini ();
  gupcr_debug_fini ();
  gupcr_close_all_open_files ();
}

/** @} */
