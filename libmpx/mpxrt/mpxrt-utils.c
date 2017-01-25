/* mpxrt-utils.c                  -*-C++-*-
 *
 *************************************************************************
 *
 *  @copyright
 *  Copyright (C) 2014, Intel Corporation
 *  All rights reserved.
 *
 *  @copyright
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *    * Neither the name of Intel Corporation nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  @copyright
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 *  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 *  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 *  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 **************************************************************************/

#define __STDC_FORMAT_MACROS
#include "config.h"
#include <inttypes.h>
#include <unistd.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <pthread.h>
#include "mpxrt-utils.h"

#ifndef HAVE_SECURE_GETENV
#define secure_getenv __secure_getenv
#endif

#define MPX_RT_OUT "CHKP_RT_OUT_FILE"
#define MPX_RT_ERR "CHKP_RT_ERR_FILE"
#define MPX_RT_VERBOSE "CHKP_RT_VERBOSE"
#define MPX_RT_VERBOSE_DEFAULT VERB_BR
#define MPX_RT_MODE "CHKP_RT_MODE"
#define MPX_RT_MODE_DEFAULT MPX_RT_COUNT
#define MPX_RT_MODE_DEFAULT_STR "count"
#define MPX_RT_STOP_HANDLER "CHKP_RT_STOP_HANDLER"
#define MPX_RT_STOP_HANDLER_DEFAULT MPX_RT_STOP_HANDLER_ABORT
#define MPX_RT_STOP_HANDLER_DEFAULT_STR "abort"
#define MPX_RT_HELP "CHKP_RT_HELP"
#define MPX_RT_ADDPID "CHKP_RT_ADDPID"
#define MPX_RT_BNDPRESERVE "CHKP_RT_BNDPRESERVE"
#define MPX_RT_BNDPRESERVE_DEFAULT 0
#define MPX_RT_PRINT_SUMMARY "CHKP_RT_PRINT_SUMMARY"

#define MAX_FILE_NAME PATH_MAX

typedef struct env_var_s {
  char *env_name;
  char *env_val;
  struct env_var_s *next;
} env_var_t;

typedef struct {
  env_var_t *first;
  env_var_t *last;
} env_var_list_t;

/* Following vars are initialized at process startup only
   and thus are considered to be thread safe.  */
static int summary;
static int add_pid;
static mpx_rt_mode_t mode;
static mpx_rt_stop_mode_handler_t stop_handler;
static env_var_list_t env_var_list;
static verbose_type verbose_val;
static FILE *out;
static FILE *err;
static char out_name[MAX_FILE_NAME];
static char err_name[MAX_FILE_NAME];

/* Following vars are read at process finalization only.
   All write accesses use the same value and thus are
   considered to be thread safe.  */
static int out_file_dirty;
static int err_file_dirty;
static int files_overwritten;

/* Mutex used to sync output.  */
static pthread_mutex_t lock;

static void *
malloc_check (size_t size)
{
  void *res = malloc (size);
  if (!res)
    __mpxrt_print (VERB_ERROR, "Couldn't allocate %zu bytes.", size);
  else
    memset (res, 0, size);
  return res;
}

static void
env_var_list_add (const char* env, const char* val)
{
  env_var_t* n;

  if (val == 0)
    return;

  n = (env_var_t *)malloc_check (sizeof (env_var_t));
  if (!n)
    return;

  if (env_var_list.first == 0)
    env_var_list.first = n;

  if (env_var_list.last)
    env_var_list.last->next = n;

  env_var_list.last = n;

  n->env_name = (char *)malloc_check (strlen (env) + 1);
  n->env_val = (char *)malloc_check (strlen (val) + 1);

  if (!n->env_name || !n->env_val)
    return;

  strcpy (n->env_name, env);
  strcpy (n->env_val, val);
}

static void
set_file_stream (FILE** file, char* file_name,
		 const char* env, FILE* deflt)
{
  int pid;
  if (env != 0)
    {
      if (add_pid)
	{
	  pid = getpid ();
	  snprintf (file_name, MAX_FILE_NAME, "%s.%d", env, pid);
	}
      else
	snprintf (file_name, MAX_FILE_NAME, "%s", env);

      *file = fopen (file_name, "we");
      if (*file != 0)
	return;
    }
  *file = deflt;
}

/*
 * this function will be called after fork in the child
 * open new files with pid of the process
 */
static void
open_child_files ()
{
  char *out_env;
  char *err_env;

  out_env = secure_getenv (MPX_RT_OUT);
  err_env = secure_getenv (MPX_RT_ERR);

  if (add_pid == 0 && (out_env != 0 || err_env != 0))
    {
      __mpxrt_print (VERB_ERROR, "MPX RUNTIME WARNING: out/err files are "
		     "overwritten in new processes since %s was not set.\n",
		     MPX_RT_ADDPID);
      files_overwritten = 1;
    }

  set_file_stream (&out, out_name, out_env, stdout);
  if (out_env == 0 || err_env == 0 || (strcmp (out_env, err_env) != 0))
    set_file_stream (&err, err_name, err_env, stderr);
  else
    /* in case we get the same file name for err and out */
    err = out;
}

/*
 * this function is called after fork in the parent
 */
static void
at_fork_check (void)
{
  char *out_env;
  char *err_env;

  out_env = secure_getenv (MPX_RT_OUT);
  err_env = secure_getenv (MPX_RT_ERR);

  if (add_pid == 0 && (out_env != 0 || err_env != 0))
    files_overwritten = 1;
}

static mpx_rt_mode_t
set_mpx_rt_mode (const char *env)
{
  if (env == 0)
    return MPX_RT_MODE_DEFAULT;
  else if (strcmp (env, "stop") == 0)
    return MPX_RT_STOP;
  else if (strcmp (env,"count") == 0)
    return MPX_RT_COUNT;
  {
    __mpxrt_print (VERB_ERROR, "Illegal value '%s' for %s. Legal values are"
		   "[stop | count]\nUsing default value %s\n",
		   env, MPX_RT_MODE, MPX_RT_MODE_DEFAULT_STR);
    return MPX_RT_MODE_DEFAULT;
  }
}

static mpx_rt_stop_mode_handler_t
set_mpx_rt_stop_handler (const char *env)
{
  if (env == 0)
    return MPX_RT_STOP_HANDLER_DEFAULT;
  else if (strcmp (env, "abort") == 0)
    return MPX_RT_STOP_HANDLER_ABORT;
  else if (strcmp (env, "exit") == 0)
    return MPX_RT_STOP_HANDLER_EXIT;
  {
    __mpxrt_print (VERB_ERROR, "Illegal value '%s' for %s. Legal values are"
		   "[abort | exit]\nUsing default value %s\n",
		   env, MPX_RT_STOP_HANDLER, MPX_RT_STOP_HANDLER_DEFAULT);
    return MPX_RT_STOP_HANDLER_DEFAULT;
  }
}

static void
print_help (void)
{
  fprintf (out, "MPX Runtime environment variables help.\n");

  fprintf (out, "%s \t set output file for info & debug [default: stdout]\n",
	   MPX_RT_OUT);
  fprintf (out, "%s \t set output file for error [default: stderr]\n",
	   MPX_RT_ERR);
  fprintf (out, "%s \t set verbosity type [default: %d]\n"
	   "\t\t\t 0 - print only internal run time errors\n"
	   "\t\t\t 1 - just print summary\n"
	   "\t\t\t 2 - print summary and bound violation information\n "
	   "\t\t\t 3 - print debug information\n",
	   MPX_RT_VERBOSE, MPX_RT_VERBOSE_DEFAULT);
  fprintf (out, "%s \t\t set MPX runtime behavior on #BR exception."
	   " [stop | count]\n"
	   "\t\t\t [default: %s]\n", MPX_RT_MODE, MPX_RT_MODE_DEFAULT_STR);
  fprintf (out, "%s \t set the handler function MPX runtime will call\n"
           "\t\t\t on #BR exception when %s is set to \'stop\'."
	   " [abort | exit]\n"
	   "\t\t\t [default: %s]\n", MPX_RT_STOP_HANDLER, MPX_RT_MODE,
           MPX_RT_STOP_HANDLER_DEFAULT_STR);
  fprintf (out, "%s \t\t generate out,err file for each process.\n"
	   "\t\t\t generated file will be MPX_RT_{OUT,ERR}_FILE.pid\n"
	   "\t\t\t [default: no]\n", MPX_RT_ADDPID);
  fprintf (out, "%s \t set value for BNDPRESERVE bit.\n"
	   "\t\t\t BNDPRESERVE = 0 flush bounds on unprefixed call/ret/jmp\n"
	   "\t\t\t BNDPRESERVE = 1 do NOT flush bounds\n"
	   "\t\t\t [default: %d]\n", MPX_RT_BNDPRESERVE,
	   MPX_RT_BNDPRESERVE_DEFAULT);
  fprintf (out, "%s \t print summary at the end of the run\n"
	   "\t\t\t [default: no]\n", MPX_RT_PRINT_SUMMARY);

  fprintf (out, "%s \t\t print this help and exit.\n"
	   "\t\t\t [default: no]\n", MPX_RT_HELP);

  exit (0);
}

static void
validate_bndpreserve (const char *env, int *bndpreserve)
{
  if (env == 0)
    bndpreserve = MPX_RT_BNDPRESERVE_DEFAULT;
  else if (strcmp (env, "0") == 0)
    *bndpreserve = 0;
  else if (strcmp (env, "1") == 0)
    *bndpreserve = 1;
  else
    {
      __mpxrt_print (VERB_ERROR, "Illegal value '%s' for %s. Legal values "
		     "are [0 | 1]\nUsing default value %d\n",
		     env, MPX_RT_BNDPRESERVE, MPX_RT_BNDPRESERVE_DEFAULT);
      *bndpreserve = MPX_RT_BNDPRESERVE_DEFAULT;
    }
}

static verbose_type
init_verbose_val (const char *env)
{
  if (env == 0)
    return MPX_RT_VERBOSE_DEFAULT;
  else if (strcmp(env, "0") == 0)
    return VERB_ERROR;
  else if (strcmp(env, "1") == 0)
    return VERB_INFO;
  else if (strcmp(env, "2") == 0)
    return VERB_BR;
  else if (strcmp(env, "3") == 0)
    return VERB_DEBUG;

  __mpxrt_print (VERB_ERROR, "Illegal value '%s' for %s. Legal values "
		 "are [0..3]\nUsing default value %d\n",
		 env, MPX_RT_VERBOSE, (int)MPX_RT_VERBOSE_DEFAULT);

  return MPX_RT_VERBOSE_DEFAULT;
}

static void
env_var_print_summary (void)
{
  env_var_t* node;

  __mpxrt_print (VERB_DEBUG, "Used environment variables:\n");

  node = env_var_list.first;
  while (node != 0)
    {
      __mpxrt_print (VERB_DEBUG, "  %s = %s\n", node->env_name, node->env_val);
      node = node->next;
    }
}

/* Return 1 if passes env var value should enable feature.  */

static int
check_yes (const char *val)
{
  return val && (!strcmp (val, "yes") || !strcmp (val, "1"));
}

void
__mpxrt_init_env_vars (int* bndpreserve)
{
  char *out_env;
  char *err_env;
  char *env;

  pthread_mutex_init (&lock, NULL);

  out_env = secure_getenv (MPX_RT_OUT);
  env_var_list_add (MPX_RT_OUT, out_env);

  err_env = secure_getenv (MPX_RT_ERR);
  env_var_list_add (MPX_RT_ERR, err_env);

  env = secure_getenv (MPX_RT_ADDPID);
  env_var_list_add (MPX_RT_ADDPID, env);
  add_pid = check_yes (env);

  set_file_stream (&out, out_name, out_env, stdout);
  if (out_env == 0 || err_env == 0 || (strcmp (out_env, err_env) != 0))
    set_file_stream (&err, err_name, err_env, stderr);
  else
    /* in case we get the same file name for err and out */
    err = out;

  env = secure_getenv (MPX_RT_VERBOSE);
  env_var_list_add (MPX_RT_VERBOSE, env);
  verbose_val = init_verbose_val (env);

  env = secure_getenv (MPX_RT_MODE);
  env_var_list_add (MPX_RT_MODE, env);
  mode = set_mpx_rt_mode (env);

  env = secure_getenv (MPX_RT_STOP_HANDLER);
  env_var_list_add (MPX_RT_STOP_HANDLER, env);
  stop_handler = set_mpx_rt_stop_handler (env);

  env = secure_getenv (MPX_RT_BNDPRESERVE);
  env_var_list_add (MPX_RT_BNDPRESERVE, env);
  validate_bndpreserve (env, bndpreserve);

  env = secure_getenv (MPX_RT_PRINT_SUMMARY);
  env_var_list_add (MPX_RT_PRINT_SUMMARY, env);
  summary = check_yes (env);

  env = secure_getenv (MPX_RT_HELP);
  if (check_yes (env))
    print_help ();

  /*
   * at fork - create new files for output and err according
   * to the env vars.
   */
  pthread_atfork (NULL, at_fork_check, open_child_files);

  env_var_print_summary ();
}

void
__mpxrt_utils_free (void)
{
  if (files_overwritten)
    __mpxrt_print (VERB_INFO, "\nMPX RUNTIME WARNING: out/err files are"
		   " overwritten in new processes since %s was not set.\n",
		   MPX_RT_ADDPID);

  if (out != stdout)
    {
      fclose (out);
      if (out_file_dirty != 1)
	remove (out_name);
    }

  if (err != stderr)
    {
      fclose (err);
      if (err_file_dirty != 1)
	remove (err_name);
    }

  pthread_mutex_destroy (&lock);
}

void
__mpxrt_write_uint (verbose_type vt, uint64_t val, unsigned base)
{
  static const char digits[] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
  char str[65];
  int pos = 64;;

  str[pos--] = 0;

  if (vt > verbose_val || base <= 1 || base > sizeof (digits))
    return;

  if (val < base)
    str[pos--] = digits[val];
  else
    while (val)
      {
	str[pos--] = digits[val % base];
	val = val / base;
      }

  __mpxrt_write (vt, str + pos + 1);
}

void
__mpxrt_write (verbose_type vt, const char* str)
{
  va_list argp;
  FILE *print_to;

  if (vt > verbose_val)
    return;

  if (vt == VERB_ERROR)
    {
      print_to = err;
      err_file_dirty = 1;
    }
  else
    {
      print_to = out;
      out_file_dirty = 1;
    }
  pthread_mutex_lock (&lock);
  write (fileno (print_to), str, strlen (str));
  pthread_mutex_unlock (&lock);
  va_end (argp);
}

void
__mpxrt_print (verbose_type vt, const char* frmt, ...)
{
  va_list argp;
  FILE *print_to;

  if (vt > verbose_val)
    return;

  va_start (argp, frmt);
  if (vt == VERB_ERROR)
    {
      print_to = err;
      err_file_dirty = 1;
    }
  else
    {
      print_to = out;
      out_file_dirty = 1;
    }
  pthread_mutex_lock (&lock);
  vfprintf (print_to, frmt, argp);
  fflush (print_to);
  pthread_mutex_unlock (&lock);
  va_end (argp);
}

mpx_rt_mode_t
__mpxrt_mode (void)
{
  return mode;
}

mpx_rt_mode_t
__mpxrt_stop_handler (void)
{
  return stop_handler;
}

void __attribute__ ((noreturn))
__mpxrt_stop (void)
{
  if (__mpxrt_stop_handler () == MPX_RT_STOP_HANDLER_ABORT)
    abort ();
  else if (__mpxrt_stop_handler () == MPX_RT_STOP_HANDLER_EXIT)
    exit (255);
  __builtin_unreachable ();
}

void
__mpxrt_print_summary (uint64_t num_brs, uint64_t l1_size)
{

  if (summary == 0)
    return;

  out_file_dirty = 1;

  pthread_mutex_lock (&lock);
  fprintf (out, "MPX runtime summary:\n");
  fprintf (out, "  Number of bounds violations: %" PRIu64 ".\n", num_brs);
  fprintf (out, "  Size of allocated L1: %" PRIu64 "B\n", l1_size);
  fflush (out);
  pthread_mutex_unlock (&lock);
}
