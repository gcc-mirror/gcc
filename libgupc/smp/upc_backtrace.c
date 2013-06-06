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

#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_sup.h"
#include "upc_access.h"
#include "upc_backtrace.h"
#include <signal.h>
#include <string.h>
#if HAVE_EXECINFO_H
#include <execinfo.h>
#endif
#if HAVE_LIMITS_H
#include <limits.h>
#endif

/** Skip over frames belonging to the backtrace code itself.  */
#define GUPCR_BT_SKIP_FRAME_CNT 3
/** Maximum number of stack frames to display.  */
#define GUPCR_BT_DEPTH_CNT 128

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif
/** Full path of the executable program.  */
static char __upc_abs_execname[PATH_MAX];

/** Backtrace on faults enabled flag.  */
static int bt_enabled = 0;

/** 
 * GLIBC backtrace.
 *
 * Show backtrace by using the GLIBC backtrace functionality.
 * Backtrace is improved with the source file/line numbers if
 * libbfd is available.
 *
 * By default backtrace lines are sent to the 'stderr' file
 * descriptor.  However, an environment variable
 * UPC_BACKTRACEFILE can be used to redirect the backtrace
 * to an actual file and it is used as a simple prefix for
 * the backtrace file. For example, if it is set to "/tmp/trace-upc",
 * the actual trace file is going to be "/tmp/trace-upc-PID.MYTHREAD".
 * If empty environment variable is provided, a simple "trace" prefix
 * is used.
 *
 */
void
__upc_backtrace (void)
{
  void *strace[GUPCR_BT_DEPTH_CNT];
  size_t size,i;
  char **strace_str;
  char *file_env;
  int under_upc_main = 1;
  FILE *traceout = stderr;
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");

  file_env = getenv (GUPCR_BACKTRACE_FILE_ENV);
  if (file_env)
    {
      int len = strlen (file_env); 
      char *tracefile = malloc (len + 128);
      if (!tracefile)
        __upc_fatal ("UPC backtrace cannot allocate file name memory");
      if (len)
        strcpy (tracefile, file_env);
      else
        {
	  strcpy (tracefile, "backtrace");
          len = 9;
	}
      len = snprintf (&tracefile[len], 128, ".%d", MYTHREAD);
      traceout = fopen (tracefile, "w");
      if (!traceout)
	__upc_fatal ("Cannot open file for backtrace log");
      free (tracefile);
    }
  else
    fprintf (traceout, "Thread %d backtrace:\n", MYTHREAD);

  /* Use "backtrace" functionality of glibc. */
  size = backtrace (strace, GUPCR_BT_DEPTH_CNT);
# if HAVE_LIBBFD
  strace_str = backtrace_src_symbols (strace, size, __upc_abs_execname);
# else
  strace_str = backtrace_symbols (strace, size);
# endif
  for (i = GUPCR_BT_SKIP_FRAME_CNT; i < size; i++)
    {
      if (under_upc_main)
        {
	  fprintf (traceout, "[%4d][%lld] %s\n", MYTHREAD, 
	      (long long int) (i - GUPCR_BT_SKIP_FRAME_CNT), strace_str[i]);
	  /* Extra info for the barrier. */
	  if ( strstr( strace_str[i], "__upc_wait"))
	    {
	      fprintf (traceout, "[%4d]       BARRIER ID: %d\n", MYTHREAD, 
		       __upc_barrier_id);
	    }
	}
      if (under_upc_main && strstr (strace_str[i], "upc_main"))
        under_upc_main = 0;
    }
  fflush (traceout);
  if (file_env)
    fclose (traceout);
}

#define GUPCR_BACKTRACE_PID_BUFLEN 16

/**
 * Backtrace on fatal errors.
 *
 * Print backtrace (stack frames) on fatal errors: run-time
 * fatal error or segementation fault. 
 *
 * Only print backtrace if environment variable UPC_BACKTRACE
 * is set to 1. The following order of backtrace capabilities
 * is searched and executed:
 *
 * (1) Use GDB for backtrace (if enabled)
 * (2) Use GLIBC backtrace with source file/line display (if
 *     libbfd is available)
 * (3) Use GLIBC backtrace with raw addresses (display is 
 *     improved if -rdynamic option is supported by the linker)
 *
 */
void
__upc_fatal_backtrace (void)
{
  if (bt_enabled)
    {
#ifdef HAVE_UPC_BACKTRACE_GDB
  	{
	  char *env;
	  const char *gdb;
          char pid_buf[GUPCR_BACKTRACE_PID_BUFLEN];
          int child_pid;
          /* Which gdb to use? */
          env = getenv (GUPCR_BACKTRACE_GDB_ENV);
          if (!env || (strlen (env) == 0))
              gdb = GUPCR_BACKTRACE_GDB;
	  else
              gdb = (const char *) env;
	  if (strcmp (gdb, "none"))
 	    {
	      const char *err_msg = 0;
	      char tmpf[PATH_MAX];
	      int fbt;
	      const char *btcmd = "backtrace 30\n";
              fprintf (stderr, "Thread %d GDB backtrace:\n", MYTHREAD);
	      /* Get pid and name of the running program. */
              sprintf(pid_buf, "%d", getpid());
	      /* Create temp file for GDB commands. */
	      if ((fbt = __upc_create_temp_file 
			 ("upc_bt_gdb.XXXXXX", tmpf, &err_msg)) == -1)
	   	{
		  fprintf (stderr, "cannot open gdb command - %s\n", err_msg);
		  return;
		}
	      if (write (fbt, btcmd, sizeof (btcmd)) == -1)
 		{
		  perror ("cannot write gdb command file for backtrace");
		  return;
		}
	      if (close (fbt))
 		{
		  perror ("cannot close gdb command file for backtrace");
		  return;
		}
              child_pid = fork();
              if (!child_pid)
		{
		  dup2(2,1);
		  execlp(gdb, gdb, "-nx", "-batch", "-x", tmpf, 
		         __upc_abs_execname, pid_buf, NULL);
		  fprintf (stderr, "cannot start GDB - %s\n", gdb);
		  abort(); /* If gdb failed to start */
		}
	      else
		waitpid(child_pid,NULL,0);
	      unlink (tmpf);
              return;
	    }
        }
#endif /* GUPCR_BACKTRACE_GDB */

       /* Simple backtrace only. */
       __upc_backtrace ();
    }
}

/**
 * Print thread/process mapping OR
 *   request a trace dump from UPC threads.
 */
static void
__upc_backtrace_monitor (void)
{
  int i;
  char *trace_file_name;
  trace_file_name = getenv (GUPCR_BACKTRACE_FILE_ENV);
  if (trace_file_name)
    {
      /* Dump backtraces into files.
         Send signal to all UPC threads.  */
      fprintf (stderr, "Thread monitor\n");
      fprintf (stderr, "Sending requests for trace dump\n");
      for (i = 0; i < THREADS; i++)
	{
	  kill (__upc_info->thread_info[i].pid, GUPCR_BACKTRACE_SIGNAL);
	}
    }
  else
    {
      fprintf (stderr, "Thread ID to PID mappings\n");
      fprintf (stderr, " Thread   PID\n");
      for (i = 0; i < THREADS; i++)
	{
	  fprintf (stderr, "   %4d   %d\n", i, __upc_info->thread_info[i].pid);
	}
    }
}

/**
 * Backtrace signal handler.
 *
 * Display stack frames on a request. In case of the
 * monitor thread only print the mappings between the 
 * UPC threads and processes.
 */
static void
__upc_backtrace_handler (int sig __attribute__ ((unused)),
			 siginfo_t *siginfo __attribute__ ((unused)),
			 void *context __attribute__ ((unused)))
{
  if (MYTHREAD == -1)
    __upc_backtrace_monitor ();
  else
    __upc_backtrace ();
}

/**
 * Backtrace fault handler.
 *
 * A fault happened and backtrace is enabled. Allow for only
 * one thread to print the backtrace. The restore signal
 * handlers to their default and return ensures that 
 * signal terminates the thread and allows for the monitor
 * thread to terminate all the other threads..
 */
static void
__upc_fault_handler (int sig __attribute__ ((unused)),
	  	     siginfo_t *siginfo __attribute__ ((unused)),
		     void *context __attribute__ ((unused)))
{
  upc_info_p u = __upc_info;
  if (u)
    __upc_acquire_lock (&u->lock);
  __upc_backtrace_restore_handlers ();
  __upc_fatal_backtrace ();
}

/**
 * Initialize UPC backtrace.
 */
void
__upc_backtrace_init (const char *execname)
{
  char *env;
  /* Find the full path for the executable. On linux systems we
     might be able to read "/proc/self/exe" to the get the full
     executable path. But, it is not portable. */
  int slen = sizeof (__upc_abs_execname) - strlen (execname) - 2;
  __upc_abs_execname[0] = 0;
  if (execname[0] != '/')
    {
      if (!getcwd (__upc_abs_execname, slen))
        strcpy (__upc_abs_execname, "/BT_CANNOT_CREATE_ABS_PATH");
      strcat (__upc_abs_execname, "/");
    }
  strcat (__upc_abs_execname, execname);

#ifdef HAVE_UPC_BACKTRACE_SIGNAL
  {
    /* Install backtrace signal handler (backtrace on request). */
    struct sigaction act;
    memset (&act, '\0', sizeof(act));
    act.sa_sigaction = &__upc_backtrace_handler;
    act.sa_flags = SA_SIGINFO;
    if (sigaction(GUPCR_BACKTRACE_SIGNAL, &act, NULL) < 0) {
      perror ("was not able to install backtrace handler");
    }
  }
#endif

  /* Install signal handlers only if backtrace is enabled.  */
  env = getenv (GUPCR_BACKTRACE_ENV);
  if (env)
    bt_enabled = atoi (env);
  
  if (bt_enabled)
    {
      struct sigaction act;
      memset (&act, '\0', sizeof(act));
      act.sa_sigaction = &__upc_fault_handler;
      act.sa_flags = SA_SIGINFO;
      if (sigaction(SIGABRT, &act, NULL) < 0)
        perror ("unable to install SIGABRT handler");
      if (sigaction(SIGILL, &act, NULL) < 0)
        perror ("unable to install SIGILL handler");
      if (sigaction(SIGSEGV, &act, NULL) < 0)
        perror ("unable to install SIGSEGV handler");
      if (sigaction(SIGBUS, &act, NULL) < 0)
        perror ("unable to install SIGBUS handler");
      if (sigaction(SIGFPE, &act, NULL) < 0)
        perror ("unable to install SIGFPE handler");
    }
}

/**
 * Restore default handlers.
 *
 * Has to be called once the run-time discovered
 * a fatal error.
 */ 
void
__upc_backtrace_restore_handlers (void)
{
  /* Don't handle any signals with backtrace code. Install
     default handlers.  */
  signal (SIGABRT, SIG_DFL);
  signal (SIGILL, SIG_DFL);
  signal (SIGSEGV, SIG_DFL);
  signal (SIGBUS, SIG_DFL);
  signal (SIGFPE, SIG_DFL);
}
