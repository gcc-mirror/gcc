/* Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

/** Default backtrace file name prefix.  */
#define UPC_BACKTRACE_PREFIX "backtrace"

/** Full path of the executable program.  */
static char *__upc_abs_execname;

/** Backtrace on faults enabled flag.  */
static int bt_enabled = 0;

/** 
 * GLIBC backtrace.
 *
 * Show backtrace by using the GLIBC backtrace functionality.
 * Backtrace is improved with the source file/line numbers if
 * addr2line is available.
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
      #define MAX_INT_STRING ".2147483647"
      char *tracefile;
      int len, lenw;
      /* Use default trace file name if one not specified by the user.  */
      if (!strlen (file_env))
	file_env = (char *) UPC_BACKTRACE_PREFIX;
      len = strlen (file_env) + strlen (MAX_INT_STRING) + 1;
      tracefile = malloc (len);
      if (!tracefile)
        __upc_fatal ("cannot allocate (%d) memory for backtrace file %s",
		     len, file_env);
      lenw = snprintf (tracefile, len, "%s.%d", file_env, MYTHREAD);
      if ((lenw >= len) || (lenw < 0))
	__upc_fatal ("cannot create backtrace file name: %s", file_env);
      traceout = fopen (tracefile, "w");
      if (!traceout)
	__upc_fatal ("cannot open backtrace file: %s", tracefile);
      free (tracefile);
    }
  else
    fprintf (traceout, "Thread %d backtrace:\n", MYTHREAD);

  /* Use "backtrace" functionality of glibc to receive
     backtrace addresses.  */
  size = backtrace (strace, GUPCR_BT_DEPTH_CNT);
  /* Add symbolic information to each address
     and print the stack trace.  */
  for (i = GUPCR_BT_SKIP_FRAME_CNT; i < size; i++)
    {
      if (under_upc_main)
        {
# if HAVE_UPC_BACKTRACE_ADDR2LINE
	  /* Call addr2line to generate source files, line numbers,
	     and functions.  In case of any error (malloc, snprintf)
	     do not abort the program.  */
	  FILE *a2l;
	  #define CMD_TMPL "%s -f -e %s %p"
	  /* Allow space for addr2line, filename, command line options,
	     and address argument for addr2line.  */
	  int cmd_size = strlen (GUPCR_BACKTRACE_ADDR2LINE) +
			 strlen (__upc_abs_execname) +
			 strlen (CMD_TMPL) +
			 strlen ("0x1234567812345678");
	  int sz;
	  char *cmd = malloc (cmd_size);
	  /* Create an actual addr2line command.  */
	  sz = snprintf (cmd, cmd_size, CMD_TMPL, GUPCR_BACKTRACE_ADDR2LINE,
			 __upc_abs_execname, strace[i]);
	  if ((sz >= cmd_size) || (sz < 0))
	    {
	      fprintf (traceout, "unable to create addr2line "
				 "command line\n");
	      return;
	    }
	  /* Execute addr2line.  */
	  a2l = popen (cmd, "r");
	  free (cmd);
	  if (a2l)
	    {
	      /* addr2line responds with two lines: procedure name and
		 the file name with line number.  */
	      int max_rep = 2 * FILENAME_MAX;
	      /* Build a data structure that is identical to the
		 structure returned by the glibc backtrace_symbol().  */
	      struct back_trace {
		char *addr;
	        char data[1];
	      };
	      struct back_trace *rep = malloc (max_rep);
	      int index = 0;
	      if (!rep)
		{
		  fprintf (traceout, "unable to acquire memory "
				     "for backtracing\n");
		  return;
		}
	      rep->data[0] = '\0';
	      /* Read addr2line response.  */
	      while (fgets(&rep->data[index], max_rep-index, a2l))
		{
		  /* Remove all the new lines, as addr2line returns
		     info in multiple lines.  */
		  index = strlen (&rep->data[0]);
		  if (rep->data[index - 1] == '\n')
		    rep->data[index - 1] = ' ';
		}
	      pclose (a2l);
	      rep->addr = &rep->data[0];
	      strace_str = &rep->addr;
	    }
	  else
	    {
	      /* Somehow we failed to invoke addr2line, fall back
	         to glibc.  */
	      strace_str = backtrace_symbols (&strace[i], 1);
	    }
# else
	  strace_str = backtrace_symbols (&strace[i], 1);
# endif
	  fprintf (traceout, "[%4d][%lld] %s\n", MYTHREAD, 
	      (long long int) (i - GUPCR_BT_SKIP_FRAME_CNT), *strace_str);
	  /* Extra info for the barrier. */
	  if (strstr( *strace_str, "__upc_wait"))
	    {
	      fprintf (traceout, "[%4d]       BARRIER ID: %d\n", MYTHREAD, 
		       __upc_barrier_id);
	    }
          if (strstr (*strace_str, "upc_main"))
	    under_upc_main = 0;
	  /* Symbol trace buffer must be released.  */
	  free (strace_str);
	}
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
 * fatal error or segmentation fault. 
 *
 * Only print backtrace if environment variable UPC_BACKTRACE
 * is set to 1. The following order of backtrace capabilities
 * is searched and executed:
 *
 * (1) Use GDB for backtrace (if enabled)
 * (2) Use GLIBC backtrace with source file/line display (if
 *     addr2line is available)
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
              sprintf(pid_buf, "%ld", (long) getpid());
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
	  fprintf (stderr,
	           "   %4d   %ld\n", i, (long) __upc_info->thread_info[i].pid);
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
  __upc_abs_execname = malloc (PATH_MAX + 1);
  if (!__upc_abs_execname)
    __upc_fatal ("cannot allocate space for executable file name");
  *__upc_abs_execname = '\0';
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
