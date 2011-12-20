/* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011
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

#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_sup.h"
#include "upc_affinity.h"
#include "upc_numa.h"
#include "upc_debug.h"
#include "gasp_upc.h"
#include "upc_pupc.h"

/* user's main program */
extern int GUPCR_MAIN (int argc, char *argv[]);

/* The number of THREADS, as specified on the command line */
int THREADS = -1;

#ifdef GUPCR_USE_PTHREADS
/* The number of pthreads to run per process, as specified on the
   command line. Required to be equal to the number of UPC threads
   in the current implementation. */
int UPC_PTHREADS = -1;
#endif /* GUPCR_USE_PTHREADS */

/* The current thread number (range: 0..THREADS-1) */
GUPCR_THREAD_LOCAL int MYTHREAD;

/* Depth count used to implement the semantics of
   nested upc_forall statements.  */
GUPCR_THREAD_LOCAL int __upc_forall_depth;

/* The UPC page size, in bits.  Don't use 'const' here
   because we want this to end up the loaded data where
   the debug assistant can read it.  Eventually move this
   to the upc_info structure.  */
int __upc_page_shift = GUPCR_VM_OFFSET_BITS;

/* Executable program's name */
static char *__upc_pgm_name;

/* Runtime state information */
upc_info_p __upc_info;

/* The filename of the location where a runtime
   error was detected.  This is set by the various
   debug-enabled ('g') UPC runtime library routines.  */
GUPCR_THREAD_LOCAL const char *__upc_err_filename;

/* The line number of the location where a runtime
   error was detected.  This is set by the various
   debug-enabled ('g') UPC runtime library routines.  */
GUPCR_THREAD_LOCAL unsigned int __upc_err_linenum;

/* Interface to the debugger */
MPIR_PROCDESC *MPIR_proctable = 0;
int MPIR_proctable_size = 0;
const char *MPIR_debug_abort_string = 0;

volatile int MPIR_debug_state;
volatile int MPIR_debug_gate;
int MPIR_being_debugged;	/* Set by the debugger */

/* per-thread initial heap size */
static size_t __upc_init_heap_size = GUPCR_DEFAULT_PER_THREAD_HEAP_SIZE;

/* CPU scheduling policy */
static upc_sched_policy_t __upc_sched_policy = GUPCR_SCHED_POLICY_DEFAULT;

/* CPU memory affinity policy */
static upc_mem_policy_t __upc_mem_policy = GUPCR_MEM_POLICY_DEFAULT;

/* list of CPU's that must be avoided */
static upc_cpu_avoid_p __upc_cpu_avoid_set;

/* UPC debug on/off */
static int __upc_gum_debug = 0;

/* UPC debug - UDA server shared pointer.
       Shared pointer type definition must be used, otherwise compiler
       removes type definition. */
static upc_dbg_shared_ptr_t dbg_shared_ptr __attribute__ ((__unused__));

#define IS_MULT_CHAR(c) ((c) == 'k' || (c) == 'K' \
			 || (c) == 'm' || (c) == 'M' \
			 || (c) == 'g' || (c) == 'G')
#define MULT_FACTOR(c) (((c) == 'k' || (c) == 'K') ? 1024 \
                        : ((c) == 'm' || (c) == 'M') ? 1024*1024 \
		        : ((c) == 'g' || (c) == 'G') ? 1024*1024*1024 : 1)

/* switches that require extra argument (values) */
#define SWITCH_TAKES_ARG(STR) \
  (!strcmp (STR, "-n") || !strcmp(STR, "-heap") \
  || !strcmp (STR, "-sched-policy") \
  || !strcmp (STR, "-mem-policy") \
  || !strcmp (STR, "-sched-cpu-avoid"))

static int
__upc_get_int_value (const char *str, long int *val,
		     int accept_multiplier, long int low, long int high)
{
  const char *s = str;
  int factor = 1;
  long int v;
  *val = 0;
  while (*s && isdigit ((int) *s))
    ++s;
  if (*s)
    {
      if (accept_multiplier && IS_MULT_CHAR (s[0])
	  && (!s[1] || (s[1] == 'b' || s[1] == 'B')))
	factor = MULT_FACTOR (s[0]);
      else
	return 0;
    }
  v = atol (str) * factor;
  if (v < low || v > high)
    return 0;
  *val = v;
  return 1;
}

/* Get list of CPUs to exclude from scheduling on (n1,n2,...) */
static int
__upc_get_cpu_avoid_values (const char *str, const upc_cpu_avoid_p avoid)
{
  const char *s = str;
  char digits[10];
  char *const last_digit = &digits[sizeof (digits) - 2];
  const long int max_cpus = (__upc_num_cpus - 1);
  while (*s)
    {
      char *d = digits;
      long int v;
      int status;
      /* A number has to begin with a valid digit.  */
      if (!isdigit ((int) *s))
	return 0;
      while (*s && isdigit ((int) *s) && d <= last_digit)
	*d++ = *s++;
      /* Too many digits.  */
      if (*s && isdigit ((int) *s))
	return 0;
      *d = '\0';
      status = __upc_get_int_value (digits, &v, 0, 0, max_cpus);
      if (!status)
	return 0;
      __upc_affinity_cpu_avoid_set (v, avoid);
      if (*s == ',')
	{
	  ++s;
	  /* Something has to follow the comma.  */
	  if (!*s)
	    return 0;
	}
    }
  return 1;
}

static void
__upc_print_help_and_exit (char *pgm)
{
  fprintf (stderr, "usage: %s [UPC switches] ...\n", pgm);
  fprintf (stderr, "where the possible UPC switches are:\n");
  fprintf (stderr,
	   "	-fupc-threads-N or -n N			N is number of threads to run\n");
  fprintf (stderr,
	   "						(N must be in the range 1..%d)\n",
	   GUPCR_THREADS_MAX);
#ifdef GUPCR_USE_PTHREADS
  fprintf (stderr,
	   "	-fupc-pthreads-per-process-N		map UPC threads to POSIX threads using TLS capability\n");
#endif /* GUPCR_USE_PTHREADS */
  fprintf (stderr,
	   "	-fupc-heap-N or -heap N			N is the maximum per-thread memory\n");
  fprintf (stderr,
	   "						allocation heap size\n");
  fprintf (stderr,
	   "						The value of N may be followed\n");
  fprintf (stderr,
	   "						by a scale factor of K, M, or G\n");
  fprintf (stderr,
	   "						(N must be in the range 1..%ld)\n",
	   GUPCR_MAX_HEAP_SIZE);
  fprintf (stderr,
	   "	-sched-policy [cpu,strict,node,auto] 	UPC scheduling policy\n");
  fprintf (stderr,
	   "	 					  cpu - bind to CPU\n");
  fprintf (stderr,
	   "	 					  strict - bind to CPU (one thread per CPU)\n");
  fprintf (stderr,
	   "	                                      	  node - bind to node (if NUMA available)\n");
  fprintf (stderr,
	   "	                                      	  auto - let kernel schedule\n");
  fprintf (stderr,
	   "	-sched-cpu-avoid n1,n2,.. 		List of CPUs to avoid schedulig on\n");
  fprintf (stderr,
	   "	  					  0 to max CPUs\n");
  fprintf (stderr,
	   "	-mem-policy [node,strict,auto]		UPC memory allocation policy\n");
  fprintf (stderr,
	   "	 				 	  node - allocate on local node first\n");
  fprintf (stderr,
	   "	                                  	  strict - only allocate on local node\n");
  fprintf (stderr,
	   "	-g                                  	Turn on UPC source code debugging\n");

  exit (2);
}

static void
__upc_shift_args (int *argc, char *argv[])
{
  if (*argc > 1)
    {
      int i;
      --*argc;
      for (i = 1; i < *argc; ++i)
	argv[i] = argv[i + 1];
    }
}

static void
__upc_process_switches (char *pgm, int *argc, char *argv[])
{
  long int threads_switch_value = 0;
#ifdef GUPCR_USE_PTHREADS
  long int pthreads_switch_value = 0;
#endif /* GUPCR_USE_PTHREADS */
  long int heap_switch_value = 0;
  const char *gum_debug_env = getenv (GUM_DEBUG_ENV);
  /* Check if GUM debugging is enabled by environment variable.  */
  if (gum_debug_env && atoi(gum_debug_env))
    {
      __upc_gum_debug = 1;
    }
  while (*argc >= 2)
    {
      const char *arg = argv[1];
      const char *tval;
      if (!strcmp (arg, "--"))
	{
	  /* -- terminates any upc switches */
	  __upc_shift_args (argc, argv);
	  break;		/* exit loop */
	}
      if (SWITCH_TAKES_ARG (arg) && (*argc < 3))
	{
	  fprintf (stderr, "%s argument requires a value\n", arg);
	  __upc_print_help_and_exit (pgm);
	}
      if (!strncmp (arg, "-fupc-threads-", 14))
	{
	  tval = arg + 14;
	  if (!__upc_get_int_value (tval, &threads_switch_value, 0,
				    1L, (long int) GUPCR_THREADS_MAX))
	    {
	      fprintf (stderr, "Invalid THREADS value\n");
	      __upc_print_help_and_exit (pgm);
	    }
	}
      else if (!strcmp (arg, "-n"))
	{
	  tval = argv[2];
	  if (!__upc_get_int_value (tval, &threads_switch_value, 0,
				    1L, (long int) GUPCR_THREADS_MAX))
	    {
	      fprintf (stderr, "Invalid THREADS value\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  __upc_shift_args (argc, argv);
	}
      else if (!strncmp (arg, "-n", 2))
	{
	  tval = arg + 2;
	  if (!__upc_get_int_value (tval, &threads_switch_value, 0,
				    1L, (long int) GUPCR_THREADS_MAX))
	    {
	      fprintf (stderr, "Invalid THREADS value\n");
	      __upc_print_help_and_exit (pgm);
	    }
	}
#ifdef GUPCR_USE_PTHREADS
      else if (!strncmp (arg, "-fupc-pthreads-", 15))
	{
	  tval = arg + 15;
	  if (!__upc_get_int_value (tval, &pthreads_switch_value, 0,
				    1L, (long int) GUPCR_THREADS_MAX))
	    {
	      fprintf (stderr, "Invalid UPC pthreads value\n");
	      __upc_print_help_and_exit (pgm);
	    }
	}
#endif /* GUPCR_USE_PTHREADS */
      else if (!strncmp (arg, "-fupc-heap-", 11))
	{
	  tval = arg + 11;
	  if (!__upc_get_int_value (tval, &heap_switch_value, 1,
				    1L, GUPCR_MAX_HEAP_SIZE))
	    {
	      fprintf (stderr, "Invalid heap size value\n");
	      __upc_print_help_and_exit (pgm);
	    }
	}
      else if (!strcmp (arg, "-heap"))
	{
	  tval = argv[2];
	  if (!__upc_get_int_value (tval, &heap_switch_value, 1,
				    1L, GUPCR_MAX_HEAP_SIZE))
	    {
	      fprintf (stderr, "Invalid heap size value\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  __upc_shift_args (argc, argv);
	}
      else if (!strncmp (arg, "-heap", 5))
	{
	  tval = arg + 5;
	  if (!__upc_get_int_value (tval, &heap_switch_value, 1,
				    1L, GUPCR_MAX_HEAP_SIZE))
	    {
	      fprintf (stderr, "Invalid heap size value\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  __upc_shift_args (argc, argv);
	}
      else if (!strcmp (arg, "-sched-policy"))
	{
	  if (!__upc_affinity_supported ())
	    {
	      fprintf (stderr,
		       "Scheduling affinity not supported or configured\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  tval = argv[2];
	  if (!strcmp (tval, "node"))
	    {
	      if (!__upc_numa_supported ())
		{
		  fprintf (stderr,
			   "NUMA node affinity not supported or configured\n");
		  __upc_print_help_and_exit (pgm);
		}
	      __upc_sched_policy = GUPCR_SCHED_POLICY_NODE;
	    }
	  else if (!strcmp (tval, "strict"))
	    {
	      __upc_sched_policy = GUPCR_SCHED_POLICY_CPU_STRICT;
	    }
	  else if (!strcmp (tval, "cpu"))
	    {
	      __upc_sched_policy = GUPCR_SCHED_POLICY_CPU;
	    }
	  else if (!strcmp (tval, "auto"))
	    {
	      __upc_sched_policy = GUPCR_SCHED_POLICY_AUTO;
	    }
	  else
	    {
	      fprintf (stderr, "Invalid scheduling policy specified\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  __upc_shift_args (argc, argv);
	}
      else if (!strcmp (arg, "-mem-policy"))
	{
	  if (!__upc_numa_supported ())
	    {
	      fprintf (stderr,
		       "NUMA node affinity not supported or configured\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  tval = argv[2];
	  if (!strcmp (tval, "node"))
	    {
	      __upc_mem_policy = GUPCR_MEM_POLICY_NODE;
	    }
	  else if (!strcmp (tval, "strict"))
	    {
	      __upc_mem_policy = GUPCR_MEM_POLICY_STRICT;
	    }
	  else if (!strcmp (tval, "auto"))
	    {
	      __upc_mem_policy = GUPCR_MEM_POLICY_AUTO;
	    }
	  else
	    {
	      fprintf (stderr,
		       "Invalid memory allocation policy specified\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  __upc_shift_args (argc, argv);
	}
      else if (!strcmp (arg, "-sched-cpu-avoid"))
	{
	  if (!__upc_affinity_supported ())
	    {
	      fprintf (stderr,
		       "Scheduling affinity not supported or configured\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  tval = argv[2];
	  if (!__upc_get_cpu_avoid_values (tval, __upc_cpu_avoid_set))
	    {
	      fprintf (stderr, "Invalid CPU to avoid string\n");
	      __upc_print_help_and_exit (pgm);
	    }
	  __upc_shift_args (argc, argv);
	}
      else if (!strcmp (arg, "-g"))
	{
	  __upc_gum_debug = 1;
	}
      else
	/* exit loop at first unrecognized switch.  */
	break;
      __upc_shift_args (argc, argv);
    }
  if (heap_switch_value)
    {
      __upc_init_heap_size = heap_switch_value;
    }
  if (threads_switch_value)
    {
      if (THREADS > 0)
	{
	  if (threads_switch_value != THREADS)
	    {
	      fprintf (stderr, "%s: UPC error: The value of the"
		       " -fupc-threads-N switch: %ld,"
		       " does not agree with the value given at"
		       " compile-time: %d\n",
		       pgm, threads_switch_value, THREADS);
	      exit (2);
	    }
	}
      else
	{
	  THREADS = threads_switch_value;
#ifdef GUPCR_USE_PTHREADS
	  if (UPC_PTHREADS == -1)
	    {
	      /* UPC threads per process specified as 'dynamic'.
	         Set it GUPCR_THREADS to specified value of THREADS. */
	      UPC_PTHREADS = THREADS;
	    }
#endif /* GUPCR_USE_PTHREADS */
	}
    }
  if (THREADS <= 0)
    {
      fprintf (stderr, "%s: UPC error: No value given for THREADS\n", pgm);
      __upc_print_help_and_exit (pgm);
    }
#ifdef GUPCR_USE_PTHREADS
  if (pthreads_switch_value)
    {
      if (pthreads_switch_value != THREADS)
	{
	  fprintf (stderr,
		   "%s: UPC error: pthreads value must equal the number of UPC threads\n",
		   pgm);
	  __upc_print_help_and_exit (pgm);
	}
      else
	{
	  UPC_PTHREADS = pthreads_switch_value;
	}
    }
#endif /* GUPCR_USE_PTHREADS */
#if !GUPCR_HAVE_GUM_DEBUG
  if (__upc_gum_debug)
    {
      fprintf (stderr, "UPC debugging not supported or configured\n");
      __upc_print_help_and_exit (pgm);
    }
#endif
}

static upc_info_p
__upc_init (char *pgm, const char **err_msg)
{
  upc_info_p u;
  os_heap_p runtime_heap;
  size_t alloc_data_size, local_size, max_init_alloc, heap_size;
  size_t mmap_fn_len;
  char mmap_file_name[2046];
  upc_page_num_t init_page_alloc;
  const size_t gpt_size = (GUPCR_VM_MAX_PAGES_PER_THREAD * THREADS)
    * sizeof (upc_pte_t);

  /* On SGI/Irix, create the shared arena, used for inter-process
     synchronization, otherwise probably a no-op.  */
  max_init_alloc =
    GUPCR_ROUND (sizeof (upc_info_t) + gpt_size + sizeof (mmap_file_name),
		 0x4000);
  runtime_heap = __upc_create_runtime_heap (max_init_alloc, err_msg);
  if (!runtime_heap)
    return 0;

  /* allocate the UPC info structure */
  u = __upc_runtime_alloc (sizeof (upc_info_t), &runtime_heap, err_msg);
  if (!u)
    return 0;
  memset (u, '\0', sizeof (upc_info_t));

  u->runtime_heap = runtime_heap;
  u->program_name = pgm;
  u->monitor_pid = getpid ();
  u->num_cpus = __upc_num_cpus;
  /* Defaults to 1, will be overidden if NUMA supported.  */
  u->num_nodes = 1;
  u->sched_policy = __upc_sched_policy;
  u->mem_policy = __upc_mem_policy;

  /* Calculate per-thread contribution to global shared memory region. */
  alloc_data_size = GUPCR_SHARED_SECTION_END - GUPCR_SHARED_SECTION_START;
  alloc_data_size = GUPCR_ROUND (alloc_data_size, C64K);
  heap_size = GUPCR_ROUND (__upc_init_heap_size, C64K);
  local_size = alloc_data_size + heap_size;
  /* Round up to a page boundary */
  local_size = GUPCR_ROUND (local_size, GUPCR_VM_PAGE_SIZE);
  init_page_alloc = local_size / GUPCR_VM_PAGE_SIZE;
  /* Everything that isn't initially allocated to data will
     be used for the heap.  */
  heap_size = local_size - alloc_data_size;
  u->init_page_alloc = init_page_alloc;
  u->init_heap_size = heap_size;
  GUPCR_PTS_SET_NULL_SHARED (u->init_heap_base);
  GUPCR_PTS_SET_VADDR (u->init_heap_base, alloc_data_size);
  u->smem_fd = __upc_create_global_mem_file (mmap_file_name, err_msg);
  if (u->smem_fd < 0)
    return 0;
  mmap_fn_len = strlen (mmap_file_name);
  u->mmap_file_name = (char *) __upc_runtime_alloc (mmap_fn_len + 1,
						    &runtime_heap, err_msg);
  if (!u->mmap_file_name)
    return 0;
  strcpy (u->mmap_file_name, mmap_file_name);
  /* Allocate the GPT.  Avoid initializing it, because it may
     be a rather large data structure of which only a few initial
     locations are used.  The VM routines that manipulate the
     GPT will initialize all needed entries as they are used. */
  u->gpt = (upc_pte_p) __upc_runtime_alloc (gpt_size, &runtime_heap, err_msg);
  if (!u->gpt)
    return 0;
  return u;
}

/* Per thread initialization.  The VM system has to be initialized
   in each thread, because it maintains a record of locally
   mapped memory regions.  Further, for thread 0, the initial
   data values need to be copied over, and the heap manager
   must be initialized.  The barrier that is executed subsequnt
   to calling this per thread initialization procedure and prior to
   calling the main program ensures that the initialization
   completes before the main program runs.  */

static void
__upc_per_thread_init (upc_info_p u)
{
  typedef void (*func_ptr_t) (void);
  extern func_ptr_t GUPCR_INIT_ARRAY_START[1];
  extern func_ptr_t GUPCR_INIT_ARRAY_END[1];
  const int n_init = (int)(GUPCR_INIT_ARRAY_END - GUPCR_INIT_ARRAY_START);
  int i;
  __upc_vm_init_per_thread ();
  __upc_heap_init (u->init_heap_base, u->init_heap_size);
  for (i = 0; i < n_init; ++i)
    {
      func_ptr_t init_func = GUPCR_INIT_ARRAY_START[i];
      /* Skip zero words introduced by section marker, or by the linker.  */
      if (init_func)
	(*init_func) ();
    }
}

#ifndef GUPCR_USE_PTHREADS

static void
__upc_run_this_thread (upc_info_p u, int argc, char *argv[],
		       unsigned int thread_id)
{
  int status;
  MYTHREAD = thread_id;
  /* Perform per thread initialization.  */
  __upc_per_thread_init (u);
  if (THREADS == 1)
    {
      /* A single thread is handled as a special case.
         No child process is created to run the thread. */
      MPIR_being_debugged = 0;
      /* Give the debugger a chance to pick up runtime info.  */
      MPIR_Breakpoint ();
      /* It is safe to unlink the temporary file, after the breakpoint
         is hit.  This gives the debugger a chance to open the mmap
         global memory file so that it can access UPC shared memory.  */
      if (unlink (u->mmap_file_name) < 0)
	{
	  perror ("cannot unlink global shared memory file");
	  abort ();
	}
    }
  else if (MPIR_being_debugged)
    {
      /* Wait for the debugger to acquire us */
      while (!MPIR_debug_gate)
	__upc_yield_cpu ();
    }
#if GUPCR_HAVE_GUM_DEBUG
  if (__upc_gum_debug)
    {
      __upc_gum_init (THREADS, thread_id);
    }
#endif
  __upc_barrier (GUPCR_RUNTIME_BARRIER_ID);
  __upc_pupc_init (&argc, &argv);
  status = GUPCR_MAIN (argc, argv);
  p_startx (GASP_UPC_COLLECTIVE_EXIT, status);
  p_endx (GASP_UPC_COLLECTIVE_EXIT, status);
  __upc_exit (status);
}

/* Implement UPC threads as processes. */
static void
__upc_run_threads (upc_info_p u, int argc, char *argv[])
{
  int thread_id;
  if (THREADS == 1)
    {
      __upc_affinity_set (u, 0);
      __upc_run_this_thread (u, argc, argv, 0);
      /* Shouldn't get here.  */
      abort ();
    }

  /* In case a debugger is using the value;
     we don't want it to see two thread zeros */
  MYTHREAD = -1;
  /* Allocate space to tell the debugger about
     the process we're creating */
  MPIR_proctable = malloc (THREADS * sizeof (*MPIR_proctable));
  /* Tell the debugger this process is a starter process.  */
  MPIR_i_am_starter ();
  for (thread_id = 0; thread_id < THREADS; ++thread_id)
    {
      pid_t pid = fork ();
      if (pid == 0)
	{
	  /* child */
	  __upc_affinity_set (u, thread_id);
	  __upc_run_this_thread (u, argc, argv, thread_id);
	}
      else if (pid > 0)
	{
	  /* parent */
	  u->thread_info[thread_id].pid = pid;
	  if (MPIR_being_debugged)
	    {
	      MPIR_proctable[thread_id].host_name = 0;
	      MPIR_proctable[thread_id].executable_name = 0;
	      MPIR_proctable[thread_id].pid = pid;
	    }
	}
      else
	{
	  /* error */
	  perror ("fork");
	  exit (2);
	}
    }
  /* We're the main process, there are child processes and they're all started.
   * Let the debugger know about that.
   */
  if (MPIR_being_debugged)
    {
      MPIR_proctable_size = THREADS;
      MPIR_debug_state = MPIR_DEBUG_SPAWNED;
      /* The debugger will have set a breakpoint there... */
      MPIR_Breakpoint ();
    }
  if (unlink (u->mmap_file_name) < 0)
    {
      perror ("cannot unlink global shared memory file");
      abort ();
    }
}

static int
__upc_get_thread_id (pid_t pid)
{
  upc_info_p u = __upc_info;
  int thread_id;
  for (thread_id = THREADS - 1;
       thread_id >= 0 && u->thread_info[thread_id].pid != pid;
       --thread_id) /* loop */ ;
  return thread_id;
}

static int
__upc_monitor_threads (void)
{
  upc_info_p u = __upc_info;
  pid_t pid;
  int wait_status;
  int exit_status;
  int thread_id;
  int global_exit_invoked;
  exit_status = -1;
  global_exit_invoked = 0;
  while ((pid = wait (&wait_status)) > 0)
    {
      thread_id = __upc_get_thread_id (pid);
      if (!global_exit_invoked && WIFEXITED (wait_status))
	{
	  int child_exit = WEXITSTATUS (wait_status);
	  if (child_exit & 0x80)
	    {
	      /* By convention, the result of a call to upc_global_exit
	         has the high bit in the byte set.
	         Terminate all the other threads in the program. */
	      int t;
	      for (t = 0; t < THREADS; ++t)
		{
		  int pid = u->thread_info[t].pid;
		  if (pid <= 0)
		    abort ();
		  if (t != thread_id)
		    (void) kill (pid, SIGKILL);
		}
	      child_exit &= 0x7f;
	      global_exit_invoked = 1;
	    }
	  else if ((exit_status != -1) && exit_status != child_exit)
	    {
	      fprintf (stderr, "conflicting exit status (%d) for"
		       " thread %d\n", child_exit, thread_id);
	    }
	  exit_status = child_exit;
	}
      else if (WIFSIGNALED (wait_status))
	{
	  int child_sig = WTERMSIG (wait_status);
	  /* Ignore SIGKILL signals.
	     We use them to implement upc_global_exit(). */
	  if (child_sig == SIGKILL)
	    continue;
	  fprintf (stderr, "thread %d terminated with signal: '%s'\n",
		   thread_id, __upc_strsignal (child_sig));
          /*  GASP note: We can't record a noncollective GASP
              exit event here, because the process has already died.  */
	  /* We'll all go away now. */
	  if (killpg (getpid (), SIGTERM) == -1)
	    {
	      perror ("killpg");
	      exit (-1);
	    }
	}
    }
  return exit_status;
}

/* Calls to exit() are rewritten into calls to __upc_exit()
   by #define in <gcc-upc.h>. Simply perform a upc_barrier and
   then exit the process. Monitor_threads() will pick up
   the returned status code.  */
void
__upc_exit (int status)
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  __upc_acquire_lock (&u->lock);
  fflush (0);
  fsync (1);
  fsync (2);
  __upc_release_lock (&u->lock);
  __upc_barrier (GUPCR_RUNTIME_BARRIER_ID);
  exit (status);
}

/* upc_global_exit - exit program with given status, terminate
   all other threads.
 
   The implementation imposes a restriction on exit return codes.
   If the return code has bit 7 (0x80) set, then the exit code will
   be interpreted as the code passed to upc_global_exit() and the
   monitor program will cancel all other executing threads.  */
void
upc_global_exit (int status)
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  exit ((THREADS > 1) ? ((status & 0x7f) | 0x80) : status);
}

#else /* !USE_UPC_PTHREADS */

/* Implement UPC threads as POSIX threads. */

/* UPC rand() pthreads implementation uses per thread seed */

static GUPCR_THREAD_LOCAL unsigned int __upc_rand_seed;

int
__upc_rand (void)
{
  return rand_r (&__upc_rand_seed);
}

void
__upc_srand (unsigned int _seed)
{
  __upc_rand_seed = _seed;
}

typedef struct upc_startup_args_struct
{
  int thread_id;
  int argc;
  char **argv;
} upc_startup_args_t;
typedef upc_startup_args_t *upc_startup_args_p;

static void *
__upc_start_pthread (void *arg)
{
  upc_startup_args_p startup_args = arg;
  int thread_id = startup_args->thread_id;
  upc_info_p u = __upc_info;
  int *status_ptr;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  /* MYTHREAD is located in thread local storage */
  MYTHREAD = thread_id;
  __upc_affinity_set (u, thread_id);
  /* Perform per thread initialization.  */
  __upc_per_thread_init (u);
  /* Initialize random number generator seed.
     Note: C99 requires an initial seed value of 1, per 7.20.2.2. */
  __upc_srand (1);
  status_ptr = &u->thread_info[thread_id].exit_status;
  __upc_barrier (GUPCR_RUNTIME_BARRIER_ID);
  __upc_pupc_init (&startup_args->argc, &startup_args->argv);
  *status_ptr = GUPCR_MAIN (startup_args->argc, startup_args->argv);
  p_startx (GASP_UPC_COLLECTIVE_EXIT, *status_ptr);
  p_endx (GASP_UPC_COLLECTIVE_EXIT, *status_ptr);
  return status_ptr;
}

static void
__upc_run_threads (upc_info_p u, int argc, char *argv[])
{
  int thread_id;
  pthread_attr_t thread_attr;
  size_t stack_size;

  if (THREADS != UPC_PTHREADS)
    {
      fprintf (stderr,
	       "GUPC pthreads implementation requires that PTHREADS be to THREADS.\n");
      abort ();
    }

  if (pthread_attr_init(&thread_attr))
    {
      perror ("pthread_attr_init");
      abort ();
    }
  if (pthread_attr_getstacksize(&thread_attr, &stack_size))
    {
      perror ("pthread_attr_getstacksize");
      abort ();
    }
  /* Add the GUPC's default per-thread stack size to the
     operating system default.  The OS default will often
     include enough space to account for TLS variables declared
     using the __thread qualifier.  */
  stack_size += GUPCR_DEFAULT_PER_THREAD_STACK_SIZE;
  if (pthread_attr_setstacksize(&thread_attr, stack_size))
    {
      perror ("pthread_attr_setstacksize");
      abort ();
    }

  /* technically, we should probably make a thread-local
     copy of the arg vector. For now, just pass the address. */

  for (thread_id = 0; thread_id < THREADS; ++thread_id)
    {
      upc_startup_args_p startup_args;
      int status;
      pthread_t pthread_id;
      startup_args =
	(upc_startup_args_p) malloc (sizeof (upc_startup_args_t));
      if (!startup_args)
	{
	  perror ("malloc");
	  abort ();
	}
      startup_args->argc = argc;
      startup_args->argv = argv;
      startup_args->thread_id = thread_id;
      status = pthread_create (&pthread_id, &thread_attr,
			       __upc_start_pthread, startup_args);
      if (status)
	{
	  perror ("pthread_create");
	  abort ();
	}
      u->thread_info[thread_id].os_thread = pthread_id;
    }
  if (unlink (u->mmap_file_name) < 0)
    {
      perror ("cannot unlink global shared memory file");
      abort ();
    }
}

/* Wait for all pthreads to exit. This implementation requires
   that there is one pthread per UPC thread. */
static int
__upc_monitor_threads (void)
{
  int exit_status = -1;
  upc_info_p u = __upc_info;
  int t;
  for (t = 0; t < THREADS; ++t)
    {
      pthread_t os_thread = u->thread_info[t].os_thread;
      void *exit_p;
      int child_exit = 0;
      int status;
      status = pthread_join (os_thread, &exit_p);
      if (status)
	{
	  perror ("pthread_join");
	  abort ();
	}
      child_exit = *((int *) exit_p);
      if ((exit_status != -1) && exit_status != child_exit)
	{
	  fprintf (stderr, "conflicting exit status (%d) for"
		   " thread %d\n", child_exit, t);
	}
      exit_status = child_exit;
    }
  return exit_status;
}

void
__upc_exit (int status)
{
  upc_info_p u = __upc_info;
  int *status_ptr;
  int thread_id = MYTHREAD;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  __upc_barrier (GUPCR_RUNTIME_BARRIER_ID);
  status_ptr = &u->thread_info[thread_id].exit_status;
  *status_ptr = status;
  pthread_exit (status_ptr);
}

/* upc_global_exit - exit program with given status, terminate
   all other threads.
 
   In the pthreads implementation inside a single process,
   upc_global_exit equates to exit().  */
void
upc_global_exit (int status)
{
  exit (status);
}

#endif /* !GUPCR_USE_PTHREADS */

void
MPIR_Breakpoint (void)
{
}

/* Tell the debugger that this initial process is not to be
   included in the set of processes which form the UPC program.  */
void
MPIR_i_am_starter (void)
{
}

/* Tell the debugger that we're not really MPI after all.  */
void
MPIR_ignore_queues (void)
{
}

/* Tell the debugger to display "main" if we stop immediately
   after acquiring the processes at startup time.  */
void
MPIR_force_to_main (void)
{
}

static void
__upc_notify_debugger_of_abort (const char *mesg)
{
  MPIR_debug_abort_string = mesg;
  MPIR_debug_state = MPIR_DEBUG_ABORTING;
  MPIR_Breakpoint ();
}

/* Issue a fatal UPC runtime error.

   Note: this is called by a UPC thread (process) when a fatal runtime
   error is detected.  */
void
__upc_fatal (const char *msg)
{
  upc_info_p u = __upc_info;
  if (u)
    __upc_acquire_lock (&u->lock);
  fflush (0);
  if (__upc_err_filename && __upc_err_linenum)
    {
      fprintf (stderr, "%s: at %s:%u, UPC error: %s\n",
         __upc_pgm_name, __upc_err_filename, __upc_err_linenum, msg);
    }
  else
    {
      fprintf (stderr, "%s: UPC error: %s\n", __upc_pgm_name, msg);
    }
  fflush (0);
  __upc_notify_debugger_of_abort (msg);
  abort ();
}

/* UPC runtime start up.  */
int
GUPCR_START (int argc, char *argv[])
{
  const char *err_msg = 0;
  int status;
  upc_info_p u;
  __upc_sys_init ();
  __upc_pgm_name = argv[0];
  __upc_validate_pgm_info (__upc_pgm_name);
  __upc_cpu_avoid_set = __upc_affinity_cpu_avoid_new ();
  __upc_process_switches (__upc_pgm_name, &argc, argv);
  u = __upc_init (__upc_pgm_name, &err_msg);
  if (!u)
    {
      fprintf (stderr, "%s: UPC initialization failed.\n"
	       "%s: reason: %s\n", __upc_pgm_name, __upc_pgm_name, err_msg);
      __upc_notify_debugger_of_abort (err_msg);
      abort ();
    }
  __upc_info = u;

  /* Initialize UPC runtime locks.  We do this after __upc_info
     has been allocated and initialized, because __upc_init_lock
     refers to __upc_info on some platforms (eg, SGI/Irix).  */
  __upc_init_lock (&u->lock);
  __upc_init_lock (&u->alloc_lock);
  /* Initialize the VM system */
  __upc_vm_init (u->init_page_alloc);
  /* Initialize thread affinity */
  if (!__upc_affinity_init (u, __upc_cpu_avoid_set, &err_msg))
    {
      fprintf (stderr, "%s: UPC initialization failed.\n"
	       "%s: reason: %s\n", __upc_pgm_name, __upc_pgm_name, err_msg);
      __upc_notify_debugger_of_abort (err_msg);
      abort ();
    }
  __upc_affinity_cpu_avoid_free (__upc_cpu_avoid_set);
  /* Ensure that the upc_forall depth count is initialized to 0.  */
  __upc_forall_depth = 0;
  /* Run the program */
  __upc_run_threads (u, argc, argv);
  status = __upc_monitor_threads ();
  exit (status);
}
