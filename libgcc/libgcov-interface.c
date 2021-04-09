/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgcov.h"
#include "gthr.h"

#if defined(inhibit_libc)

#ifdef L_gcov_reset
void __gcov_reset (void) {}
#endif

#ifdef L_gcov_dump
void __gcov_dump (void) {}
#endif

#else

extern __gthread_mutex_t __gcov_mx ATTRIBUTE_HIDDEN;

#ifdef L_gcov_lock_unlock
#ifdef __GTHREAD_MUTEX_INIT
__gthread_mutex_t __gcov_mx = __GTHREAD_MUTEX_INIT;
#define init_mx_once()
#else
__gthread_mutex_t __gcov_mx;

static void
init_mx (void)
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&__gcov_mx);
}

static void
init_mx_once (void)
{
  static __gthread_once_t once = __GTHREAD_ONCE_INIT;
  __gthread_once (&once, init_mx);
}
#endif

/* Lock critical section for __gcov_dump and __gcov_reset functions.  */

void
__gcov_lock (void)
{
  init_mx_once ();
  __gthread_mutex_lock (&__gcov_mx);
}

/* Unlock critical section for __gcov_dump and __gcov_reset functions.  */

void
__gcov_unlock (void)
{
  __gthread_mutex_unlock (&__gcov_mx);
}
#endif

#ifdef L_gcov_reset

/* Reset all counters to zero.  */

static void
gcov_clear (const struct gcov_info *list)
{
  const struct gcov_info *gi_ptr;

  for (gi_ptr = list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      unsigned f_ix;

      for (f_ix = 0; f_ix < gi_ptr->n_functions; f_ix++)
        {
          unsigned t_ix;
          const struct gcov_fn_info *gfi_ptr = gi_ptr->functions[f_ix];

          if (!gfi_ptr || gfi_ptr->key != gi_ptr)
            continue;
          const struct gcov_ctr_info *ci_ptr = gfi_ptr->ctrs;
          for (t_ix = 0; t_ix != GCOV_COUNTERS; t_ix++)
            {
              if (!gi_ptr->merge[t_ix])
                continue;

              memset (ci_ptr->values, 0, sizeof (gcov_type) * ci_ptr->num);
              ci_ptr++;
            }
        }
    }
}

/* Function that can be called from application to reset counters to zero,
   in order to collect profile in region of interest.  */

void
__gcov_reset_int (void)
{
  struct gcov_root *root;

  /* If we're compatible with the master, iterate over everything,
     otherise just do us.  */
  for (root = __gcov_master.version == GCOV_VERSION
	 ? __gcov_master.root : &__gcov_root; root; root = root->next)
    {
      gcov_clear (root->list);
      root->dumped = 0;
    }
}

/* Exported function __gcov_reset.  */

void
__gcov_reset (void)
{
  __gcov_lock ();

  __gcov_reset_int ();

  __gcov_unlock ();
}

#endif /* L_gcov_reset */

#ifdef L_gcov_dump
/* Function that can be called from application to write profile collected
   so far, in order to collect profile in region of interest.  */

void
__gcov_dump_int (void)
{
  struct gcov_root *root;

  /* If we're compatible with the master, iterate over everything,
     otherise just do us.  */
  for (root = __gcov_master.version == GCOV_VERSION
	 ? __gcov_master.root : &__gcov_root; root; root = root->next)
    __gcov_dump_one (root);
}

/* Exported function __gcov_dump.  */

void
__gcov_dump (void)
{
  __gcov_lock ();

  __gcov_dump_int ();

  __gcov_unlock ();
}

#endif /* L_gcov_dump */

#ifdef L_gcov_fork
/* A wrapper for the fork function.  We reset counters in the child
   so that they are not counted twice.  */

pid_t
__gcov_fork (void)
{
  pid_t pid;
  pid = fork ();
  if (pid == 0)
    {
      __GTHREAD_MUTEX_INIT_FUNCTION (&__gcov_mx);
      /* We do not need locking as we are the only thread in the child.  */
      __gcov_reset_int ();
    }
  return pid;
}
#endif

#ifdef L_gcov_execl
/* A wrapper for the execl function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execl (const char *path, char *arg, ...)
{
  va_list ap, aq;
  unsigned i, length;
  char **args;

  /* Dump counters only, they will be lost after exec.  */
  __gcov_dump ();

  va_start (ap, arg);
  va_copy (aq, ap);

  length = 2;
  while (va_arg (ap, char *))
    length++;
  va_end (ap);

  args = (char **) alloca (length * sizeof (void *));
  args[0] = arg;
  for (i = 1; i < length; i++)
    args[i] = va_arg (aq, char *);
  va_end (aq);

  int ret = execv (path, args);
  /* We reach this code only when execv fails, reset counter then here.  */
  __gcov_reset ();
  return ret;
}
#endif

#ifdef L_gcov_execlp
/* A wrapper for the execlp function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execlp (const char *path, char *arg, ...)
{
  va_list ap, aq;
  unsigned i, length;
  char **args;

  /* Dump counters only, they will be lost after exec.  */
  __gcov_dump ();

  va_start (ap, arg);
  va_copy (aq, ap);

  length = 2;
  while (va_arg (ap, char *))
    length++;
  va_end (ap);

  args = (char **) alloca (length * sizeof (void *));
  args[0] = arg;
  for (i = 1; i < length; i++)
    args[i] = va_arg (aq, char *);
  va_end (aq);

  int ret = execvp (path, args);
  /* We reach this code only when execv fails, reset counter then here.  */
  __gcov_reset ();
  return ret;
}
#endif

#ifdef L_gcov_execle
/* A wrapper for the execle function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execle (const char *path, char *arg, ...)
{
  va_list ap, aq;
  unsigned i, length;
  char **args;
  char **envp;

  /* Dump counters only, they will be lost after exec.  */
  __gcov_dump ();

  va_start (ap, arg);
  va_copy (aq, ap);

  length = 2;
  while (va_arg (ap, char *))
    length++;
  va_end (ap);

  args = (char **) alloca (length * sizeof (void *));
  args[0] = arg;
  for (i = 1; i < length; i++)
    args[i] = va_arg (aq, char *);
  envp = va_arg (aq, char **);
  va_end (aq);

  int ret = execve (path, args, envp);
  /* We reach this code only when execv fails, reset counter then here.  */
  __gcov_reset ();
  return ret;
}
#endif

#ifdef L_gcov_execv
/* A wrapper for the execv function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execv (const char *path, char *const argv[])
{
  /* Dump counters only, they will be lost after exec.  */
  __gcov_dump ();
  int ret = execv (path, argv);
  /* We reach this code only when execv fails, reset counter then here.  */
  __gcov_reset ();
  return ret;
}
#endif

#ifdef L_gcov_execvp
/* A wrapper for the execvp function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execvp (const char *path, char *const argv[])
{
  /* Dump counters only, they will be lost after exec.  */
  __gcov_dump ();
  int ret = execvp (path, argv);
  /* We reach this code only when execv fails, reset counter then here.  */
  __gcov_reset ();
  return ret;
}
#endif

#ifdef L_gcov_execve
/* A wrapper for the execve function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execve (const char *path, char *const argv[], char *const envp[])
{
  /* Dump counters only, they will be lost after exec.  */
  __gcov_dump ();
  int ret = execve (path, argv, envp);
  /* We reach this code only when execv fails, reset counter then here.  */
  __gcov_reset ();
  return ret;
}
#endif
#endif /* inhibit_libc */
