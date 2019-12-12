/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2019 Free Software Foundation, Inc.

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

#ifdef L_gcov_flush
void __gcov_flush (void) {}
#endif

#ifdef L_gcov_reset
void __gcov_reset (void) {}
#endif

#ifdef L_gcov_dump
void __gcov_dump (void) {}
#endif

#else

/* Some functions we want to bind in this dynamic object, but have an
   overridable global alias.  Unfortunately not all targets support
   aliases, so we just have a forwarding function.  That'll be tail
   called, so the cost is a single jump instruction.*/

#define ALIAS_void_fn(src,dst) \
  void dst (void)	    \
  { src (); }

extern __gthread_mutex_t __gcov_flush_mx ATTRIBUTE_HIDDEN;
extern __gthread_mutex_t __gcov_flush_mx ATTRIBUTE_HIDDEN;

#ifdef L_gcov_flush
#ifdef __GTHREAD_MUTEX_INIT
__gthread_mutex_t __gcov_flush_mx = __GTHREAD_MUTEX_INIT;
#define init_mx_once()
#else
__gthread_mutex_t __gcov_flush_mx;

static void
init_mx (void)
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&__gcov_flush_mx);
}

static void
init_mx_once (void)
{
  static __gthread_once_t once = __GTHREAD_ONCE_INIT;
  __gthread_once (&once, init_mx);
}
#endif

/* Called before fork or exec - write out profile information gathered so
   far and reset it to zero.  This avoids duplication or loss of the
   profile information gathered so far.  */

void
__gcov_flush (void)
{
  init_mx_once ();
  __gthread_mutex_lock (&__gcov_flush_mx);

  __gcov_dump_int ();
  __gcov_reset_int ();

  __gthread_mutex_unlock (&__gcov_flush_mx);
}

#endif /* L_gcov_flush */

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

ALIAS_void_fn (__gcov_reset_int, __gcov_reset);

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

ALIAS_void_fn (__gcov_dump_int, __gcov_dump);

#endif /* L_gcov_dump */

#ifdef L_gcov_fork
/* A wrapper for the fork function.  Flushes the accumulated profiling data, so
   that they are not counted twice.  */

pid_t
__gcov_fork (void)
{
  pid_t pid;
  __gcov_flush ();
  pid = fork ();
  if (pid == 0)
    __GTHREAD_MUTEX_INIT_FUNCTION (&__gcov_flush_mx);
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

  __gcov_flush ();

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

  return execv (path, args);
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

  __gcov_flush ();

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

  return execvp (path, args);
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

  __gcov_flush ();

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

  return execve (path, args, envp);
}
#endif

#ifdef L_gcov_execv
/* A wrapper for the execv function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execv (const char *path, char *const argv[])
{
  __gcov_flush ();
  return execv (path, argv);
}
#endif

#ifdef L_gcov_execvp
/* A wrapper for the execvp function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execvp (const char *path, char *const argv[])
{
  __gcov_flush ();
  return execvp (path, argv);
}
#endif

#ifdef L_gcov_execve
/* A wrapper for the execve function.  Flushes the accumulated
   profiling data, so that they are not lost.  */

int
__gcov_execve (const char *path, char *const argv[], char *const envp[])
{
  __gcov_flush ();
  return execve (path, argv, envp);
}
#endif
#endif /* inhibit_libc */
