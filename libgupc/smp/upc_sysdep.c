/* Copyright (C) 2003-2014 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_sup.h"
#include "upc_sync.h"

#ifdef __sgi__
#ifndef _SC_NPROCESSORS_ONLN
#define _SC_NPROCESSORS_ONLN _SC_NPROC_ONLN
#endif
#endif

int __upc_num_cpus;

void
__upc_sys_init ()
{
  __upc_num_cpus = (int) sysconf (_SC_NPROCESSORS_ONLN);

  /* Make sure that this main process is the process group leader */
  if (getpgrp() != getpid())
    {
      if (setpgid(0, 0) == -1)
        { perror("setpgid"); exit (2); }
    }
}

char *__upc_strsignal (sig)
     int sig;
{
  static char sigbuf[64];
#if defined(__sgi__) || defined(__sun__)
  char **sys_siglist = _sys_siglist;
  const int nsig = _sys_nsig;
#else
#ifndef __NetBSD__ // signal.h has pointer decl instead of array
  extern const char * const sys_siglist[];
#endif
  const int nsig = NSIG;
#endif
  if (sig > 0 && sig < nsig)
    return (char *)sys_siglist[sig];
  else
    return (sprintf (sigbuf, "signal number %d", sig), sigbuf);
}

#ifndef __upc_atomic_cas
/* If a builtin implementation of __upc_atomic_cas was found,
   then the symbol will be defined as a pre-processor macro.
   Otherwise, implement the function out-of-line.  */
#ifdef __sgi__
int
__upc_atomic_cas (os_atomic_p ptr, os_atomic_t old, os_atomic_t new)
{
  upc_info_p u = __upc_info;
  return uscas ((void *)ptr, old, new, (usptr_t *)u->runtime_heap);
}
#elif __i386__

#define LOCK_PREFIX "lock ; "

int
__upc_atomic_cas (os_atomic_p ptr, os_atomic_t old, os_atomic_t new)
{
  os_atomic_t prev;
  __asm__ __volatile__(LOCK_PREFIX "cmpxchgl %1,%2"
		       : "=a"(prev)
		       : "q"(new), "m"(*ptr), "0"(old)
		       : "memory");
  return prev == old;
}
#elif __x86_64__

#define LOCK_PREFIX "lock ; "

int
__upc_atomic_cas (os_atomic_p ptr, os_atomic_t old, os_atomic_t new)
{
  os_atomic_t prev;
  __asm__ __volatile__(LOCK_PREFIX "cmpxchgl %1,%2"
		       : "=a"(prev)
		       : "q"(new), "m"(*ptr), "0"(old)
		       : "memory");
  return prev == old;
}

#elif __ia64__

#include <linux/types.h>
#include <asm/intrinsics.h>

int
__upc_atomic_cas (os_atomic_p ptr, os_atomic_t old, os_atomic_t new)
{
  os_atomic_t prev;
  prev = cmpxchg (ptr, old, new);
  return prev == old;
}
#else
  #error "__upc_atomic_cas not implemented on this target"
#endif
#endif /* ! __upc_atomic_cas */

int
__upc_atomic_get_bit (os_atomic_p bits, int bitnum)
{
  os_atomic_t *word_ptr = bits + (bitnum / OS_BITS_PER_ATOMIC_WORD);
  os_atomic_t bit = (1 << (bitnum % OS_BITS_PER_ATOMIC_WORD));
  os_atomic_t word = *word_ptr;
  GUPCR_READ_FENCE();
  return (word & bit) != 0;
}

void
__upc_atomic_set_bit (os_atomic_p bits, int bitnum)
{
  os_atomic_t *word = bits + (bitnum / OS_BITS_PER_ATOMIC_WORD);
  os_atomic_t bit = (1 << (bitnum % OS_BITS_PER_ATOMIC_WORD));
  os_atomic_t old_val, new_val;
  do
    {
      old_val = *word;
      new_val = old_val | bit;
    }
  while (!__upc_atomic_cas (word, old_val, new_val));
}

os_heap_p
__upc_create_runtime_heap (size_t ARG_UNUSED (max_size),
                           const char **ARG_UNUSED (err_msg))
{
  os_heap_p heap;
#ifdef __sgi__
  /* Create the shared arena. */
  (void) usconfig (CONF_INITSIZE, max_size);
  heap = (os_heap_p) usinit (DEV_ZERO);
  if (!heap)
    { *err_msg = strerror(errno); return 0; }
#else
  /* On platforms other than SGI/Irix we don't
     need a heap, because mmap() will work well
     for the limited number of data structures
     allocated during runtime initialization.  */
  heap = (void *)-1;
#endif
  return heap;
}

void *
__upc_runtime_alloc (size_t size, os_heap_p *ARG_UNUSED (heap),
                     const char **err_msg)
{
  void *alloc;
#ifdef __sgi__
  alloc = (void *) usmalloc (size, (usptr_t *)*heap);
  if (!alloc)
    { *err_msg = strerror(errno); return 0; }
#else
  alloc = mmap ((void *) 0, size,
	        PROT_READ | PROT_WRITE,
	        MAP_SHARED | MAP_ANONYMOUS, -1, OFFSET_ZERO);
  if (!alloc || alloc == MAP_ERROR)
    { *err_msg = strerror(errno); return 0; }
#endif
  return alloc;
}

void
__upc_init_lock (lock)
     os_lock_p lock;
{
#ifdef __sgi__
  {
    upc_info_p u = __upc_info;
    if (!u)
      __upc_fatal ("UPC runtime not initialized");
    *lock = (os_lock_t) usnewlock ((usptr_t *) u->runtime_heap);
    if (!*lock)
      { perror ("__upc_init_lock"); abort (); }
  }
#else
  *lock = 0;
#endif
}

void
__upc_acquire_lock (lock)
     os_lock_p lock;
{
  if (!lock)
    __upc_fatal ("NULL shared pointer passed to UPC lock operation");
#ifdef __sgi__
  {
    int status;
    status = ussetlock(*lock);
    if (status == 0)
      __upc_fatal ("upc_lock() could not acquire lock");
    else if (status < 0)
      { perror ("upc_acquire_lock"); abort (); }
  }
#else
  __upc_spin_until (__upc_atomic_cas ((os_atomic_p) lock, 0, 1));
#endif
  GUPCR_FENCE();
}

int
__upc_try_acquire_lock (lock)
     os_lock_p lock;
{
  int status;
  if (!lock)
    __upc_fatal ("NULL shared pointer passed to UPC lock operation");
#ifdef __sgi__
  status = uscsetlock(*lock, 0);
  if (status < 0)
    { perror ("upc_try_acquire_lock"); abort (); }
#else
  status = __upc_atomic_cas ((os_atomic_p) lock, 0, 1);
#endif
  if (status)
    GUPCR_FENCE();
  return status;
}

void
__upc_release_lock (lock)
     os_lock_p lock;
{
  if (!lock)
    __upc_fatal ("NULL shared pointer passed to UPC lock operation");
  GUPCR_FENCE();
#ifdef __sgi__
  {
    int status;
    status = usunsetlock(*lock);
    if (status > 0)
      __upc_fatal ("upc_unlock() could not release lock");
    else if (status < 0)
      { perror ("upc_unlock"); abort (); }
  }
#else
  *((os_atomic_p) lock) = 0;
#endif
}

/* Given a "tag" (a relative filename ending in XXXXXX),
   create a temporary file using the tag.
   Return a file descriptor associated with the newly
   created temporary file.
   [see: http://www.linux.com/howtos/Secure-Programs-HOWTO/avoid-race.shtml]  */

int
__upc_create_temp_file (const char *tag, char *tmp_fname,
                        const char **err_msg)
{
  const char *tmpdir = NULL;
  mode_t old_mode;
  int fd;
  if ((getuid () == geteuid ()) && (getgid () == getegid ()))
    {
      tmpdir = getenv ("TMPDIR");
      if (!tmpdir)
        tmpdir = getenv ("TMP");
    }
  if (!tmpdir)
    tmpdir = "/tmp";
  sprintf (tmp_fname, "%s/%s", tmpdir, tag);
  /* Create file with restrictive permissions */
  old_mode = umask (077);
  fd = mkstemp (tmp_fname);
  (void) umask (old_mode);
  if (fd < 0)
    *err_msg = "Couldn't open temporary file";
  return fd;
}

/* Create a file that will be used as the backing store
   for the UPC program's global shared memory. Return
   a file descriptor that can subsequently be used
   to mmap() the file.  If an error is encountered,
   fd will be set to -1, and err_msg will contain
   a descriptive error message.  */
int
__upc_create_global_mem_file (char *tmp_fname, const char **err_msg)
{
  int fd;
  char fname_template[30];
  sprintf (fname_template, "upc_shmem.%d.XXXXXX", (int)getpid());
  fd = __upc_create_temp_file (fname_template, tmp_fname, err_msg);
  return fd;
}
