/* Copyright (C) 2011 Free Software Foundation, Inc.

   This file is part of the GNU OpenMP Library (libgomp).

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

/* This is a variant of config/posix/sem.h for Tru64 UNIX.  The librt
   sem_init implementation assumes int (4-byte) alignment for sem_t, while
   the type only requires short (2-byte) alignment.  This mismatch causes
   lots of unaligned access warnings from the kernel, so enforce that
   alignment.  */

#ifndef GOMP_SEM_H
#define GOMP_SEM_H 1

#include <semaphore.h>

typedef sem_t gomp_sem_t __attribute__((aligned (__alignof__ (int))));

static inline void gomp_sem_init (gomp_sem_t *sem, int value)
{
  sem_init (sem, 0, value);
}

extern void gomp_sem_wait (gomp_sem_t *sem);

static inline void gomp_sem_post (gomp_sem_t *sem)
{
  sem_post (sem);
}

static inline void gomp_sem_destroy (gomp_sem_t *sem)
{
  sem_destroy (sem);
}
#endif /* GOMP_SEM_H  */
