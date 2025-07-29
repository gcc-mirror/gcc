/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
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

#ifndef COUNTER_BARRIER_HDR
#define COUNTER_BARRIER_HDR

#include <pthread.h>

/* Usable as counter barrier and as waitable counter.
   This "class" allows to sync all images acting as a barrier.  For this the
   counter_barrier is to be initialized by the number of images and then later
   calls to counter_barrier_wait() will sync the given number of images.  There
   is no order in which the images will be woken up from their wait.
   Furthermore may this "class" be used as a event queue counter.  To use it in
   that way the counter barrier is to be initialized with zero.  Every "add" to
   the queue then is to be made by incrementing the counter_barrier every take
   by decrementing the queue.  If the queue does not satiesfy the needed number
   of entries they can be waited for.
 */

typedef struct
{
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  volatile int wait_count;
  volatile int curr_wait_group;
  volatile int count;
} counter_barrier;

/* Initialize the counter barrier.  Only to be called once per counter barrier.
   I.e. a counter barrier in shared memory must only be initialized by one
   image.  */

void counter_barrier_init (counter_barrier *, int);

/* Add the given number to the counter barrier.  This signals waiting images
   when the count drops below 0.  This routine is only to be called, when the
   image has taken the counter barrier's lock by some other way.  */

int counter_barrier_add_locked (counter_barrier *, int);

/* Add the given number to the counter barrier.  This signals waiting images
   when the count drops below 0.  */

int counter_barrier_add (counter_barrier *, int);

/* Get the count of the barrier.  */

int counter_barrier_get_count (counter_barrier *);

/* Wait for the count in the barrier drop to or below 0.  */

void counter_barrier_wait (counter_barrier *);

#endif
