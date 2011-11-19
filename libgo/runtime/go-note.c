/* go-note.c -- implement notesleep, notewakeup and noteclear.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* A note is a one-time notification.  noteclear clears the note.
   notesleep waits for a call to notewakeup.  notewakeup wakes up
   every thread waiting on the note.  */

#include "go-assert.h"
#include "runtime.h"

/* We use a single global lock and condition variable.  It would be
   better to use a futex on GNU/Linux.  */

static pthread_mutex_t note_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t note_cond = PTHREAD_COND_INITIALIZER;

/* noteclear is called before any calls to notesleep or
   notewakeup.  */

void
runtime_noteclear (Note* n)
{
  int32 i;

  i = pthread_mutex_lock (&note_lock);
  __go_assert (i == 0);

  n->woken = 0;

  i = pthread_mutex_unlock (&note_lock);
  __go_assert (i == 0);
}

/* Wait until notewakeup is called.  */

void
runtime_notesleep (Note* n)
{
  int32 i;

  i = pthread_mutex_lock (&note_lock);
  __go_assert (i == 0);

  while (!n->woken)
    {
      i = pthread_cond_wait (&note_cond, &note_lock);
      __go_assert (i == 0);
    }

  i = pthread_mutex_unlock (&note_lock);
  __go_assert (i == 0);
}

/* Wake up every thread sleeping on the note.  */

void
runtime_notewakeup (Note *n)
{
  int32 i;

  i = pthread_mutex_lock (&note_lock);
  __go_assert (i == 0);

  n->woken = 1;

  i = pthread_cond_broadcast (&note_cond);
  __go_assert (i == 0);

  i = pthread_mutex_unlock (&note_lock);
  __go_assert (i == 0);
}
