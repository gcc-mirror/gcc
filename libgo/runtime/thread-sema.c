// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"

#include <errno.h>
#include <stdlib.h>
#include <time.h>
#include <semaphore.h>

/* Create a semaphore.  */

uintptr
runtime_semacreate(void)
{
  sem_t *p;

  /* Call malloc rather than runtime_malloc.  This will allocate space
     on the C heap.  We can't call runtime_malloc here because it
     could cause a deadlock.  */
  p = malloc (sizeof (sem_t));
  if (sem_init (p, 0, 0) != 0)
    runtime_throw ("sem_init");
  return (uintptr) p;
}

/* Acquire m->waitsema.  */

int32
runtime_semasleep (int64 ns)
{
  int r;

  if (ns >= 0)
    {
      struct timespec ts;

      ns += runtime_nanotime ();
      ts.tv_sec = ns / 1000000000LL;
      ts.tv_nsec = ns % 1000000000LL;
      r = sem_timedwait ((sem_t *) m->waitsema, &ts);
      if (r != 0)
	{
	  if (errno == ETIMEDOUT || errno == EINTR)
	    return -1;
	  runtime_throw ("sema_timedwait");
	}
      return 0;
    }

  while (sem_wait ((sem_t *) m->waitsema) != 0)
    {
      if (errno == EINTR)
	continue;
      runtime_throw ("sem_wait");
    }

  return 0;
}

/* Wake up mp->waitsema.  */

void
runtime_semawakeup (M *mp)
{
  if (sem_post ((sem_t *) mp->waitsema) != 0)
    runtime_throw ("sem_post");
}

void
runtime_osinit(void)
{
}
