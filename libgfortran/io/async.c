/* Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Nicolas Koenig

   This file is part of the GNU Fortran runtime library (libgfortran).

   Libgfortran is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgfortran is distributed in the hope that it will be useful,
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

#include "libgfortran.h"

#define _GTHREAD_USE_COND_INIT_FUNC
#include "../../libgcc/gthr.h"
#include "io.h"
#include "fbuf.h"
#include "format.h"
#include "unix.h"
#include <string.h>
#include <assert.h>

#include <sys/types.h>

#include "async.h"
#if ASYNC_IO

DEBUG_LINE (__thread const char *aio_prefix = MPREFIX);

DEBUG_LINE (__gthread_mutex_t debug_queue_lock = __GTHREAD_MUTEX_INIT;)
DEBUG_LINE (aio_lock_debug *aio_debug_head = NULL;)

/* Current unit for asynchronous I/O.  Needed for error reporting.  */

__thread gfc_unit *thread_unit = NULL;

/* Queue entry for the asynchronous I/O entry.  */
typedef struct transfer_queue
{
  enum aio_do type;
  struct transfer_queue *next;
  struct st_parameter_dt *new_pdt;
  transfer_args arg;
  _Bool has_id;
  int read_flag;
} transfer_queue;

struct error {
  st_parameter_dt *dtp;
  int id;
};

/* Helper function to exchange the old vs. a new PDT.  */

static void
update_pdt (st_parameter_dt **old, st_parameter_dt *new) {
  st_parameter_dt *temp;
  NOTE ("Changing pdts, current_unit = %p", (void *) (new->u.p.current_unit));
  temp = *old;
  *old = new;
  if (temp)
    free (temp);
}

/* Destroy an adv_cond structure.  */

static void
destroy_adv_cond (struct adv_cond *ac)
{
  T_ERROR (__gthread_mutex_destroy, &ac->lock);
  T_ERROR (__gthread_cond_destroy, &ac->signal);
}

/* Function invoked as start routine for a new asynchronous I/O unit.
   Contains the main loop for accepting requests and handling them.  */

static void *
async_io (void *arg)
{
  DEBUG_LINE (aio_prefix = TPREFIX);
  transfer_queue *ctq = NULL, *prev = NULL;
  gfc_unit *u = (gfc_unit *) arg;
  async_unit *au = u->au;
  LOCK (&au->lock);
  thread_unit = u;
  au->thread = __gthread_self ();
  while (true)
    {
      /* Main loop.  At this point, au->lock is always held. */
      WAIT_SIGNAL_MUTEX (&au->work, au->tail != NULL, &au->lock);
      LOCK (&au->lock);
      ctq = au->head;
      prev = NULL;
      /* Loop over the queue entries until they are finished.  */
      while (ctq)
	{
	  if (prev)
	    free (prev);
	  prev = ctq;
	  if (!au->error.has_error)
	    {
	      UNLOCK (&au->lock);

	      switch (ctq->type)
		{
		case AIO_WRITE_DONE:
		  NOTE ("Finalizing write");
		  st_write_done_worker (au->pdt);
		  UNLOCK (&au->io_lock);
		  break;

		case AIO_READ_DONE:
		  NOTE ("Finalizing read");
		  st_read_done_worker (au->pdt);
		  UNLOCK (&au->io_lock);
		  break;

		case AIO_DATA_TRANSFER_INIT:
		  NOTE ("Data transfer init");
		  LOCK (&au->io_lock);
		  update_pdt (&au->pdt, ctq->new_pdt);
		  data_transfer_init_worker (au->pdt, ctq->read_flag);
		  break;

		case AIO_TRANSFER_SCALAR:
		  NOTE ("Starting scalar transfer");
		  ctq->arg.scalar.transfer (au->pdt, ctq->arg.scalar.arg_bt,
					    ctq->arg.scalar.data,
					    ctq->arg.scalar.i,
					    ctq->arg.scalar.s1,
					    ctq->arg.scalar.s2);
		  break;

		case AIO_TRANSFER_ARRAY:
		  NOTE ("Starting array transfer");
		  NOTE ("ctq->arg.array.desc = %p",
			(void *) (ctq->arg.array.desc));
		  transfer_array_inner (au->pdt, ctq->arg.array.desc,
					ctq->arg.array.kind,
					ctq->arg.array.charlen);
		  free (ctq->arg.array.desc);
		  break;

		case AIO_CLOSE:
		  NOTE ("Received AIO_CLOSE");
		  goto finish_thread;

		default:
		  internal_error (NULL, "Invalid queue type");
		  break;
		}
	      LOCK (&au->lock);
	      if (unlikely (au->error.has_error))
		au->error.last_good_id = au->id.low - 1;
	    }
	  else
	    {
	      if (ctq->type == AIO_WRITE_DONE || ctq->type == AIO_READ_DONE)
		{
		  UNLOCK (&au->io_lock);
		}
	      else if (ctq->type == AIO_CLOSE)
		{
		  NOTE ("Received AIO_CLOSE during error condition");
		  UNLOCK (&au->lock);
		  goto finish_thread;
		}
	    }

  	  NOTE ("Next ctq, current id: %d", au->id.low);
  	  if (ctq->has_id && au->id.waiting == au->id.low++)
	    SIGNAL (&au->id.done);

	  ctq = ctq->next;
	}
      au->tail = NULL;
      au->head = NULL;
      au->empty = 1;
      UNLOCK (&au->lock);
      SIGNAL (&au->emptysignal);
      LOCK (&au->lock);
    }
 finish_thread:
  au->tail = NULL;
  au->head = NULL;
  au->empty = 1;
  SIGNAL (&au->emptysignal);
  free (ctq);
  return NULL;
}

/* Free an asynchronous unit.  */

static void
free_async_unit (async_unit *au)
{
  if (au->tail)
    internal_error (NULL, "Trying to free nonempty asynchronous unit");

  destroy_adv_cond (&au->work);
  destroy_adv_cond (&au->emptysignal);
  destroy_adv_cond (&au->id.done);
  T_ERROR (__gthread_mutex_destroy, &au->lock);
  free (au);
}

/* Initialize an adv_cond structure.  */

static void
init_adv_cond (struct adv_cond *ac)
{
  ac->pending = 0;
  __GTHREAD_MUTEX_INIT_FUNCTION (&ac->lock);
  __gthread_cond_init_function (&ac->signal);
}

/* Initialize an asyncronous unit, returning zero on success,
 nonzero on failure.  It also sets u->au.  */

void
init_async_unit (gfc_unit *u)
{
  async_unit *au;
  if (!__gthread_active_p ())
    {
      u->au = NULL;
      return;
    }
  
  au = (async_unit *) xmalloc (sizeof (async_unit));
  u->au = au;
  init_adv_cond (&au->work);
  init_adv_cond (&au->emptysignal);
  __GTHREAD_MUTEX_INIT_FUNCTION (&au->lock);
  __GTHREAD_MUTEX_INIT_FUNCTION (&au->io_lock);
  LOCK (&au->lock);
  T_ERROR (__gthread_create, &au->thread, &async_io, (void *) u);
  au->pdt = NULL;
  au->head = NULL;
  au->tail = NULL;
  au->empty = true;
  au->id.waiting = -1;
  au->id.low = 0;
  au->id.high = 0;
  au->error.fatal_error = 0;
  au->error.has_error = 0;
  au->error.last_good_id = 0;
  init_adv_cond (&au->id.done);
  UNLOCK (&au->lock);
}

/* Enqueue a transfer statement.  */

void
enqueue_transfer (async_unit *au, transfer_args *arg, enum aio_do type)
{
  transfer_queue *tq = calloc (sizeof (transfer_queue), 1);
  tq->arg = *arg;
  tq->type = type;
  tq->has_id = 0;
  LOCK (&au->lock);
  if (!au->tail)
    au->head = tq;
  else
    au->tail->next = tq;
  au->tail = tq;
  REVOKE_SIGNAL (&(au->emptysignal));
  au->empty = false;
  UNLOCK (&au->lock);
  SIGNAL (&au->work);
}

/* Enqueue an st_write_done or st_read_done which contains an ID.  */

int
enqueue_done_id (async_unit *au, enum aio_do type)
{
  int ret;
  transfer_queue *tq = calloc (sizeof (transfer_queue), 1);

  tq->type = type;
  tq->has_id = 1;
  LOCK (&au->lock);
  if (!au->tail)
    au->head = tq;
  else
    au->tail->next = tq;
  au->tail = tq;
  REVOKE_SIGNAL (&(au->emptysignal));
  au->empty = false;
  ret = au->id.high++;
  NOTE ("Enqueue id: %d", ret);
  UNLOCK (&au->lock);
  SIGNAL (&au->work);
  return ret;
}

/* Enqueue an st_write_done or st_read_done without an ID.  */

void
enqueue_done (async_unit *au, enum aio_do type)
{
  transfer_queue *tq = calloc (sizeof (transfer_queue), 1);
  tq->type = type;
  tq->has_id = 0;
  LOCK (&au->lock);
  if (!au->tail)
    au->head = tq;
  else
    au->tail->next = tq;
  au->tail = tq;
  REVOKE_SIGNAL (&(au->emptysignal));
  au->empty = false;
  UNLOCK (&au->lock);
  SIGNAL (&au->work);
}

/* Enqueue a CLOSE statement.  */

void
enqueue_close (async_unit *au)
{
  transfer_queue *tq = calloc (sizeof (transfer_queue), 1);

  tq->type = AIO_CLOSE;
  LOCK (&au->lock);
  if (!au->tail)
    au->head = tq;
  else
    au->tail->next = tq;
  au->tail = tq;
  REVOKE_SIGNAL (&(au->emptysignal));
  au->empty = false;
  UNLOCK (&au->lock);
  SIGNAL (&au->work);
}

/* The asynchronous unit keeps the currently active PDT around.
   This function changes that to the current one.  */

void
enqueue_data_transfer_init (async_unit *au, st_parameter_dt *dt, int read_flag)
{
  st_parameter_dt *new = xmalloc (sizeof (st_parameter_dt));
  transfer_queue *tq = xmalloc (sizeof (transfer_queue));

  memcpy ((void *) new, (void *) dt, sizeof (st_parameter_dt));

  NOTE ("dt->internal_unit_desc = %p", dt->internal_unit_desc);
  NOTE ("common.flags & mask = %d", dt->common.flags & IOPARM_LIBRETURN_MASK);
  tq->next = NULL;
  tq->type = AIO_DATA_TRANSFER_INIT;
  tq->read_flag = read_flag;
  tq->has_id = 0;
  tq->new_pdt = new;
  LOCK (&au->lock);

  if (!au->tail)
    au->head = tq;
  else
    au->tail->next = tq;
  au->tail = tq;
  REVOKE_SIGNAL (&(au->emptysignal));
  au->empty = 0;
  UNLOCK (&au->lock);
  SIGNAL (&au->work);
}

/* Collect the errors that may have happened asynchronously.  Return true if
   an error has been encountered.  */

bool
collect_async_errors (st_parameter_common *cmp, async_unit *au)
{
  bool has_error = au->error.has_error;

  if (has_error)
    {
      if (generate_error_common (cmp, au->error.family, au->error.message))
	{
	  au->error.has_error = 0;
	  au->error.cmp = NULL;
	}
      else
	{
	  /* The program will exit later.  */
	  au->error.fatal_error = true;
	}
    }
  return has_error;
}

/* Perform a wait operation on an asynchronous unit with an ID specified,
   which means collecting the errors that may have happened asynchronously.
   Return true if an error has been encountered.  */

bool
async_wait_id (st_parameter_common *cmp, async_unit *au, int i)
{
  bool ret;

  if (au == NULL)
    return false;

  if (cmp == NULL)
    cmp = au->error.cmp;

  if (au->error.has_error)
    {
      if (i <= au->error.last_good_id)
	return false;

      return collect_async_errors (cmp, au);
    }

  LOCK (&au->lock);
  NOTE ("Waiting for id %d", i);
  if (au->id.waiting < i)
    au->id.waiting = i;
  UNLOCK (&au->lock);
  SIGNAL (&(au->work));
  LOCK (&au->lock);
  WAIT_SIGNAL_MUTEX (&(au->id.done),
		     (au->id.low >= au->id.waiting || au->empty), &au->lock);
  LOCK (&au->lock);
  ret = collect_async_errors (cmp, au);
  UNLOCK (&au->lock);
  return ret;
}

/* Perform a wait operation an an asynchronous unit without an ID.  */

bool
async_wait (st_parameter_common *cmp, async_unit *au)
{
  bool ret;

  if (au == NULL)
    return false;

  if (cmp == NULL)
    cmp = au->error.cmp;

  SIGNAL (&(au->work));
  LOCK (&(au->lock));

  if (au->empty)
    {
      ret = collect_async_errors (cmp, au);
      UNLOCK (&au->lock);
      return ret;
    }

  WAIT_SIGNAL_MUTEX (&(au->emptysignal), (au->empty), &au->lock);
  ret = collect_async_errors (cmp, au);
  return ret;
}

/* Close an asynchronous unit.  */

void
async_close (async_unit *au)
{
  if (au == NULL)
    return;

  NOTE ("Closing async unit");
  enqueue_close (au);
  T_ERROR (__gthread_join, au->thread, NULL);
  free_async_unit (au);
}

#else

/* Only set u->au to NULL so no async I/O will happen.  */

void
init_async_unit (gfc_unit *u)
{
  u->au = NULL;
  return;
}

/* Do-nothing function, which will not be called.  */

void
enqueue_transfer (async_unit *au, transfer_args *arg, enum aio_do type)
{
  return;
}

/* Do-nothing function, which will not be called.  */

int
enqueue_done_id (async_unit *au, enum aio_do type)
{
  return 0;
}

/* Do-nothing function, which will not be called.  */

void
enqueue_done (async_unit *au, enum aio_do type)
{
  return;
}

/* Do-nothing function, which will not be called.  */

void
enqueue_close (async_unit *au)
{
  return;
}

/* Do-nothing function, which will not be called.  */

void
enqueue_data_transfer_init (async_unit *au, st_parameter_dt *dt, int read_flag)
{
  return;
}

/* Do-nothing function, which will not be called.  */

bool
collect_async_errors (st_parameter_common *cmp, async_unit *au)
{
  return false;
}

/* Do-nothing function, which will not be called.  */

bool
async_wait_id (st_parameter_common *cmp, async_unit *au, int i)
{
  return false;
}

/* Do-nothing function, which will not be called.  */

bool
async_wait (st_parameter_common *cmp, async_unit *au)
{
  return false;
}

/* Do-nothing function, which will not be called.  */

void
async_close (async_unit *au)
{
  return;
}

#endif
