/* OpenACC Runtime Library Definitions.

   Copyright (C) 2013-2024 Free Software Foundation, Inc.

   Contributed by Mentor Embedded.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

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

#include <assert.h>
#include <string.h>
#include "openacc.h"
#include "libgomp.h"
#include "oacc-int.h"

static struct goacc_thread *
get_goacc_thread (void)
{
  struct goacc_thread *thr = goacc_thread ();

  if (!thr || !thr->dev)
    gomp_fatal ("no device active");

  return thr;
}

static int
validate_async_val (int async)
{
  if (!async_valid_p (async))
    gomp_fatal ("invalid async-argument: %d", async);

  if (async == acc_async_sync)
    return -1;

  if (async == acc_async_noval)
    return 0;

  if (async >= 0)
    /* TODO: we reserve 0 for acc_async_noval before we can clarify the
       semantics of "default_async".  */
    return 1 + async;
  else
    __builtin_unreachable ();
}

/* Return the asyncqueue to be used for OpenACC async-argument ASYNC.  This
   might return NULL if no asyncqueue is to be used.  Otherwise, if CREATE,
   create the asyncqueue if it doesn't exist yet.

   Unless CREATE, this will not generate any OpenACC Profiling Interface
   events.  */

attribute_hidden struct goacc_asyncqueue *
lookup_goacc_asyncqueue (struct goacc_thread *thr, bool create, int async)
{
  async = validate_async_val (async);
  if (async < 0)
    return NULL;

  struct goacc_asyncqueue *ret_aq = NULL;
  struct gomp_device_descr *dev = thr->dev;

  gomp_mutex_lock (&dev->openacc.async.lock);

  if (!create
      && (async >= dev->openacc.async.nasyncqueue
	  || !dev->openacc.async.asyncqueue[async]))
    goto end;

  if (async >= dev->openacc.async.nasyncqueue)
    {
      int diff = async + 1 - dev->openacc.async.nasyncqueue;
      dev->openacc.async.asyncqueue
	= gomp_realloc (dev->openacc.async.asyncqueue,
			sizeof (goacc_aq) * (async + 1));
      memset (dev->openacc.async.asyncqueue + dev->openacc.async.nasyncqueue,
	      0, sizeof (goacc_aq) * diff);
      dev->openacc.async.nasyncqueue = async + 1;
    }

  if (!dev->openacc.async.asyncqueue[async])
    {
      dev->openacc.async.asyncqueue[async]
	= dev->openacc.async.construct_func (dev->target_id);

      if (!dev->openacc.async.asyncqueue[async])
	{
	  gomp_mutex_unlock (&dev->openacc.async.lock);
	  gomp_fatal ("async %d creation failed", async);
	}
      
      /* Link new async queue into active list.  */
      goacc_aq_list n = gomp_malloc (sizeof (struct goacc_asyncqueue_list));
      n->aq = dev->openacc.async.asyncqueue[async];
      n->next = dev->openacc.async.active;
      dev->openacc.async.active = n;
    }

  ret_aq = dev->openacc.async.asyncqueue[async];

 end:
  gomp_mutex_unlock (&dev->openacc.async.lock);
  return ret_aq;
}

/* Return the asyncqueue to be used for OpenACC async-argument ASYNC.  This
   might return NULL if no asyncqueue is to be used.  Otherwise, create the
   asyncqueue if it doesn't exist yet.  */

attribute_hidden struct goacc_asyncqueue *
get_goacc_asyncqueue (int async)
{
  struct goacc_thread *thr = get_goacc_thread ();
  return lookup_goacc_asyncqueue (thr, true, async);
}

int
acc_async_test (int async)
{
  struct goacc_thread *thr = goacc_thread ();

  if (!thr || !thr->dev)
    gomp_fatal ("no device active");

  goacc_aq aq = lookup_goacc_asyncqueue (thr, false, async);
  if (!aq)
    return 1;

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
    }

  int res = thr->dev->openacc.async.test_func (aq);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }

  return res;
}

int
acc_async_test_all (void)
{
  struct goacc_thread *thr = get_goacc_thread ();

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

  int ret = 1;
  gomp_mutex_lock (&thr->dev->openacc.async.lock);
  for (goacc_aq_list l = thr->dev->openacc.async.active; l; l = l->next)
    if (!thr->dev->openacc.async.test_func (l->aq))
      {
	ret = 0;
	break;
      }
  gomp_mutex_unlock (&thr->dev->openacc.async.lock);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }

  return ret;
}

void
acc_wait (int async)
{
  struct goacc_thread *thr = get_goacc_thread ();

  goacc_aq aq = lookup_goacc_asyncqueue (thr, false, async);
  if (!aq)
    return;

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
    }

  if (!thr->dev->openacc.async.synchronize_func (aq))
    gomp_fatal ("wait on %d failed", async);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}

/* acc_async_wait is an OpenACC 1.0 compatibility name for acc_wait.  */
#ifdef HAVE_ATTRIBUTE_ALIAS
strong_alias (acc_wait, acc_async_wait)
#else
void
acc_async_wait (int async)
{
  acc_wait (async);
}
#endif

void
acc_wait_async (int async1, int async2)
{
  struct goacc_thread *thr = get_goacc_thread ();

  goacc_aq aq1 = lookup_goacc_asyncqueue (thr, false, async1);
  /* TODO: Is this also correct for acc_async_sync, assuming that in this case,
     we'll always be synchronous anyways?  */
  if (!aq1)
    return;

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async2;
      prof_info.async_queue = prof_info.async;
    }

  goacc_aq aq2 = lookup_goacc_asyncqueue (thr, true, async2);
  /* An async queue is always synchronized with itself.  */
  if (aq1 == aq2)
    goto out_prof;

  if (aq2)
    {
      if (!thr->dev->openacc.async.serialize_func (aq1, aq2))
	gomp_fatal ("ordering of async ids %d and %d failed", async1, async2);
    }
  else
    {
      /* TODO: Local thread synchronization.
	 Necessary for the "async2 == acc_async_sync" case, or can just skip?  */
      if (!thr->dev->openacc.async.synchronize_func (aq1))
	gomp_fatal ("wait on %d failed", async1);
    }

 out_prof:
  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}

void
acc_wait_all (void)
{
  struct goacc_thread *thr = goacc_thread ();

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

  bool ret = true;
  gomp_mutex_lock (&thr->dev->openacc.async.lock);
  for (goacc_aq_list l = thr->dev->openacc.async.active; l; l = l->next)
    ret &= thr->dev->openacc.async.synchronize_func (l->aq);
  gomp_mutex_unlock (&thr->dev->openacc.async.lock);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }

  if (!ret)
    gomp_fatal ("wait all failed");
}

/* acc_async_wait_all is an OpenACC 1.0 compatibility name for acc_wait_all.  */
#ifdef HAVE_ATTRIBUTE_ALIAS
strong_alias (acc_wait_all, acc_async_wait_all)
#else
void
acc_async_wait_all (void)
{
  acc_wait_all ();
}
#endif

void
acc_wait_all_async (int async)
{
  struct goacc_thread *thr = get_goacc_thread ();

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
    }

  goacc_aq waiting_queue = lookup_goacc_asyncqueue (thr, true, async);

  bool ret = true;
  gomp_mutex_lock (&thr->dev->openacc.async.lock);
  for (goacc_aq_list l = thr->dev->openacc.async.active; l; l = l->next)
    {
      if (waiting_queue)
	ret &= thr->dev->openacc.async.serialize_func (l->aq, waiting_queue);
      else
	/* TODO: Local thread synchronization.
	   Necessary for the "async2 == acc_async_sync" case, or can just skip?  */
	ret &= thr->dev->openacc.async.synchronize_func (l->aq);
    }
  gomp_mutex_unlock (&thr->dev->openacc.async.lock);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }

  if (!ret)
    gomp_fatal ("wait all async(%d) failed", async);
}

void
GOACC_wait (int async, int num_waits, ...)
{
  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();

  /* No nesting.  */
  assert (thr->prof_info == NULL);
  assert (thr->api_info == NULL);
  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
    }

  if (num_waits)
    {
      va_list ap;

      va_start (ap, num_waits);
      goacc_wait (async, num_waits, &ap);
      va_end (ap);
    }
  else if (async == acc_async_sync)
    acc_wait_all ();
  else
    acc_wait_all_async (async);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}

attribute_hidden void
goacc_wait (int async, int num_waits, va_list *ap)
{
  while (num_waits--)
    {
      int qid = va_arg (*ap, int);

      /* Waiting on ACC_ASYNC_NOVAL maps to 'wait all'.  */
      if (qid == acc_async_noval)
	{
	  if (async == acc_async_sync)
	    acc_wait_all ();
	  else
	    acc_wait_all_async (async);
	  break;
	}

      if (async == acc_async_sync)
	acc_wait (qid);
      else if (qid == async)
	/* If we're waiting on the same asynchronous queue as we're
	   launching on, the queue itself will order work as
	   required, so there's no need to wait explicitly.  */
	;
      else
	acc_wait_async (qid, async);
    }
}

attribute_hidden void
goacc_async_free (struct gomp_device_descr *devicep,
		  struct goacc_asyncqueue *aq, void *ptr)
{
  if (!aq)
    free (ptr);
  else
    devicep->openacc.async.queue_callback_func (aq, free, ptr);
}

/* This function initializes the asyncqueues for the device specified by
   DEVICEP.  TODO DEVICEP must be locked on entry, and remains locked on
   return.  */

attribute_hidden void
goacc_init_asyncqueues (struct gomp_device_descr *devicep)
{
  devicep->openacc.async.nasyncqueue = 0;
  devicep->openacc.async.asyncqueue = NULL;
  devicep->openacc.async.active = NULL;
  gomp_mutex_init (&devicep->openacc.async.lock);
}

/* This function finalizes the asyncqueues for the device specified by DEVICEP.
   TODO DEVICEP must be locked on entry, and remains locked on return.  */

attribute_hidden bool
goacc_fini_asyncqueues (struct gomp_device_descr *devicep)
{
  bool ret = true;
  gomp_mutex_lock (&devicep->openacc.async.lock);
  if (devicep->openacc.async.nasyncqueue > 0)
    {
      goacc_aq_list next;
      for (goacc_aq_list l = devicep->openacc.async.active; l; l = next)
	{
	  ret &= devicep->openacc.async.destruct_func (l->aq);
	  next = l->next;
	  free (l);
	}
      free (devicep->openacc.async.asyncqueue);
      devicep->openacc.async.nasyncqueue = 0;
      devicep->openacc.async.asyncqueue = NULL;
      devicep->openacc.async.active = NULL;
    }
  gomp_mutex_unlock (&devicep->openacc.async.lock);
  gomp_mutex_destroy (&devicep->openacc.async.lock);
  return ret;
}
