/* OpenACC Runtime Library: CUDA support glue.

   Copyright (C) 2014-2020 Free Software Foundation, Inc.

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

#include "openacc.h"
#include "libgomp.h"
#include "oacc-int.h"
#include <assert.h>

void *
acc_get_current_cuda_device (void)
{
  struct goacc_thread *thr = goacc_thread ();

  void *ret = NULL;
  if (thr && thr->dev && thr->dev->openacc.cuda.get_current_device_func)
    {
      acc_prof_info prof_info;
      acc_api_info api_info;
      bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

      ret = thr->dev->openacc.cuda.get_current_device_func ();

      if (profiling_p)
	{
	  thr->prof_info = NULL;
	  thr->api_info = NULL;
	}
    }

  return ret;
}

void *
acc_get_current_cuda_context (void)
{
  struct goacc_thread *thr = goacc_thread ();

  void *ret = NULL;
  if (thr && thr->dev && thr->dev->openacc.cuda.get_current_context_func)
    {
      acc_prof_info prof_info;
      acc_api_info api_info;
      bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

      ret = thr->dev->openacc.cuda.get_current_context_func ();

      if (profiling_p)
	{
	  thr->prof_info = NULL;
	  thr->api_info = NULL;
	}
    }

  return ret;
}

void *
acc_get_cuda_stream (int async)
{
  struct goacc_thread *thr = goacc_thread ();

  if (!async_valid_p (async))
    return NULL;

  void *ret = NULL;
  if (thr && thr->dev && thr->dev->openacc.cuda.get_stream_func)
    {
      goacc_aq aq = lookup_goacc_asyncqueue (thr, false, async);
      if (!aq)
	return ret;

      acc_prof_info prof_info;
      acc_api_info api_info;
      bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
      if (profiling_p)
	{
	  prof_info.async = async;
	  prof_info.async_queue = prof_info.async;
	}

      ret = thr->dev->openacc.cuda.get_stream_func (aq);

      if (profiling_p)
	{
	  thr->prof_info = NULL;
	  thr->api_info = NULL;
	}
    }

  return ret;
}

int
acc_set_cuda_stream (int async, void *stream)
{
  struct goacc_thread *thr;

  if (!async_valid_p (async) || stream == NULL)
    return 0;

  goacc_lazy_initialize ();

  thr = goacc_thread ();

  int ret = -1;
  if (thr && thr->dev && thr->dev->openacc.cuda.set_stream_func)
    {
      acc_prof_info prof_info;
      acc_api_info api_info;
      bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
      if (profiling_p)
	{
	  prof_info.async = async;
	  prof_info.async_queue = prof_info.async;
	}

      goacc_aq aq = get_goacc_asyncqueue (async);
      /* Due to not using an asyncqueue for "acc_async_sync", this cannot be
	 used to change the CUDA stream associated with "acc_async_sync".  */
      if (!aq)
	{
	  assert (async == acc_async_sync);
	  gomp_debug (0, "Refusing request to set CUDA stream associated"
		      " with \"acc_async_sync\"\n");
	  ret = 0;
	  goto out_prof;
	}
      gomp_mutex_lock (&thr->dev->openacc.async.lock);
      ret = thr->dev->openacc.cuda.set_stream_func (aq, stream);
      gomp_mutex_unlock (&thr->dev->openacc.async.lock);

    out_prof:
      if (profiling_p)
	{
	  thr->prof_info = NULL;
	  thr->api_info = NULL;
	}
    }

  return ret;
}
