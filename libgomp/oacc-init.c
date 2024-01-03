/* OpenACC Runtime initialization routines

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

#include "libgomp.h"
#include "oacc-int.h"
#include "openacc.h"
#include <assert.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <string.h>

/* This lock is used to protect access to cached_base_dev, dispatchers and
   the (abstract) initialisation state of attached offloading devices.  */

static gomp_mutex_t acc_device_lock;

static gomp_mutex_t acc_init_state_lock;
static enum { uninitialized, initializing, initialized } acc_init_state
  = uninitialized;
static pthread_t acc_init_thread;

/* A cached version of the dispatcher for the global "current" accelerator type,
   e.g. used as the default when creating new host threads.  This is the
   device-type equivalent of goacc_device_num (which specifies which device to
   use out of potentially several of the same type).  If there are several
   devices of a given type, this points at the first one.  */

static struct gomp_device_descr *cached_base_dev = NULL;

#if defined HAVE_TLS || defined USE_EMUTLS
__thread struct goacc_thread *goacc_tls_data;
#else
pthread_key_t goacc_tls_key;
#endif
static pthread_key_t goacc_cleanup_key;

static struct goacc_thread *goacc_threads;
static gomp_mutex_t goacc_thread_lock;

/* An array of dispatchers for device types, indexed by the type.  This array
   only references "base" devices, and other instances of the same type are
   found by simply indexing from each such device (which are stored linearly,
   grouped by device in target.c:devices).  */
static struct gomp_device_descr *dispatchers[_ACC_device_hwm] = { 0 };

attribute_hidden void
goacc_register (struct gomp_device_descr *disp)
{
  /* Only register the 0th device here.  */
  if (disp->target_id != 0)
    return;

  gomp_mutex_lock (&acc_device_lock);

  assert (acc_device_type (disp->type) != acc_device_none
	  && acc_device_type (disp->type) != acc_device_default
	  && acc_device_type (disp->type) != acc_device_not_host);
  assert (!dispatchers[disp->type]);
  dispatchers[disp->type] = disp;

  gomp_mutex_unlock (&acc_device_lock);
}

static bool
known_device_type_p (acc_device_t d)
{
  return d >= 0 && d < _ACC_device_hwm;
}

static void
unknown_device_type_error (acc_device_t invalid_type)
{
  gomp_fatal ("unknown device type %u", invalid_type);
}

/* OpenACC names some things a little differently.  */

static const char *
get_openacc_name (const char *name)
{
  if (strcmp (name, "gcn") == 0)
    return "radeon";
  else if (strcmp (name, "nvptx") == 0)
    return "nvidia";
  else
    return name;
}

static const char *
name_of_acc_device_t (enum acc_device_t type)
{
  switch (type)
    {
    case acc_device_none: return "none";
    case acc_device_default: return "default";
    case acc_device_host: return "host";
    case acc_device_not_host: return "not_host";
    case acc_device_nvidia: return "nvidia";
    case acc_device_radeon: return "radeon";
    default: unknown_device_type_error (type);
    }
  __builtin_unreachable ();
}

/* ACC_DEVICE_LOCK must be held before calling this function.  If FAIL_IS_ERROR
   is true, this function raises an error if there are no devices of type D,
   otherwise it returns NULL in that case.  */

static struct gomp_device_descr *
resolve_device (acc_device_t d, bool fail_is_error)
{
  acc_device_t d_arg = d;

  switch (d)
    {
    case acc_device_default:
      {
	if (goacc_device_type)
	  {
	    /* Lookup the named device.  */
	    while (known_device_type_p (++d))
	      if (dispatchers[d]
		  && !strcasecmp (goacc_device_type,
				  get_openacc_name (dispatchers[d]->name))
		  && dispatchers[d]->get_num_devices_func (0) > 0)
		goto found;

	    if (fail_is_error)
	      {
		gomp_mutex_unlock (&acc_device_lock);
		gomp_fatal ("device type %s not supported", goacc_device_type);
	      }
	    else
	      return NULL;
	  }

	/* No default device specified, so start scanning for any non-host
	   device that is available.  */
	d = acc_device_not_host;
      }
      /* FALLTHROUGH */

    case acc_device_not_host:
      /* Find the first available device after acc_device_not_host.  */
      while (known_device_type_p (++d))
	if (dispatchers[d] && dispatchers[d]->get_num_devices_func (0) > 0)
	  goto found;
      if (d_arg == acc_device_default)
	{
	  d = acc_device_host;
	  goto found;
	}
      if (fail_is_error)
        {
	  gomp_mutex_unlock (&acc_device_lock);
	  gomp_fatal ("no device found");
	}
      else
        return NULL;
      break;

    case acc_device_host:
      break;

    default:
      if (!known_device_type_p (d))
	{
	  if (fail_is_error)
	    goto unsupported_device;
	  else
	    return NULL;
	}
      break;
    }
 found:

  assert (d != acc_device_none
	  && d != acc_device_default
	  && d != acc_device_not_host);

  if (dispatchers[d] == NULL && fail_is_error)
    {
    unsupported_device:
      gomp_mutex_unlock (&acc_device_lock);
      gomp_fatal ("device type %s not supported", name_of_acc_device_t (d));
    }

  return dispatchers[d];
}

/* Emit a suitable error if no device of a particular type is available, or
   the given device number is out-of-range.  */
static void
acc_dev_num_out_of_range (acc_device_t d, int ord, int ndevs)
{
  if (ndevs == 0)
    gomp_fatal ("no devices of type %s available", name_of_acc_device_t (d));
  else
    gomp_fatal ("device %u out of range", ord);
}

/* This is called when plugins have been initialized, and serves to call
   (indirectly) the target's device_init hook.  Calling multiple times without
   an intervening acc_shutdown_1 call is an error.  ACC_DEVICE_LOCK must be
   held before calling this function.  */

static struct gomp_device_descr *
acc_init_1 (acc_device_t d, acc_construct_t parent_construct, int implicit)
{
  gomp_mutex_lock (&acc_init_state_lock);
  acc_init_state = initializing;
  acc_init_thread = pthread_self ();
  gomp_mutex_unlock (&acc_init_state_lock);

  bool check_not_nested_p;
  if (implicit)
    {
      /* In the implicit case, there should (TODO: must?) already be something
	 have been set up for an outer construct.  */
      check_not_nested_p = false;
    }
  else
    {
      check_not_nested_p = true;
      /* TODO: should we set 'thr->prof_info' etc. in this case ('acc_init')?
	 The problem is, that we don't have 'thr' yet?  (So,
	 'check_not_nested_p = true' also is pointless actually.)  */
    }
  bool profiling_p = GOACC_PROFILING_DISPATCH_P (check_not_nested_p);

  acc_prof_info prof_info;
  if (profiling_p)
    {
      prof_info.event_type = acc_ev_device_init_start;
      prof_info.valid_bytes = _ACC_PROF_INFO_VALID_BYTES;
      prof_info.version = _ACC_PROF_INFO_VERSION;
      prof_info.device_type = d;
      prof_info.device_number = goacc_device_num;
      prof_info.thread_id = -1;
      prof_info.async = acc_async_sync;
      prof_info.async_queue = prof_info.async;
      prof_info.src_file = NULL;
      prof_info.func_name = NULL;
      prof_info.line_no = -1;
      prof_info.end_line_no = -1;
      prof_info.func_line_no = -1;
      prof_info.func_end_line_no = -1;
    }
  acc_event_info device_init_event_info;
  if (profiling_p)
    {
      device_init_event_info.other_event.event_type = prof_info.event_type;
      device_init_event_info.other_event.valid_bytes
	= _ACC_OTHER_EVENT_INFO_VALID_BYTES;
      device_init_event_info.other_event.parent_construct = parent_construct;
      device_init_event_info.other_event.implicit = implicit;
      device_init_event_info.other_event.tool_info = NULL;
    }
  acc_api_info api_info;
  if (profiling_p)
    {
      api_info.device_api = acc_device_api_none;
      api_info.valid_bytes = _ACC_API_INFO_VALID_BYTES;
      api_info.device_type = prof_info.device_type;
      api_info.vendor = -1;
      api_info.device_handle = NULL;
      api_info.context_handle = NULL;
      api_info.async_handle = NULL;
    }

  if (profiling_p)
    goacc_profiling_dispatch (&prof_info, &device_init_event_info, &api_info);

  struct gomp_device_descr *base_dev, *acc_dev;
  int ndevs;

  base_dev = resolve_device (d, true);

  ndevs = base_dev->get_num_devices_func (0);

  if (ndevs <= 0 || goacc_device_num >= ndevs)
    acc_dev_num_out_of_range (d, goacc_device_num, ndevs);

  acc_dev = &base_dev[goacc_device_num];

  gomp_mutex_lock (&acc_dev->lock);
  if (acc_dev->state == GOMP_DEVICE_INITIALIZED)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("device already active");
    }

  gomp_init_device (acc_dev);
  gomp_mutex_unlock (&acc_dev->lock);

  if (profiling_p)
    {
      prof_info.event_type = acc_ev_device_init_end;
      device_init_event_info.other_event.event_type = prof_info.event_type;
      goacc_profiling_dispatch (&prof_info, &device_init_event_info,
				&api_info);
    }

  /* We're setting 'initialized' *after* 'goacc_profiling_dispatch', so that a
     nested 'acc_get_device_type' called from a profiling callback still sees
     'initializing', so that we don't deadlock when it then again tries to lock
     'goacc_prof_lock'.  See also the discussion in 'acc_get_device_type'.  */
  gomp_mutex_lock (&acc_init_state_lock);
  acc_init_state = initialized;
  gomp_mutex_unlock (&acc_init_state_lock);

  return base_dev;
}

/* ACC_DEVICE_LOCK must be held before calling this function.  */

static void
acc_shutdown_1 (acc_device_t d)
{
  struct gomp_device_descr *base_dev;
  struct goacc_thread *walk;
  int ndevs, i;
  bool devices_active = false;

  /* Get the base device for this device type.  */
  base_dev = resolve_device (d, true);

  ndevs = base_dev->get_num_devices_func (0);

  /* Unload all the devices of this type that have been opened.  */
  for (i = 0; i < ndevs; i++)
    {
      struct gomp_device_descr *acc_dev = &base_dev[i];

      gomp_mutex_lock (&acc_dev->lock);
      gomp_unload_device (acc_dev);
      gomp_mutex_unlock (&acc_dev->lock);
    }
  
  gomp_mutex_lock (&goacc_thread_lock);

  /* Free target-specific TLS data and close all devices.  */
  for (walk = goacc_threads; walk != NULL; walk = walk->next)
    {
      if (walk->target_tls)
	base_dev->openacc.destroy_thread_data_func (walk->target_tls);

      walk->target_tls = NULL;

      /* This would mean the user is shutting down OpenACC in the middle of an
         "acc data" pragma.  Likely not intentional.  */
      if (walk->mapped_data)
	{
	  gomp_mutex_unlock (&goacc_thread_lock);
	  gomp_fatal ("shutdown in 'acc data' region");
	}

      /* Similarly, if this happens then user code has done something weird.  */
      if (walk->saved_bound_dev)
	{
	  gomp_mutex_unlock (&goacc_thread_lock);
	  gomp_fatal ("shutdown during host fallback");
	}

      if (walk->dev)
	{
	  gomp_mutex_lock (&walk->dev->lock);

	  while (walk->dev->mem_map.root)
	    {
	      splay_tree_key k = &walk->dev->mem_map.root->key;
	      if (k->aux)
		k->aux->link_key = NULL;
	      gomp_remove_var (walk->dev, k);
	    }

	  gomp_mutex_unlock (&walk->dev->lock);

	  walk->dev = NULL;
	  walk->base_dev = NULL;
	}
    }

  gomp_mutex_unlock (&goacc_thread_lock);

  /* Close all the devices of this type that have been opened.  */
  bool ret = true;
  for (i = 0; i < ndevs; i++)
    {
      struct gomp_device_descr *acc_dev = &base_dev[i];
      gomp_mutex_lock (&acc_dev->lock);
      if (acc_dev->state == GOMP_DEVICE_INITIALIZED)
        {
	  devices_active = true;
	  ret &= gomp_fini_device (acc_dev);
	  acc_dev->state = GOMP_DEVICE_UNINITIALIZED;
	}
      gomp_mutex_unlock (&acc_dev->lock);
    }

  if (!ret)
    gomp_fatal ("device finalization failed");

  if (!devices_active)
    gomp_fatal ("no device initialized");
}

static struct goacc_thread *
goacc_new_thread (void)
{
  struct goacc_thread *thr = gomp_malloc (sizeof (struct goacc_thread));

#if defined HAVE_TLS || defined USE_EMUTLS
  goacc_tls_data = thr;
#else
  pthread_setspecific (goacc_tls_key, thr);
#endif

  pthread_setspecific (goacc_cleanup_key, thr);

  gomp_mutex_lock (&goacc_thread_lock);
  thr->next = goacc_threads;
  goacc_threads = thr;
  gomp_mutex_unlock (&goacc_thread_lock);

  return thr;
}

static void
goacc_destroy_thread (void *data)
{
  struct goacc_thread *thr = data, *walk, *prev;

  gomp_mutex_lock (&goacc_thread_lock);

  if (thr)
    {
      struct gomp_device_descr *acc_dev = thr->dev;

      if (acc_dev && thr->target_tls)
	{
	  acc_dev->openacc.destroy_thread_data_func (thr->target_tls);
	  thr->target_tls = NULL;
	}

      assert (!thr->mapped_data);

      /* Remove from thread list.  */
      for (prev = NULL, walk = goacc_threads; walk;
	   prev = walk, walk = walk->next)
	if (walk == thr)
	  {
	    if (prev == NULL)
	      goacc_threads = walk->next;
	    else
	      prev->next = walk->next;

	    free (thr);

	    break;
	  }

      assert (walk);
    }

  gomp_mutex_unlock (&goacc_thread_lock);
}

/* Use the ORD'th device instance for the current host thread (or -1 for the
   current global default).  The device (and the runtime) must be initialised
   before calling this function.  */

void
goacc_attach_host_thread_to_device (int ord)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = NULL, *base_dev = NULL;
  int num_devices;
  
  if (thr && thr->dev && (thr->dev->target_id == ord || ord < 0))
    return;
  
  if (ord < 0)
    ord = goacc_device_num;
  
  /* Decide which type of device to use.  If the current thread has a device
     type already (e.g. set by acc_set_device_type), use that, else use the
     global default.  */
  if (thr && thr->base_dev)
    base_dev = thr->base_dev;
  else
    {
      assert (cached_base_dev);
      base_dev = cached_base_dev;
    }
  
  num_devices = base_dev->get_num_devices_func (0);
  if (num_devices <= 0 || ord >= num_devices)
    acc_dev_num_out_of_range (acc_device_type (base_dev->type), ord,
			      num_devices);
  
  if (!thr)
    thr = goacc_new_thread ();
  
  thr->base_dev = base_dev;
  thr->dev = acc_dev = &base_dev[ord];
  thr->saved_bound_dev = NULL;
  thr->mapped_data = NULL;
  thr->prof_info = NULL;
  thr->api_info = NULL;
  /* Initially, all callbacks for all events are enabled.  */
  thr->prof_callbacks_enabled = true;

  thr->target_tls
    = acc_dev->openacc.create_thread_data_func (ord);
}

/* OpenACC 2.0a (3.2.12, 3.2.13) doesn't specify whether the serialization of
   init/shutdown is per-process or per-thread.  We choose per-process.  */

void
acc_init (acc_device_t d)
{
  if (!known_device_type_p (d))
    unknown_device_type_error (d);

  gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);
  cached_base_dev = acc_init_1 (d, acc_construct_runtime_api, 0);
  gomp_mutex_unlock (&acc_device_lock);
  
  goacc_attach_host_thread_to_device (-1);
}

ialias (acc_init)

void
acc_shutdown (acc_device_t d)
{
  if (!known_device_type_p (d))
    unknown_device_type_error (d);

  gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);

  acc_shutdown_1 (d);

  gomp_mutex_unlock (&acc_device_lock);
}

ialias (acc_shutdown)

int
acc_get_num_devices (acc_device_t d)
{
  if (!known_device_type_p (d))
    unknown_device_type_error (d);

  int n = 0;
  struct gomp_device_descr *acc_dev;

  if (d == acc_device_none)
    return 0;

  gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);
  acc_dev = resolve_device (d, false);
  gomp_mutex_unlock (&acc_device_lock);

  if (!acc_dev)
    return 0;

  n = acc_dev->get_num_devices_func (0);
  if (n < 0)
    n = 0;

  return n;
}

ialias (acc_get_num_devices)

/* Set the device type for the current thread only (using the current global
   default device number), initialising that device if necessary.  Also set the
   default device type for new threads to D.  */

void
acc_set_device_type (acc_device_t d)
{
  if (!known_device_type_p (d))
    unknown_device_type_error (d);

  struct gomp_device_descr *base_dev, *acc_dev;
  struct goacc_thread *thr = goacc_thread ();

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    prof_info.device_type = d;

  gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);

  cached_base_dev = base_dev = resolve_device (d, true);
  acc_dev = &base_dev[goacc_device_num];

  gomp_mutex_lock (&acc_dev->lock);
  if (acc_dev->state == GOMP_DEVICE_UNINITIALIZED)
    gomp_init_device (acc_dev);
  gomp_mutex_unlock (&acc_dev->lock);

  gomp_mutex_unlock (&acc_device_lock);

  /* We're changing device type: invalidate the current thread's dev and
     base_dev pointers.  */
  if (thr && thr->base_dev != base_dev)
    {
      thr->base_dev = thr->dev = NULL;
      if (thr->mapped_data)
        gomp_fatal ("acc_set_device_type in 'acc data' region");
    }

  goacc_attach_host_thread_to_device (-1);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}

ialias (acc_set_device_type)

static bool
self_initializing_p (void)
{
  bool res;
  gomp_mutex_lock (&acc_init_state_lock);
  res = (acc_init_state == initializing
	 && pthread_equal (acc_init_thread, pthread_self ()));
  gomp_mutex_unlock (&acc_init_state_lock);
  return res;
}

acc_device_t
acc_get_device_type (void)
{
  acc_device_t res = acc_device_none;
  struct gomp_device_descr *dev;
  struct goacc_thread *thr = goacc_thread ();

  if (thr && thr->base_dev)
    res = acc_device_type (thr->base_dev->type);
  else if (self_initializing_p ())
    /* The Cuda libaccinj64.so version 9.0+ calls acc_get_device_type during the
       acc_ev_device_init_start event callback, which is dispatched during
       acc_init_1.  Trying to lock acc_device_lock during such a call (as we do
       in the else clause below), will result in deadlock, since the lock has
       already been taken by the acc_init_1 caller.  We work around this problem
       by using the acc_get_device_type property "If the device type has not yet
       been selected, the value acc_device_none may be returned".  */
    ;
  else
    {
      acc_prof_info prof_info;
      acc_api_info api_info;
      bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

      gomp_init_targets_once ();

      gomp_mutex_lock (&acc_device_lock);
      dev = resolve_device (acc_device_default, true);
      gomp_mutex_unlock (&acc_device_lock);
      res = acc_device_type (dev->type);

      if (profiling_p)
	{
	  thr->prof_info = NULL;
	  thr->api_info = NULL;
	}
    }

  assert (res != acc_device_default
	  && res != acc_device_not_host
	  && res != acc_device_current);

  return res;
}

ialias (acc_get_device_type)

int
acc_get_device_num (acc_device_t d)
{
  if (!known_device_type_p (d))
    unknown_device_type_error (d);

  const struct gomp_device_descr *dev;
  struct goacc_thread *thr = goacc_thread ();

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    prof_info.device_type = d;

  gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);
  dev = resolve_device (d, true);
  gomp_mutex_unlock (&acc_device_lock);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }

  if (thr && thr->base_dev == dev && thr->dev)
    return thr->dev->target_id;

  return goacc_device_num;
}

ialias (acc_get_device_num)

void
acc_set_device_num (int ord, acc_device_t d)
{
  if (!known_device_type_p (d))
    unknown_device_type_error (d);

  struct gomp_device_descr *base_dev, *acc_dev;
  int num_devices;

  gomp_init_targets_once ();

  if (ord < 0)
    ord = goacc_device_num;

  if ((int) d == 0)
    /* Set whatever device is being used by the current host thread to use
       device instance ORD.  It's unclear if this is supposed to affect other
       host threads too (OpenACC 2.0 (3.2.4) acc_set_device_num).  */
    goacc_attach_host_thread_to_device (ord);
  else
    {
      gomp_mutex_lock (&acc_device_lock);

      cached_base_dev = base_dev = resolve_device (d, true);

      num_devices = base_dev->get_num_devices_func (0);

      if (num_devices <= 0 || ord >= num_devices)
        acc_dev_num_out_of_range (d, ord, num_devices);

      acc_dev = &base_dev[ord];

      gomp_mutex_lock (&acc_dev->lock);
      if (acc_dev->state == GOMP_DEVICE_UNINITIALIZED)
        gomp_init_device (acc_dev);
      gomp_mutex_unlock (&acc_dev->lock);

      gomp_mutex_unlock (&acc_device_lock);

      goacc_attach_host_thread_to_device (ord);
    }
  
  goacc_device_num = ord;
}

ialias (acc_set_device_num)

static union goacc_property_value
get_property_any (int ord, acc_device_t d, acc_device_property_t prop)
{
  goacc_lazy_initialize ();
  struct goacc_thread *thr = goacc_thread ();

  if (d == acc_device_current && thr && thr->dev)
    return thr->dev->openacc.get_property_func (thr->dev->target_id, prop);

  gomp_mutex_lock (&acc_device_lock);

  struct gomp_device_descr *dev = resolve_device (d, true);

  int num_devices = dev->get_num_devices_func (0);

  if (num_devices <= 0 || ord >= num_devices)
    acc_dev_num_out_of_range (d, ord, num_devices);

  dev += ord;

  gomp_mutex_lock (&dev->lock);
  if (dev->state == GOMP_DEVICE_UNINITIALIZED)
    gomp_init_device (dev);
  gomp_mutex_unlock (&dev->lock);

  gomp_mutex_unlock (&acc_device_lock);

  assert (dev);

  return dev->openacc.get_property_func (dev->target_id, prop);
}

size_t
acc_get_property (int ord, acc_device_t d, acc_device_property_t prop)
{
  if (!known_device_type_p (d))
    unknown_device_type_error(d);

  if (prop & GOACC_PROPERTY_STRING_MASK)
    return 0;
  else
    return get_property_any (ord, d, prop).val;
}

ialias (acc_get_property)

const char *
acc_get_property_string (int ord, acc_device_t d, acc_device_property_t prop)
{
  if (!known_device_type_p (d))
    unknown_device_type_error(d);

  if (prop & GOACC_PROPERTY_STRING_MASK)
    return get_property_any (ord, d, prop).ptr;
  else
    return NULL;
}

ialias (acc_get_property_string)

/* For -O and higher, the compiler always attempts to expand acc_on_device, but
   if the user disables the builtin, or calls it via a pointer, we'll need this
   version.

   Compile this with optimization, so that the compiler expands
   this, rather than generating infinitely recursive code.

   The function just forwards its argument to __builtin_acc_on_device.  It does
   not verify that the argument is a valid acc_device_t enumeration value.  */

int __attribute__ ((__optimize__ ("O2")))
acc_on_device (acc_device_t dev)
{
  return __builtin_acc_on_device (dev);
}

ialias (acc_on_device)

attribute_hidden void
goacc_runtime_initialize (void)
{
  gomp_mutex_init (&acc_device_lock);

#if !(defined HAVE_TLS || defined USE_EMUTLS)
  pthread_key_create (&goacc_tls_key, NULL);
#endif

  pthread_key_create (&goacc_cleanup_key, goacc_destroy_thread);

  cached_base_dev = NULL;

  goacc_threads = NULL;
  gomp_mutex_init (&goacc_thread_lock);

  /* Initialize and register the 'host' device type.  */
  goacc_host_init ();
}

static void __attribute__((destructor))
goacc_runtime_deinitialize (void)
{
#if !(defined HAVE_TLS || defined USE_EMUTLS)
  pthread_key_delete (goacc_tls_key);
#endif
  pthread_key_delete (goacc_cleanup_key);
}

/* Compiler helper functions */

attribute_hidden void
goacc_save_and_set_bind (acc_device_t d)
{
  struct goacc_thread *thr = goacc_thread ();

  assert (!thr->saved_bound_dev);

  thr->saved_bound_dev = thr->dev;
  thr->dev = dispatchers[d];
}

attribute_hidden void
goacc_restore_bind (void)
{
  struct goacc_thread *thr = goacc_thread ();

  thr->dev = thr->saved_bound_dev;
  thr->saved_bound_dev = NULL;
}

/* This is called from any OpenACC support function that may need to implicitly
   initialize the libgomp runtime, either globally or from a new host thread. 
   On exit "goacc_thread" will return a valid & populated thread block.  */

attribute_hidden void
goacc_lazy_initialize (void)
{
  struct goacc_thread *thr = goacc_thread ();

  if (thr && thr->dev)
    return;

  gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);
  if (!cached_base_dev)
    cached_base_dev = acc_init_1 (acc_device_default,
				  acc_construct_parallel, 1);
  gomp_mutex_unlock (&acc_device_lock);

  goacc_attach_host_thread_to_device (-1);
}
