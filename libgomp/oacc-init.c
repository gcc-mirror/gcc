/* OpenACC Runtime initialization routines

   Copyright (C) 2013-2015 Free Software Foundation, Inc.

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

static gomp_mutex_t acc_device_lock;

/* The dispatch table for the current accelerator device.  This is global, so
   you can only have one type of device open at any given time in a program.
   This is the "base" device in that several devices that use the same
   dispatch table may be active concurrently: this one (the "zeroth") is used
   for overall initialisation/shutdown, and other instances -- not necessarily
   including this one -- may be opened and closed once the base device has
   been initialized.  */
struct gomp_device_descr *base_dev;

#if defined HAVE_TLS || defined USE_EMUTLS
__thread struct goacc_thread *goacc_tls_data;
#else
pthread_key_t goacc_tls_key;
#endif
static pthread_key_t goacc_cleanup_key;

/* Current dispatcher, and how it was initialized */
static acc_device_t init_key = _ACC_device_hwm;

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

/* OpenACC names some things a little differently.  */

static const char *
get_openacc_name (const char *name)
{
  if (strcmp (name, "nvptx") == 0)
    return "nvidia";
  else
    return name;
}

static struct gomp_device_descr *
resolve_device (acc_device_t d)
{
  acc_device_t d_arg = d;

  switch (d)
    {
    case acc_device_default:
      {
	if (goacc_device_type)
	  {
	    /* Lookup the named device.  */
	    while (++d != _ACC_device_hwm)
	      if (dispatchers[d]
		  && !strcasecmp (goacc_device_type,
				  get_openacc_name (dispatchers[d]->name))
		  && dispatchers[d]->get_num_devices_func () > 0)
		goto found;

	    gomp_fatal ("device type %s not supported", goacc_device_type);
	  }

	/* No default device specified, so start scanning for any non-host
	   device that is available.  */
	d = acc_device_not_host;
      }
      /* FALLTHROUGH */

    case acc_device_not_host:
      /* Find the first available device after acc_device_not_host.  */
      while (++d != _ACC_device_hwm)
	if (dispatchers[d] && dispatchers[d]->get_num_devices_func () > 0)
	  goto found;
      if (d_arg == acc_device_default)
	{
	  d = acc_device_host;
	  goto found;
	}
      gomp_fatal ("no device found");
      break;

    case acc_device_host:
      break;

    default:
      if (d > _ACC_device_hwm)
	gomp_fatal ("device %u out of range", (unsigned)d);
      break;
    }
 found:

  assert (d != acc_device_none
	  && d != acc_device_default
	  && d != acc_device_not_host);

  return dispatchers[d];
}

/* This is called when plugins have been initialized, and serves to call
   (indirectly) the target's device_init hook.  Calling multiple times without
   an intervening acc_shutdown_1 call is an error.  */

static struct gomp_device_descr *
acc_init_1 (acc_device_t d)
{
  struct gomp_device_descr *acc_dev;

  acc_dev = resolve_device (d);

  if (!acc_dev || acc_dev->get_num_devices_func () <= 0)
    gomp_fatal ("device %u not supported", (unsigned)d);

  if (acc_dev->is_initialized)
    gomp_fatal ("device already active");

  /* We need to remember what we were intialized as, to check shutdown etc.  */
  init_key = d;

  gomp_init_device (acc_dev);

  return acc_dev;
}

static struct goacc_thread *
goacc_new_thread (void)
{
  struct goacc_thread *thr = gomp_malloc (sizeof (struct gomp_thread));

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
      if (base_dev && thr->target_tls)
	{
	  base_dev->openacc.destroy_thread_data_func (thr->target_tls);
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

/* Open the ORD'th device of the currently-active type (base_dev must be
   initialised before calling).  If ORD is < 0, open the default-numbered
   device (set by the ACC_DEVICE_NUM environment variable or a call to
   acc_set_device_num), or leave any currently-opened device as is.  "Opening"
   consists of calling the device's open_device_func hook, and setting up
   thread-local data (maybe allocating, then initializing with information
   pertaining to the newly-opened or previously-opened device).  */

static void
lazy_open (int ord)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev;

  if (thr && thr->dev)
    {
      assert (ord < 0 || ord == thr->dev->target_id);
      return;
    }

  assert (base_dev);

  if (ord < 0)
    ord = goacc_device_num;

  /* The OpenACC 2.0 spec leaves the runtime's behaviour when an out-of-range
     device is requested as implementation-defined (4.2 ACC_DEVICE_NUM).
     We choose to raise an error in such a case.  */
  if (ord >= base_dev->get_num_devices_func ())
    gomp_fatal ("device %u does not exist", ord);

  if (!thr)
    thr = goacc_new_thread ();

  acc_dev = thr->dev = &base_dev[ord];

  assert (acc_dev->target_id == ord);

  thr->saved_bound_dev = NULL;
  thr->mapped_data = NULL;

  if (!acc_dev->openacc.target_data)
    acc_dev->openacc.target_data = acc_dev->openacc.open_device_func (ord);

  thr->target_tls
    = acc_dev->openacc.create_thread_data_func (acc_dev->openacc.target_data);

  acc_dev->openacc.async_set_async_func (acc_async_sync);

  struct gomp_memory_mapping *mem_map = &acc_dev->mem_map;
  gomp_mutex_lock (&mem_map->lock);
  if (!mem_map->is_initialized)
    gomp_init_tables (acc_dev, mem_map);
  gomp_mutex_unlock (&mem_map->lock);
}

/* OpenACC 2.0a (3.2.12, 3.2.13) doesn't specify whether the serialization of
   init/shutdown is per-process or per-thread.  We choose per-process.  */

void
acc_init (acc_device_t d)
{
  if (!base_dev)
    gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);

  base_dev = acc_init_1 (d);

  lazy_open (-1);

  gomp_mutex_unlock (&acc_device_lock);
}

ialias (acc_init)

static void
acc_shutdown_1 (acc_device_t d)
{
  struct goacc_thread *walk;

  /* We don't check whether d matches the actual device found, because
     OpenACC 2.0 (3.2.12) says the parameters to the init and this
     call must match (for the shutdown call anyway, it's silent on
     others).  */

  if (!base_dev)
    gomp_fatal ("no device initialized");
  if (d != init_key)
    gomp_fatal ("device %u(%u) is initialized",
		(unsigned) init_key, (unsigned) base_dev->type);

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
	gomp_fatal ("shutdown in 'acc data' region");

      if (walk->dev)
	{
	  void *target_data = walk->dev->openacc.target_data;
	  if (walk->dev->openacc.close_device_func (target_data) < 0)
	    gomp_fatal ("failed to close device");

	  walk->dev->openacc.target_data = target_data = NULL;

	  struct gomp_memory_mapping *mem_map = &walk->dev->mem_map;
	  gomp_mutex_lock (&mem_map->lock);
	  gomp_free_memmap (mem_map);
	  gomp_mutex_unlock (&mem_map->lock);

	  walk->dev = NULL;
	}
    }

  gomp_mutex_unlock (&goacc_thread_lock);

  gomp_fini_device (base_dev);

  base_dev = NULL;
}

void
acc_shutdown (acc_device_t d)
{
  gomp_mutex_lock (&acc_device_lock);

  acc_shutdown_1 (d);

  gomp_mutex_unlock (&acc_device_lock);
}

ialias (acc_shutdown)

/* This function is called after plugins have been initialized.  It deals with
   the "base" device, and is used to prepare the runtime for dealing with a
   number of such devices (as implemented by some particular plugin).  If the
   argument device type D matches a previous call to the function, return the
   current base device, else shut the old device down and re-initialize with
   the new device type.  */

static struct gomp_device_descr *
lazy_init (acc_device_t d)
{
  if (base_dev)
    {
      /* Re-initializing the same device, do nothing.  */
      if (d == init_key)
	return base_dev;

      acc_shutdown_1 (init_key);
    }

  assert (!base_dev);

  return acc_init_1 (d);
}

/* Ensure that plugins are loaded, initialize and open the (default-numbered)
   device.  */

static void
lazy_init_and_open (acc_device_t d)
{
  if (!base_dev)
    gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);

  base_dev = lazy_init (d);

  lazy_open (-1);

  gomp_mutex_unlock (&acc_device_lock);
}

int
acc_get_num_devices (acc_device_t d)
{
  int n = 0;
  const struct gomp_device_descr *acc_dev;

  if (d == acc_device_none)
    return 0;

  if (!base_dev)
    gomp_init_targets_once ();

  acc_dev = resolve_device (d);
  if (!acc_dev)
    return 0;

  n = acc_dev->get_num_devices_func ();
  if (n < 0)
    n = 0;

  return n;
}

ialias (acc_get_num_devices)

void
acc_set_device_type (acc_device_t d)
{
  lazy_init_and_open (d);
}

ialias (acc_set_device_type)

acc_device_t
acc_get_device_type (void)
{
  acc_device_t res = acc_device_none;
  const struct gomp_device_descr *dev;

  if (base_dev)
    res = acc_device_type (base_dev->type);
  else
    {
      gomp_init_targets_once ();

      dev = resolve_device (acc_device_default);
      res = acc_device_type (dev->type);
    }

  assert (res != acc_device_default
	  && res != acc_device_not_host);

  return res;
}

ialias (acc_get_device_type)

int
acc_get_device_num (acc_device_t d)
{
  const struct gomp_device_descr *dev;
  int num;

  if (d >= _ACC_device_hwm)
    gomp_fatal ("device %u out of range", (unsigned)d);

  if (!base_dev)
    gomp_init_targets_once ();

  dev = resolve_device (d);
  if (!dev)
    gomp_fatal ("no devices of type %u", d);

  /* We might not have called lazy_open for this host thread yet, in which case
     the get_device_num_func hook will return -1.  */
  num = dev->openacc.get_device_num_func ();
  if (num < 0)
    num = goacc_device_num;

  return num;
}

ialias (acc_get_device_num)

void
acc_set_device_num (int n, acc_device_t d)
{
  const struct gomp_device_descr *dev;
  int num_devices;

  if (!base_dev)
    gomp_init_targets_once ();

  if ((int) d == 0)
    {
      int i;

      /* A device setting of zero sets all device types on the system to use
         the Nth instance of that device type.  Only attempt it for initialized
	 devices though.  */
      for (i = acc_device_not_host + 1; i < _ACC_device_hwm; i++)
        {
	  dev = resolve_device (d);
	  if (dev && dev->is_initialized)
	    dev->openacc.set_device_num_func (n);
	}

      /* ...and for future calls to acc_init/acc_set_device_type, etc.  */
      goacc_device_num = n;
    }
  else
    {
      struct goacc_thread *thr = goacc_thread ();

      gomp_mutex_lock (&acc_device_lock);

      base_dev = lazy_init (d);

      num_devices = base_dev->get_num_devices_func ();

      if (n >= num_devices)
        gomp_fatal ("device %u out of range", n);

      /* If we're changing the device number, de-associate this thread with
	 the device (but don't close the device, since it may be in use by
	 other threads).  */
      if (thr && thr->dev && n != thr->dev->target_id)
	thr->dev = NULL;

      lazy_open (n);

      gomp_mutex_unlock (&acc_device_lock);
    }
}

ialias (acc_set_device_num)

int
acc_on_device (acc_device_t dev)
{
  struct goacc_thread *thr = goacc_thread ();

  if (thr && thr->dev
      && acc_device_type (thr->dev->type) == acc_device_host_nonshm)
    return dev == acc_device_host_nonshm || dev == acc_device_not_host;

  /* Just rely on the compiler builtin.  */
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

  base_dev = NULL;

  goacc_threads = NULL;
  gomp_mutex_init (&goacc_thread_lock);
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
   initialize the libgomp runtime.  On exit all such initialization will have
   been done, and both the global ACC_dev and the per-host-thread ACC_memmap
   pointers will be valid.  */

attribute_hidden void
goacc_lazy_initialize (void)
{
  struct goacc_thread *thr = goacc_thread ();

  if (thr && thr->dev)
    return;

  if (!base_dev)
    lazy_init_and_open (acc_device_default);
  else
    {
      gomp_mutex_lock (&acc_device_lock);
      lazy_open (-1);
      gomp_mutex_unlock (&acc_device_lock);
    }
}
