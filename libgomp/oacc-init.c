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
#include "plugin/plugin-host.h"
#include <assert.h>
#include <stdlib.h>
#include <strings.h>
#include <stdbool.h>
#include <string.h>

static gomp_mutex_t acc_device_lock;

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

/* OpenACC names some things a little differently.  */

static const char *
get_openacc_name (const char *name)
{
  if (strcmp (name, "nvptx") == 0)
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
    case acc_device_host_nonshm: return "host_nonshm";
    case acc_device_not_host: return "not_host";
    case acc_device_nvidia: return "nvidia";
    default: gomp_fatal ("unknown device type %u", (unsigned) type);
    }
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
  struct gomp_device_descr *base_dev, *acc_dev;
  int ndevs;

  base_dev = resolve_device (d);

  ndevs = base_dev->get_num_devices_func ();

  if (!base_dev || ndevs <= 0 || goacc_device_num >= ndevs)
    gomp_fatal ("device %s not supported", name_of_acc_device_t (d));

  acc_dev = &base_dev[goacc_device_num];

  if (acc_dev->is_initialized)
    gomp_fatal ("device already active");

  gomp_init_device (acc_dev);

  return base_dev;
}

static void
acc_shutdown_1 (acc_device_t d)
{
  struct gomp_device_descr *base_dev;
  struct goacc_thread *walk;
  int ndevs, i;
  bool devices_active = false;

  /* Get the base device for this device type.  */
  base_dev = resolve_device (d);

  if (!base_dev)
    gomp_fatal ("device %s not supported", name_of_acc_device_t (d));

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

      /* Similarly, if this happens then user code has done something weird.  */
      if (walk->saved_bound_dev)
        gomp_fatal ("shutdown during host fallback");

      if (walk->dev)
	{
	  gomp_mutex_lock (&walk->dev->lock);
	  gomp_free_memmap (&walk->dev->mem_map);
	  gomp_mutex_unlock (&walk->dev->lock);

	  walk->dev = NULL;
	  walk->base_dev = NULL;
	}
    }

  gomp_mutex_unlock (&goacc_thread_lock);

  ndevs = base_dev->get_num_devices_func ();

  /* Close all the devices of this type that have been opened.  */
  for (i = 0; i < ndevs; i++)
    {
      struct gomp_device_descr *acc_dev = &base_dev[i];
      if (acc_dev->is_initialized)
        {
	  devices_active = true;
	  gomp_fini_device (acc_dev);
	}
    }

  if (!devices_active)
    gomp_fatal ("no device initialized");
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
  
  num_devices = base_dev->get_num_devices_func ();
  if (num_devices <= 0 || ord >= num_devices)
    gomp_fatal ("device %u out of range", ord);
  
  if (!thr)
    thr = goacc_new_thread ();
  
  thr->base_dev = base_dev;
  thr->dev = acc_dev = &base_dev[ord];
  thr->saved_bound_dev = NULL;
  thr->mapped_data = NULL;
  
  thr->target_tls
    = acc_dev->openacc.create_thread_data_func (ord);
  
  acc_dev->openacc.async_set_async_func (acc_async_sync);
}

/* OpenACC 2.0a (3.2.12, 3.2.13) doesn't specify whether the serialization of
   init/shutdown is per-process or per-thread.  We choose per-process.  */

void
acc_init (acc_device_t d)
{
  if (!cached_base_dev)
    gomp_init_targets_once ();

  gomp_mutex_lock (&acc_device_lock);

  cached_base_dev = acc_init_1 (d);

  gomp_mutex_unlock (&acc_device_lock);
  
  goacc_attach_host_thread_to_device (-1);
}

ialias (acc_init)

void
acc_shutdown (acc_device_t d)
{
  gomp_mutex_lock (&acc_device_lock);

  acc_shutdown_1 (d);

  gomp_mutex_unlock (&acc_device_lock);
}

ialias (acc_shutdown)

int
acc_get_num_devices (acc_device_t d)
{
  int n = 0;
  struct gomp_device_descr *acc_dev;

  if (d == acc_device_none)
    return 0;

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

/* Set the device type for the current thread only (using the current global
   default device number), initialising that device if necessary.  Also set the
   default device type for new threads to D.  */

void
acc_set_device_type (acc_device_t d)
{
  struct gomp_device_descr *base_dev, *acc_dev;
  struct goacc_thread *thr = goacc_thread ();

  gomp_mutex_lock (&acc_device_lock);

  if (!cached_base_dev)
    gomp_init_targets_once ();

  cached_base_dev = base_dev = resolve_device (d);
  acc_dev = &base_dev[goacc_device_num];

  if (!acc_dev->is_initialized)
    gomp_init_device (acc_dev);

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
}

ialias (acc_set_device_type)

acc_device_t
acc_get_device_type (void)
{
  acc_device_t res = acc_device_none;
  struct gomp_device_descr *dev;
  struct goacc_thread *thr = goacc_thread ();

  if (thr && thr->base_dev)
    res = acc_device_type (thr->base_dev->type);
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
  struct goacc_thread *thr = goacc_thread ();

  if (d >= _ACC_device_hwm)
    gomp_fatal ("device %u out of range", (unsigned)d);

  if (!cached_base_dev)
    gomp_init_targets_once ();

  dev = resolve_device (d);
  if (!dev)
    gomp_fatal ("device %s not supported", name_of_acc_device_t (d));

  if (thr && thr->base_dev == dev && thr->dev)
    return thr->dev->target_id;

  return goacc_device_num;
}

ialias (acc_get_device_num)

void
acc_set_device_num (int ord, acc_device_t d)
{
  struct gomp_device_descr *base_dev, *acc_dev;
  int num_devices;

  if (!cached_base_dev)
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

      cached_base_dev = base_dev = resolve_device (d);

      num_devices = base_dev->get_num_devices_func ();

      if (ord >= num_devices)
        gomp_fatal ("device %u out of range", ord);

      acc_dev = &base_dev[ord];

      if (!acc_dev->is_initialized)
        gomp_init_device (acc_dev);

      gomp_mutex_unlock (&acc_device_lock);

      goacc_attach_host_thread_to_device (ord);
    }
  
  goacc_device_num = ord;
}

ialias (acc_set_device_num)

int
acc_on_device (acc_device_t dev)
{
  struct goacc_thread *thr = goacc_thread ();

  /* We only want to appear to be the "host_nonshm" plugin from "offloaded"
     code -- i.e. within a parallel region.  Test a flag set by the
     openacc_parallel hook of the host_nonshm plugin to determine that.  */
  if (acc_get_device_type () == acc_device_host_nonshm
      && thr && thr->target_tls
      && ((struct nonshm_thread *)thr->target_tls)->nonshm_exec)
    return dev == acc_device_host_nonshm || dev == acc_device_not_host;

  /* For OpenACC, libgomp is only built for the host, so this is sufficient.  */
  return dev == acc_device_host || dev == acc_device_none;
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

  if (!cached_base_dev)
    acc_init (acc_device_default);
  else
    goacc_attach_host_thread_to_device (-1);
}
