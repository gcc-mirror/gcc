/* go-cgo.c -- SWIG support routines for libgo.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"

/* Used for _cgo_wait_runtime_init_done.  This is based on code in
   runtime/cgo/gcc_libinit.c in the master library.  */

static pthread_cond_t runtime_init_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t runtime_init_mu = PTHREAD_MUTEX_INITIALIZER;
static _Bool runtime_init_done;

/* This is called by exported cgo functions to ensure that the runtime
   has been initialized before we enter the function.  This is needed
   when building with -buildmode=c-archive or similar.  */

void
_cgo_wait_runtime_init_done (void)
{
  int err;

  if (__atomic_load_n (&runtime_init_done, __ATOMIC_ACQUIRE))
    return;

  err = pthread_mutex_lock (&runtime_init_mu);
  if (err != 0)
    abort ();
  while (!__atomic_load_n (&runtime_init_done, __ATOMIC_ACQUIRE))
    {
      err = pthread_cond_wait (&runtime_init_cond, &runtime_init_mu);
      if (err != 0)
	abort ();
    }
  err = pthread_mutex_unlock (&runtime_init_mu);
  if (err != 0)
    abort ();
}

/* This is called by runtime_main after the Go runtime is
   initialized.  */

void
_cgo_notify_runtime_init_done (void)
{
  int err;

  err = pthread_mutex_lock (&runtime_init_mu);
  if (err != 0)
    abort ();
  __atomic_store_n (&runtime_init_done, 1, __ATOMIC_RELEASE);
  err = pthread_cond_broadcast (&runtime_init_cond);
  if (err != 0)
    abort ();
  err = pthread_mutex_unlock (&runtime_init_mu);
  if (err != 0)
    abort ();
}

// runtime_iscgo is set to true if some cgo code is linked in.
// This is done by a constructor in the cgo generated code.
_Bool runtime_iscgo;
