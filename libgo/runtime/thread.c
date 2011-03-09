// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <errno.h>
#include "runtime.h"
#include "go-assert.h"

void
runtime_initlock(Lock *l)
{
	l->key = 0;
	if(sem_init(&l->sem, 0, 0) != 0)
		runtime_throw("sem_init failed");
}

static uint32
runtime_xadd(uint32 volatile *val, int32 delta)
{
	uint32 oval, nval;

	for(;;){
		oval = *val;
		nval = oval + delta;
		if(runtime_cas(val, oval, nval))
			return nval;
	}
}

// noinline so that runtime_lock doesn't have to split the stack.
static void runtime_lock_full(Lock *l) __attribute__ ((noinline));

static void
runtime_lock_full(Lock *l)
{
	for(;;){
		if(sem_wait(&l->sem) == 0)
			return;
		if(errno != EINTR)
			runtime_throw("sem_wait failed");
	}
}

void
runtime_lock(Lock *l)
{
	if(m != nil) {
		if(m->locks < 0)
			runtime_throw("lock count");
		m->locks++;
	}

	if(runtime_xadd(&l->key, 1) > 1)	// someone else has it; wait
		runtime_lock_full(l);
}

static void runtime_unlock_full(Lock *l) __attribute__ ((noinline));

static void
runtime_unlock_full(Lock *l)
{
	if(sem_post(&l->sem) != 0)
		runtime_throw("sem_post failed");
}

void
runtime_unlock(Lock *l)
{
	if(m != nil) {
		m->locks--;
		if(m->locks < 0)
			runtime_throw("lock count");
	}

	if(runtime_xadd(&l->key, -1) > 0)	// someone else is waiting
		runtime_unlock_full(l);
}

void
runtime_destroylock(Lock *l)
{
	sem_destroy(&l->sem);
}

#ifndef HAVE_SYNC_BOOL_COMPARE_AND_SWAP_4

// For targets which don't have the required sync support.  Really
// this should be provided by gcc itself.  FIXME.

static pthread_mutex_t sync_lock = PTHREAD_MUTEX_INITIALIZER;

_Bool
__sync_bool_compare_and_swap_4(uint32*, uint32, uint32)
  __attribute__((visibility("hidden")));

_Bool
__sync_bool_compare_and_swap_4(uint32* ptr, uint32 old, uint32 new)
{
  int i;
  _Bool ret;

  i = pthread_mutex_lock(&sync_lock);
  __go_assert(i == 0);

  if(*ptr != old) {
    ret = 0;
  } else {
    *ptr = new;
    ret = 1;
  }

  i = pthread_mutex_unlock(&sync_lock);
  __go_assert(i == 0);

  return ret;
}

#endif
