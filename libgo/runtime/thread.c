// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"

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
	if(sem_wait(&l->sem) != 0)
		runtime_throw("sem_wait failed");
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
