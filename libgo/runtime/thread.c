// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"

void
runtime_initlock(Lock *l)
{
	if(pthread_mutex_init(&l->mutex, NULL) != 0)
		runtime_throw("pthread_mutex_init failed");
}

void
runtime_lock(Lock *l)
{
	if(pthread_mutex_lock(&l->mutex) != 0)
		runtime_throw("lock failed");
}

void
runtime_unlock(Lock *l)
{
	if(pthread_mutex_unlock(&l->mutex) != 0)
		runtime_throw("unlock failed");
}

void
runtime_destroylock(Lock *l)
{
	pthread_mutex_destroy(&l->mutex);
}

bool
runtime_trylock(Lock *l)
{
	return pthread_mutex_trylock(&l->mutex) == 0;
}
