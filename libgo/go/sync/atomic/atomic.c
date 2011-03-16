/* atomic.c -- implement atomic routines for Go.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

_Bool CompareAndSwapInt32 (int32_t *, int32_t, int32_t)
  asm ("libgo_sync.atomic.CompareAndSwapInt32");

_Bool
CompareAndSwapInt32 (int32_t *val, int32_t old, int32_t new)
{
  return __sync_bool_compare_and_swap (val, old, new);
}

_Bool CompareAndSwapInt64 (int64_t *, int64_t, int64_t)
  asm ("libgo_sync.atomic.CompareAndSwapInt64");

_Bool
CompareAndSwapInt64 (int64_t *val, int64_t old, int64_t new)
{
  return __sync_bool_compare_and_swap (val, old, new);
}

_Bool CompareAndSwapUint32 (uint32_t *, uint32_t, uint32_t)
  asm ("libgo_sync.atomic.CompareAndSwapUint32");

_Bool
CompareAndSwapUint32 (uint32_t *val, uint32_t old, uint32_t new)
{
  return __sync_bool_compare_and_swap (val, old, new);
}

_Bool CompareAndSwapUint64 (uint64_t *, uint64_t, uint64_t)
  asm ("libgo_sync.atomic.CompareAndSwapUint64");

_Bool
CompareAndSwapUint64 (uint64_t *val, uint64_t old, uint64_t new)
{
  return __sync_bool_compare_and_swap (val, old, new);
}

_Bool CompareAndSwapUintptr (uintptr_t *, uintptr_t, uintptr_t)
  asm ("libgo_sync.atomic.CompareAndSwapUintptr");

_Bool
CompareAndSwapUintptr (uintptr_t *val, uintptr_t old, uintptr_t new)
{
  return __sync_bool_compare_and_swap (val, old, new);
}

int32_t AddInt32 (int32_t *, int32_t)
  asm ("libgo_sync.atomic.AddInt32");

int32_t
AddInt32 (int32_t *val, int32_t delta)
{
  return __sync_add_and_fetch (val, delta);
}

uint32_t AddUint32 (uint32_t *, uint32_t)
  asm ("libgo_sync.atomic.AddUint32");

uint32_t
AddUint32 (uint32_t *val, uint32_t delta)
{
  return __sync_add_and_fetch (val, delta);
}

int64_t AddInt64 (int64_t *, int64_t)
  asm ("libgo_sync.atomic.AddInt64");

int64_t
AddInt64 (int64_t *val, int64_t delta)
{
  return __sync_add_and_fetch (val, delta);
}

uint64_t AddUint64 (uint64_t *, uint64_t)
  asm ("libgo_sync.atomic.AddUint64");

uint64_t
AddUint64 (uint64_t *val, uint64_t delta)
{
  return __sync_add_and_fetch (val, delta);
}

uintptr_t AddUintptr (uintptr_t *, uintptr_t)
  asm ("libgo_sync.atomic.AddUintptr");

uintptr_t
AddUintptr (uintptr_t *val, uintptr_t delta)
{
  return __sync_add_and_fetch (val, delta);
}
