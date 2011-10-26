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

_Bool CompareAndSwapPointer (void **, void *, void *)
  asm ("libgo_sync.atomic.CompareAndSwapPointer");

_Bool
CompareAndSwapPointer (void **val, void *old, void *new)
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

int32_t LoadInt32 (int32_t *addr)
  asm ("libgo_sync.atomic.LoadInt32");

int32_t
LoadInt32 (int32_t *addr)
{
  int32_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, v))
    v = *addr;
  return v;
}

int64_t LoadInt64 (int64_t *addr)
  asm ("libgo_sync.atomic.LoadInt64");

int64_t
LoadInt64 (int64_t *addr)
{
  int64_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, v))
    v = *addr;
  return v;
}

uint32_t LoadUint32 (uint32_t *addr)
  asm ("libgo_sync.atomic.LoadUint32");

uint32_t
LoadUint32 (uint32_t *addr)
{
  uint32_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, v))
    v = *addr;
  return v;
}

uint64_t LoadUint64 (uint64_t *addr)
  asm ("libgo_sync.atomic.LoadUint64");

uint64_t
LoadUint64 (uint64_t *addr)
{
  uint64_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, v))
    v = *addr;
  return v;
}

uintptr_t LoadUintptr (uintptr_t *addr)
  asm ("libgo_sync.atomic.LoadUintptr");

uintptr_t
LoadUintptr (uintptr_t *addr)
{
  uintptr_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, v))
    v = *addr;
  return v;
}

void *LoadPointer (void **addr)
  asm ("libgo_sync.atomic.LoadPointer");

void *
LoadPointer (void **addr)
{
  void *v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, v))
    v = *addr;
  return v;
}

void StoreInt32 (int32_t *addr, int32_t val)
  asm ("libgo_sync.atomic.StoreInt32");

void
StoreInt32 (int32_t *addr, int32_t val)
{
  int32_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, val))
    v = *addr;
}

void StoreInt64 (int64_t *addr, int64_t val)
  asm ("libgo_sync.atomic.StoreInt64");

void
StoreInt64 (int64_t *addr, int64_t val)
{
  int64_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, val))
    v = *addr;
}

void StoreUint32 (uint32_t *addr, uint32_t val)
  asm ("libgo_sync.atomic.StoreUint32");

void
StoreUint32 (uint32_t *addr, uint32_t val)
{
  uint32_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, val))
    v = *addr;
}

void StoreUint64 (uint64_t *addr, uint64_t val)
  asm ("libgo_sync.atomic.StoreUint64");

void
StoreUint64 (uint64_t *addr, uint64_t val)
{
  uint64_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, val))
    v = *addr;
}

void StoreUintptr (uintptr_t *addr, uintptr_t val)
  asm ("libgo_sync.atomic.StoreUintptr");

void
StoreUintptr (uintptr_t *addr, uintptr_t val)
{
  uintptr_t v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, val))
    v = *addr;
}

void StorePointer (void **addr, void *val)
  asm ("libgo_sync.atomic.StorePointer");

void
StorePointer (void **addr, void *val)
{
  void *v;

  v = *addr;
  while (! __sync_bool_compare_and_swap (addr, v, val))
    v = *addr;
}
