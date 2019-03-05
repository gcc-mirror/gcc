/* atomic.c -- implement atomic routines for Go.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "runtime.h"

int32_t SwapInt32 (int32_t *, int32_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.SwapInt32")
  __attribute__ ((no_split_stack));

int32_t
SwapInt32 (int32_t *addr, int32_t new)
{
  return __atomic_exchange_n (addr, new, __ATOMIC_SEQ_CST);
}

int64_t SwapInt64 (int64_t *, int64_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.SwapInt64")
  __attribute__ ((no_split_stack));

int64_t
SwapInt64 (int64_t *addr, int64_t new)
{
  if (((uintptr_t) addr & 7) != 0)
    panicmem ();
  return __atomic_exchange_n (addr, new, __ATOMIC_SEQ_CST);
}

uint32_t SwapUint32 (uint32_t *, uint32_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.SwapUint32")
  __attribute__ ((no_split_stack));

uint32_t
SwapUint32 (uint32_t *addr, uint32_t new)
{
  return __atomic_exchange_n (addr, new, __ATOMIC_SEQ_CST);
}

uint64_t SwapUint64 (uint64_t *, uint64_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.SwapUint64")
  __attribute__ ((no_split_stack));

uint64_t
SwapUint64 (uint64_t *addr, uint64_t new)
{
  if (((uintptr_t) addr & 7) != 0)
    panicmem ();
  return __atomic_exchange_n (addr, new, __ATOMIC_SEQ_CST);
}

uintptr_t SwapUintptr (uintptr_t *, uintptr_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.SwapUintptr")
  __attribute__ ((no_split_stack));

uintptr_t
SwapUintptr (uintptr_t *addr, uintptr_t new)
{
  return __atomic_exchange_n (addr, new, __ATOMIC_SEQ_CST);
}

_Bool CompareAndSwapInt32 (int32_t *, int32_t, int32_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.CompareAndSwapInt32")
  __attribute__ ((no_split_stack));

_Bool
CompareAndSwapInt32 (int32_t *val, int32_t old, int32_t new)
{
  return __atomic_compare_exchange_n (val, &old, new, false, __ATOMIC_SEQ_CST,
				      __ATOMIC_RELAXED);
}

_Bool CompareAndSwapInt64 (int64_t *, int64_t, int64_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.CompareAndSwapInt64")
  __attribute__ ((no_split_stack));

_Bool
CompareAndSwapInt64 (int64_t *val, int64_t old, int64_t new)
{
  if (((uintptr_t) val & 7) != 0)
    val = NULL;
  return __atomic_compare_exchange_n (val, &old, new, false, __ATOMIC_SEQ_CST,
				      __ATOMIC_RELAXED);
}

_Bool CompareAndSwapUint32 (uint32_t *, uint32_t, uint32_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.CompareAndSwapUint32")
  __attribute__ ((no_split_stack));

_Bool
CompareAndSwapUint32 (uint32_t *val, uint32_t old, uint32_t new)
{
  return __atomic_compare_exchange_n (val, &old, new, false, __ATOMIC_SEQ_CST,
				      __ATOMIC_RELAXED);
}

_Bool CompareAndSwapUint64 (uint64_t *, uint64_t, uint64_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.CompareAndSwapUint64")
  __attribute__ ((no_split_stack));

_Bool
CompareAndSwapUint64 (uint64_t *val, uint64_t old, uint64_t new)
{
  if (((uintptr_t) val & 7) != 0)
    val = NULL;
  return __atomic_compare_exchange_n (val, &old, new, false, __ATOMIC_SEQ_CST,
				      __ATOMIC_RELAXED);
}

_Bool CompareAndSwapUintptr (uintptr_t *, uintptr_t, uintptr_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.CompareAndSwapUintptr")
  __attribute__ ((no_split_stack));

_Bool
CompareAndSwapUintptr (uintptr_t *val, uintptr_t old, uintptr_t new)
{
  return __atomic_compare_exchange_n (val, &old, new, false, __ATOMIC_SEQ_CST,
				      __ATOMIC_RELAXED);
}

int32_t AddInt32 (int32_t *, int32_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.AddInt32")
  __attribute__ ((no_split_stack));

int32_t
AddInt32 (int32_t *val, int32_t delta)
{
  return __atomic_add_fetch (val, delta, __ATOMIC_SEQ_CST);
}

uint32_t AddUint32 (uint32_t *, uint32_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.AddUint32")
  __attribute__ ((no_split_stack));

uint32_t
AddUint32 (uint32_t *val, uint32_t delta)
{
  return __atomic_add_fetch (val, delta, __ATOMIC_SEQ_CST);
}

int64_t AddInt64 (int64_t *, int64_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.AddInt64")
  __attribute__ ((no_split_stack));

int64_t
AddInt64 (int64_t *val, int64_t delta)
{
  if (((uintptr_t) val & 7) != 0)
    val = NULL;
  return __atomic_add_fetch (val, delta, __ATOMIC_SEQ_CST);
}

uint64_t AddUint64 (uint64_t *, uint64_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.AddUint64")
  __attribute__ ((no_split_stack));

uint64_t
AddUint64 (uint64_t *val, uint64_t delta)
{
  if (((uintptr_t) val & 7) != 0)
    val = NULL;
  return __atomic_add_fetch (val, delta, __ATOMIC_SEQ_CST);
}

uintptr_t AddUintptr (uintptr_t *, uintptr_t)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.AddUintptr")
  __attribute__ ((no_split_stack));

uintptr_t
AddUintptr (uintptr_t *val, uintptr_t delta)
{
  return __atomic_add_fetch (val, delta, __ATOMIC_SEQ_CST);
}

int32_t LoadInt32 (int32_t *addr)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.LoadInt32")
  __attribute__ ((no_split_stack));

int32_t
LoadInt32 (int32_t *addr)
{
  return __atomic_load_n (addr, __ATOMIC_SEQ_CST);
}

int64_t LoadInt64 (int64_t *addr)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.LoadInt64")
  __attribute__ ((no_split_stack));

int64_t
LoadInt64 (int64_t *addr)
{
  if (((uintptr_t) addr & 7) != 0)
    panicmem ();
  return __atomic_load_n (addr, __ATOMIC_SEQ_CST);
}

uint32_t LoadUint32 (uint32_t *addr)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.LoadUint32")
  __attribute__ ((no_split_stack));

uint32_t
LoadUint32 (uint32_t *addr)
{
  return __atomic_load_n (addr, __ATOMIC_SEQ_CST);
}

uint64_t LoadUint64 (uint64_t *addr)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.LoadUint64")
  __attribute__ ((no_split_stack));

uint64_t
LoadUint64 (uint64_t *addr)
{
  if (((uintptr_t) addr & 7) != 0)
    panicmem ();
  return __atomic_load_n (addr, __ATOMIC_SEQ_CST);
}

uintptr_t LoadUintptr (uintptr_t *addr)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.LoadUintptr")
  __attribute__ ((no_split_stack));

uintptr_t
LoadUintptr (uintptr_t *addr)
{
  return __atomic_load_n (addr, __ATOMIC_SEQ_CST);
}

void *LoadPointer (void **addr)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.LoadPointer")
  __attribute__ ((no_split_stack));

void *
LoadPointer (void **addr)
{
  return __atomic_load_n (addr, __ATOMIC_SEQ_CST);
}

void StoreInt32 (int32_t *addr, int32_t val)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.StoreInt32")
  __attribute__ ((no_split_stack));

void
StoreInt32 (int32_t *addr, int32_t val)
{
  __atomic_store_n (addr, val, __ATOMIC_SEQ_CST);
}

void StoreInt64 (int64_t *addr, int64_t val)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.StoreInt64")
  __attribute__ ((no_split_stack));

void
StoreInt64 (int64_t *addr, int64_t val)
{
  if (((uintptr_t) addr & 7) != 0)
    panicmem ();
  __atomic_store_n (addr, val, __ATOMIC_SEQ_CST);
}

void StoreUint32 (uint32_t *addr, uint32_t val)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.StoreUint32")
  __attribute__ ((no_split_stack));

void
StoreUint32 (uint32_t *addr, uint32_t val)
{
  __atomic_store_n (addr, val, __ATOMIC_SEQ_CST);
}

void StoreUint64 (uint64_t *addr, uint64_t val)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.StoreUint64")
  __attribute__ ((no_split_stack));

void
StoreUint64 (uint64_t *addr, uint64_t val)
{
  if (((uintptr_t) addr & 7) != 0)
    panicmem ();
  __atomic_store_n (addr, val, __ATOMIC_SEQ_CST);
}

void StoreUintptr (uintptr_t *addr, uintptr_t val)
  __asm__ (GOSYM_PREFIX "sync..z2fatomic.StoreUintptr")
  __attribute__ ((no_split_stack));

void
StoreUintptr (uintptr_t *addr, uintptr_t val)
{
  __atomic_store_n (addr, val, __ATOMIC_SEQ_CST);
}
