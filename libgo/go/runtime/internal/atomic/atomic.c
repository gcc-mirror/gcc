// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdint.h>

#include "runtime.h"

uint32_t Load (uint32_t *ptr)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Load")
  __attribute__ ((no_split_stack));

uint32_t
Load (uint32_t *ptr)
{
  return __atomic_load_n (ptr, __ATOMIC_SEQ_CST);
}

void *Loadp (void *ptr)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Loadp")
  __attribute__ ((no_split_stack));

void *
Loadp (void *ptr)
{
  return __atomic_load_n ((void **) ptr, __ATOMIC_SEQ_CST);
}

uint64_t Load64 (uint64_t *ptr)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Load64")
  __attribute__ ((no_split_stack));

uint64_t
Load64 (uint64_t *ptr)
{
  if (((uintptr_t) ptr & 7) != 0)
    panicmem ();
  return __atomic_load_n (ptr, __ATOMIC_SEQ_CST);
}

uint32_t LoadAcq (uint32_t *ptr)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.LoadAcq")
  __attribute__ ((no_split_stack));

uint32_t
LoadAcq (uint32_t *ptr)
{
  return __atomic_load_n (ptr, __ATOMIC_ACQUIRE);
}

uintptr_t Loaduintptr (uintptr_t *ptr)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Loaduintptr")
  __attribute__ ((no_split_stack));

uintptr_t
Loaduintptr (uintptr_t *ptr)
{
  return __atomic_load_n (ptr, __ATOMIC_SEQ_CST);
}

uintgo Loaduint (uintgo *ptr)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Loaduint")
  __attribute__ ((no_split_stack));

uintgo
Loaduint (uintgo *ptr)
{
  return __atomic_load_n (ptr, __ATOMIC_SEQ_CST);
}

int64_t Loadint64 (int64_t *ptr)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Loadint64")
  __attribute__ ((no_split_stack));

int64_t
Loadint64 (int64_t *ptr)
{
  if (((uintptr_t) ptr & 7) != 0)
    panicmem ();
  return __atomic_load_n (ptr, __ATOMIC_SEQ_CST);
}

uint32_t Xadd (uint32_t *ptr, int32_t delta)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Xadd")
  __attribute__ ((no_split_stack));

uint32_t
Xadd (uint32_t *ptr, int32_t delta)
{
  return __atomic_add_fetch (ptr, (uint32_t) delta, __ATOMIC_SEQ_CST);
}

uint64_t Xadd64 (uint64_t *ptr, int64_t delta)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Xadd64")
  __attribute__ ((no_split_stack));

uint64_t
Xadd64 (uint64_t *ptr, int64_t delta)
{
  if (((uintptr_t) ptr & 7) != 0)
    panicmem ();
  return __atomic_add_fetch (ptr, (uint64_t) delta, __ATOMIC_SEQ_CST);
}

uintptr_t Xadduintptr (uintptr_t *ptr, uintptr_t delta)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Xadduintptr")
  __attribute__ ((no_split_stack));

uintptr_t
Xadduintptr (uintptr_t *ptr, uintptr_t delta)
{
  return __atomic_add_fetch (ptr, delta, __ATOMIC_SEQ_CST);
}

int64_t Xaddint64 (int64_t *ptr, int64_t delta)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Xaddint64")
  __attribute__ ((no_split_stack));

int64_t
Xaddint64 (int64_t *ptr, int64_t delta)
{
  if (((uintptr_t) ptr & 7) != 0)
    panicmem ();
  return __atomic_add_fetch (ptr, delta, __ATOMIC_SEQ_CST);
}

uint32_t Xchg (uint32_t *ptr, uint32_t new)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Xchg")
  __attribute__ ((no_split_stack));

uint32_t
Xchg (uint32_t *ptr, uint32_t new)
{
  return __atomic_exchange_n (ptr, new, __ATOMIC_SEQ_CST);
}

uint64_t Xchg64 (uint64_t *ptr, uint64_t new)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Xchg64")
  __attribute__ ((no_split_stack));

uint64_t
Xchg64 (uint64_t *ptr, uint64_t new)
{
  if (((uintptr_t) ptr & 7) != 0)
    panicmem ();
  return __atomic_exchange_n (ptr, new, __ATOMIC_SEQ_CST);
}

uintptr_t Xchguintptr (uintptr_t *ptr, uintptr_t new)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Xchguintptr")
  __attribute__ ((no_split_stack));

uintptr_t
Xchguintptr (uintptr_t *ptr, uintptr_t new)
{
  return __atomic_exchange_n (ptr, new, __ATOMIC_SEQ_CST);
}

void And8 (uint8_t *ptr, uint8_t val)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.And8")
  __attribute__ ((no_split_stack));

void
And8 (uint8_t *ptr, uint8_t val)
{
  __atomic_and_fetch (ptr, val, __ATOMIC_SEQ_CST);
}

void Or8 (uint8_t *ptr, uint8_t val)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Or8")
  __attribute__ ((no_split_stack));

void
Or8 (uint8_t *ptr, uint8_t val)
{
  __atomic_or_fetch (ptr, val, __ATOMIC_SEQ_CST);
}

_Bool Cas (uint32_t *ptr, uint32_t old, uint32_t new)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Cas")
  __attribute__ ((no_split_stack));

_Bool
Cas (uint32_t *ptr, uint32_t old, uint32_t new)
{
  return __atomic_compare_exchange_n (ptr, &old, new, false, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
}

_Bool Cas64 (uint64_t *ptr, uint64_t old, uint64_t new)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Cas64")
  __attribute__ ((no_split_stack));

_Bool
Cas64 (uint64_t *ptr, uint64_t old, uint64_t new)
{
  if (((uintptr_t) ptr & 7) != 0)
    panicmem ();
  return __atomic_compare_exchange_n (ptr, &old, new, false, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
}

_Bool CasRel (uint32_t *ptr, uint32_t old, uint32_t new)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.CasRel")
  __attribute__ ((no_split_stack));

_Bool
CasRel (uint32_t *ptr, uint32_t old, uint32_t new)
{
  return __atomic_compare_exchange_n (ptr, &old, new, false, __ATOMIC_RELEASE, __ATOMIC_RELAXED);
}

_Bool Casp1 (void **ptr, void *old, void *new)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Casp1")
  __attribute__ ((no_split_stack));

_Bool
Casp1 (void **ptr, void *old, void *new)
{
  return __atomic_compare_exchange_n (ptr, &old, new, false, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
}

_Bool Casuintptr (uintptr_t *ptr, uintptr_t old, uintptr_t new)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Casuintptr")
  __attribute__ ((no_split_stack));

_Bool
Casuintptr (uintptr_t *ptr, uintptr_t old, uintptr_t new)
{
  return __atomic_compare_exchange_n (ptr, &old, new, false, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
}

void Store (uint32_t *ptr, uint32_t val)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Store")
  __attribute__ ((no_split_stack));

void
Store (uint32_t *ptr, uint32_t val)
{
  __atomic_store_n (ptr, val, __ATOMIC_SEQ_CST);
}

void Store64 (uint64_t *ptr, uint64_t val)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Store64")
  __attribute__ ((no_split_stack));

void
Store64 (uint64_t *ptr, uint64_t val)
{
  if (((uintptr_t) ptr & 7) != 0)
    panicmem ();
  __atomic_store_n (ptr, val, __ATOMIC_SEQ_CST);
}

void StoreRel (uint32_t *ptr, uint32_t val)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.StoreRel")
  __attribute__ ((no_split_stack));

void
StoreRel (uint32_t *ptr, uint32_t val)
{
  __atomic_store_n (ptr, val, __ATOMIC_RELEASE);
}

void Storeuintptr (uintptr_t *ptr, uintptr_t val)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.Storeuintptr")
  __attribute__ ((no_split_stack));

void
Storeuintptr (uintptr_t *ptr, uintptr_t val)
{
  __atomic_store_n (ptr, val, __ATOMIC_SEQ_CST);
}

void StorepNoWB (void *ptr, void *val)
  __asm__ (GOSYM_PREFIX "runtime..z2finternal..z2fatomic.StorepNoWB")
  __attribute__ ((no_split_stack));

void
StorepNoWB (void *ptr, void *val)
{
  __atomic_store_n ((void**) ptr, val, __ATOMIC_SEQ_CST);
}
