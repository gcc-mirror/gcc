//===-- sanitizer_allocator.h -----------------------------------*- C++ -*-===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Specialized memory allocator for ThreadSanitizer, MemorySanitizer, etc.
//
//===----------------------------------------------------------------------===//

#ifndef SANITIZER_ALLOCATOR_H
#define SANITIZER_ALLOCATOR_H

#include "sanitizer_internal_defs.h"
#include "sanitizer_common.h"
#include "sanitizer_libc.h"
#include "sanitizer_list.h"
#include "sanitizer_mutex.h"
#include "sanitizer_lfstack.h"
#include "sanitizer_procmaps.h"

namespace __sanitizer {

// Since flags are immutable and allocator behavior can be changed at runtime
// (unit tests or ASan on Android are some examples), allocator_may_return_null
// flag value is cached here and can be altered later.
bool AllocatorMayReturnNull();
void SetAllocatorMayReturnNull(bool may_return_null);

// Allocator failure handling policies:
// Implements AllocatorMayReturnNull policy, returns null when the flag is set,
// dies otherwise.
struct ReturnNullOrDieOnFailure {
  static void *OnBadRequest();
  static void *OnOOM();
};
// Always dies on the failure.
struct DieOnFailure {
  static void NORETURN *OnBadRequest();
  static void NORETURN *OnOOM();
};

// Returns true if allocator detected OOM condition. Can be used to avoid memory
// hungry operations. Set when AllocatorReturnNullOrDieOnOOM() is called.
bool IsAllocatorOutOfMemory();

// Allocators call these callbacks on mmap/munmap.
struct NoOpMapUnmapCallback {
  void OnMap(uptr p, uptr size) const { }
  void OnUnmap(uptr p, uptr size) const { }
};

// Callback type for iterating over chunks.
typedef void (*ForEachChunkCallback)(uptr chunk, void *arg);

#include "sanitizer_allocator_size_class_map.h"
#include "sanitizer_allocator_stats.h"
#include "sanitizer_allocator_primary64.h"
#include "sanitizer_allocator_bytemap.h"
#include "sanitizer_allocator_primary32.h"
#include "sanitizer_allocator_local_cache.h"
#include "sanitizer_allocator_secondary.h"
#include "sanitizer_allocator_combined.h"

} // namespace __sanitizer

#endif // SANITIZER_ALLOCATOR_H
