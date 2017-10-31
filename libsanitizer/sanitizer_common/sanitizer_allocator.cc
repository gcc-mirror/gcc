//===-- sanitizer_allocator.cc --------------------------------------------===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is shared between AddressSanitizer and ThreadSanitizer
// run-time libraries.
// This allocator is used inside run-times.
//===----------------------------------------------------------------------===//

#include "sanitizer_allocator.h"

#include "sanitizer_allocator_checks.h"
#include "sanitizer_allocator_internal.h"
#include "sanitizer_atomic.h"
#include "sanitizer_common.h"

namespace __sanitizer {

// ThreadSanitizer for Go uses libc malloc/free.
#if SANITIZER_GO || defined(SANITIZER_USE_MALLOC)
# if SANITIZER_LINUX && !SANITIZER_ANDROID
extern "C" void *__libc_malloc(uptr size);
#  if !SANITIZER_GO
extern "C" void *__libc_memalign(uptr alignment, uptr size);
#  endif
extern "C" void *__libc_realloc(void *ptr, uptr size);
extern "C" void __libc_free(void *ptr);
# else
#  include <stdlib.h>
#  define __libc_malloc malloc
#  if !SANITIZER_GO
static void *__libc_memalign(uptr alignment, uptr size) {
  void *p;
  uptr error = posix_memalign(&p, alignment, size);
  if (error) return nullptr;
  return p;
}
#  endif
#  define __libc_realloc realloc
#  define __libc_free free
# endif

static void *RawInternalAlloc(uptr size, InternalAllocatorCache *cache,
                              uptr alignment) {
  (void)cache;
#if !SANITIZER_GO
  if (alignment == 0)
    return __libc_malloc(size);
  else
    return __libc_memalign(alignment, size);
#else
  // Windows does not provide __libc_memalign/posix_memalign. It provides
  // __aligned_malloc, but the allocated blocks can't be passed to free,
  // they need to be passed to __aligned_free. InternalAlloc interface does
  // not account for such requirement. Alignemnt does not seem to be used
  // anywhere in runtime, so just call __libc_malloc for now.
  DCHECK_EQ(alignment, 0);
  return __libc_malloc(size);
#endif
}

static void *RawInternalRealloc(void *ptr, uptr size,
                                InternalAllocatorCache *cache) {
  (void)cache;
  return __libc_realloc(ptr, size);
}

static void RawInternalFree(void *ptr, InternalAllocatorCache *cache) {
  (void)cache;
  __libc_free(ptr);
}

InternalAllocator *internal_allocator() {
  return 0;
}

#else  // SANITIZER_GO || defined(SANITIZER_USE_MALLOC)

static ALIGNED(64) char internal_alloc_placeholder[sizeof(InternalAllocator)];
static atomic_uint8_t internal_allocator_initialized;
static StaticSpinMutex internal_alloc_init_mu;

static InternalAllocatorCache internal_allocator_cache;
static StaticSpinMutex internal_allocator_cache_mu;

InternalAllocator *internal_allocator() {
  InternalAllocator *internal_allocator_instance =
      reinterpret_cast<InternalAllocator *>(&internal_alloc_placeholder);
  if (atomic_load(&internal_allocator_initialized, memory_order_acquire) == 0) {
    SpinMutexLock l(&internal_alloc_init_mu);
    if (atomic_load(&internal_allocator_initialized, memory_order_relaxed) ==
        0) {
      internal_allocator_instance->Init(kReleaseToOSIntervalNever);
      atomic_store(&internal_allocator_initialized, 1, memory_order_release);
    }
  }
  return internal_allocator_instance;
}

static void *RawInternalAlloc(uptr size, InternalAllocatorCache *cache,
                              uptr alignment) {
  if (alignment == 0) alignment = 8;
  if (cache == 0) {
    SpinMutexLock l(&internal_allocator_cache_mu);
    return internal_allocator()->Allocate(&internal_allocator_cache, size,
                                          alignment);
  }
  return internal_allocator()->Allocate(cache, size, alignment);
}

static void *RawInternalRealloc(void *ptr, uptr size,
                                InternalAllocatorCache *cache) {
  uptr alignment = 8;
  if (cache == 0) {
    SpinMutexLock l(&internal_allocator_cache_mu);
    return internal_allocator()->Reallocate(&internal_allocator_cache, ptr,
                                            size, alignment);
  }
  return internal_allocator()->Reallocate(cache, ptr, size, alignment);
}

static void RawInternalFree(void *ptr, InternalAllocatorCache *cache) {
  if (!cache) {
    SpinMutexLock l(&internal_allocator_cache_mu);
    return internal_allocator()->Deallocate(&internal_allocator_cache, ptr);
  }
  internal_allocator()->Deallocate(cache, ptr);
}

#endif  // SANITIZER_GO || defined(SANITIZER_USE_MALLOC)

const u64 kBlockMagic = 0x6A6CB03ABCEBC041ull;

void *InternalAlloc(uptr size, InternalAllocatorCache *cache, uptr alignment) {
  if (size + sizeof(u64) < size)
    return nullptr;
  void *p = RawInternalAlloc(size + sizeof(u64), cache, alignment);
  if (!p)
    return nullptr;
  ((u64*)p)[0] = kBlockMagic;
  return (char*)p + sizeof(u64);
}

void *InternalRealloc(void *addr, uptr size, InternalAllocatorCache *cache) {
  if (!addr)
    return InternalAlloc(size, cache);
  if (size + sizeof(u64) < size)
    return nullptr;
  addr = (char*)addr - sizeof(u64);
  size = size + sizeof(u64);
  CHECK_EQ(kBlockMagic, ((u64*)addr)[0]);
  void *p = RawInternalRealloc(addr, size, cache);
  if (!p)
    return nullptr;
  return (char*)p + sizeof(u64);
}

void *InternalCalloc(uptr count, uptr size, InternalAllocatorCache *cache) {
  if (UNLIKELY(CheckForCallocOverflow(count, size)))
    return InternalAllocator::FailureHandler::OnBadRequest();
  void *p = InternalAlloc(count * size, cache);
  if (p) internal_memset(p, 0, count * size);
  return p;
}

void InternalFree(void *addr, InternalAllocatorCache *cache) {
  if (!addr)
    return;
  addr = (char*)addr - sizeof(u64);
  CHECK_EQ(kBlockMagic, ((u64*)addr)[0]);
  ((u64*)addr)[0] = 0;
  RawInternalFree(addr, cache);
}

// LowLevelAllocator
static LowLevelAllocateCallback low_level_alloc_callback;

void *LowLevelAllocator::Allocate(uptr size) {
  // Align allocation size.
  size = RoundUpTo(size, 8);
  if (allocated_end_ - allocated_current_ < (sptr)size) {
    uptr size_to_allocate = Max(size, GetPageSizeCached());
    allocated_current_ =
        (char*)MmapOrDie(size_to_allocate, __func__);
    allocated_end_ = allocated_current_ + size_to_allocate;
    if (low_level_alloc_callback) {
      low_level_alloc_callback((uptr)allocated_current_,
                               size_to_allocate);
    }
  }
  CHECK(allocated_end_ - allocated_current_ >= (sptr)size);
  void *res = allocated_current_;
  allocated_current_ += size;
  return res;
}

void SetLowLevelAllocateCallback(LowLevelAllocateCallback callback) {
  low_level_alloc_callback = callback;
}

static atomic_uint8_t allocator_out_of_memory = {0};
static atomic_uint8_t allocator_may_return_null = {0};

bool IsAllocatorOutOfMemory() {
  return atomic_load_relaxed(&allocator_out_of_memory);
}

// Prints error message and kills the program.
void NORETURN ReportAllocatorCannotReturnNull() {
  Report("%s's allocator is terminating the process instead of returning 0\n",
         SanitizerToolName);
  Report("If you don't like this behavior set allocator_may_return_null=1\n");
  CHECK(0);
  Die();
}

bool AllocatorMayReturnNull() {
  return atomic_load(&allocator_may_return_null, memory_order_relaxed);
}

void SetAllocatorMayReturnNull(bool may_return_null) {
  atomic_store(&allocator_may_return_null, may_return_null,
               memory_order_relaxed);
}

void *ReturnNullOrDieOnFailure::OnBadRequest() {
  if (AllocatorMayReturnNull())
    return nullptr;
  ReportAllocatorCannotReturnNull();
}

void *ReturnNullOrDieOnFailure::OnOOM() {
  atomic_store_relaxed(&allocator_out_of_memory, 1);
  if (AllocatorMayReturnNull())
    return nullptr;
  ReportAllocatorCannotReturnNull();
}

void NORETURN *DieOnFailure::OnBadRequest() {
  ReportAllocatorCannotReturnNull();
}

void NORETURN *DieOnFailure::OnOOM() {
  atomic_store_relaxed(&allocator_out_of_memory, 1);
  ReportAllocatorCannotReturnNull();
}

} // namespace __sanitizer
