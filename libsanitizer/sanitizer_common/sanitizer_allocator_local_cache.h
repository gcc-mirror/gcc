//===-- sanitizer_allocator_local_cache.h -----------------------*- C++ -*-===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Part of the Sanitizer Allocator.
//
//===----------------------------------------------------------------------===//
#ifndef SANITIZER_ALLOCATOR_H
#error This file must be included inside sanitizer_allocator.h
#endif

// Objects of this type should be used as local caches for SizeClassAllocator64
// or SizeClassAllocator32. Since the typical use of this class is to have one
// object per thread in TLS, is has to be POD.
template<class SizeClassAllocator>
struct SizeClassAllocatorLocalCache
    : SizeClassAllocator::AllocatorCache {
};

// Cache used by SizeClassAllocator64.
template <class SizeClassAllocator>
struct SizeClassAllocator64LocalCache {
  typedef SizeClassAllocator Allocator;

  void Init(AllocatorGlobalStats *s) {
    stats_.Init();
    if (s)
      s->Register(&stats_);
  }

  void Destroy(SizeClassAllocator *allocator, AllocatorGlobalStats *s) {
    Drain(allocator);
    if (s)
      s->Unregister(&stats_);
  }

  void *Allocate(SizeClassAllocator *allocator, uptr class_id) {
    CHECK_NE(class_id, 0UL);
    CHECK_LT(class_id, kNumClasses);
    PerClass *c = &per_class_[class_id];
    if (UNLIKELY(c->count == 0)) {
      if (UNLIKELY(!Refill(c, allocator, class_id)))
        return nullptr;
    }
    stats_.Add(AllocatorStatAllocated, c->class_size);
    CHECK_GT(c->count, 0);
    CompactPtrT chunk = c->chunks[--c->count];
    void *res = reinterpret_cast<void *>(allocator->CompactPtrToPointer(
        allocator->GetRegionBeginBySizeClass(class_id), chunk));
    return res;
  }

  void Deallocate(SizeClassAllocator *allocator, uptr class_id, void *p) {
    CHECK_NE(class_id, 0UL);
    CHECK_LT(class_id, kNumClasses);
    // If the first allocator call on a new thread is a deallocation, then
    // max_count will be zero, leading to check failure.
    InitCache();
    PerClass *c = &per_class_[class_id];
    stats_.Sub(AllocatorStatAllocated, c->class_size);
    CHECK_NE(c->max_count, 0UL);
    if (UNLIKELY(c->count == c->max_count))
      Drain(c, allocator, class_id, c->max_count / 2);
    CompactPtrT chunk = allocator->PointerToCompactPtr(
        allocator->GetRegionBeginBySizeClass(class_id),
        reinterpret_cast<uptr>(p));
    c->chunks[c->count++] = chunk;
  }

  void Drain(SizeClassAllocator *allocator) {
    for (uptr i = 0; i < kNumClasses; i++) {
      PerClass *c = &per_class_[i];
      while (c->count > 0)
        Drain(c, allocator, i, c->count);
    }
  }

 private:
  typedef typename Allocator::SizeClassMapT SizeClassMap;
  static const uptr kNumClasses = SizeClassMap::kNumClasses;
  typedef typename Allocator::CompactPtrT CompactPtrT;

  struct PerClass {
    u32 count;
    u32 max_count;
    uptr class_size;
    CompactPtrT chunks[2 * SizeClassMap::kMaxNumCachedHint];
  };
  PerClass per_class_[kNumClasses];
  AllocatorStats stats_;

  void InitCache() {
    if (LIKELY(per_class_[1].max_count))
      return;
    for (uptr i = 0; i < kNumClasses; i++) {
      PerClass *c = &per_class_[i];
      c->max_count = 2 * SizeClassMap::MaxCachedHint(i);
      c->class_size = Allocator::ClassIdToSize(i);
    }
  }

  NOINLINE bool Refill(PerClass *c, SizeClassAllocator *allocator,
                       uptr class_id) {
    InitCache();
    uptr num_requested_chunks = c->max_count / 2;
    if (UNLIKELY(!allocator->GetFromAllocator(&stats_, class_id, c->chunks,
                                              num_requested_chunks)))
      return false;
    c->count = num_requested_chunks;
    return true;
  }

  NOINLINE void Drain(PerClass *c, SizeClassAllocator *allocator, uptr class_id,
                      uptr count) {
    InitCache();
    CHECK_GE(c->count, count);
    uptr first_idx_to_drain = c->count - count;
    c->count -= count;
    allocator->ReturnToAllocator(&stats_, class_id,
                                 &c->chunks[first_idx_to_drain], count);
  }
};

// Cache used by SizeClassAllocator32.
template <class SizeClassAllocator>
struct SizeClassAllocator32LocalCache {
  typedef SizeClassAllocator Allocator;
  typedef typename Allocator::TransferBatch TransferBatch;

  void Init(AllocatorGlobalStats *s) {
    stats_.Init();
    if (s)
      s->Register(&stats_);
  }

  // Returns a TransferBatch suitable for class_id.
  TransferBatch *CreateBatch(uptr class_id, SizeClassAllocator *allocator,
                             TransferBatch *b) {
    if (uptr batch_class_id = per_class_[class_id].batch_class_id)
      return (TransferBatch*)Allocate(allocator, batch_class_id);
    return b;
  }

  // Destroys TransferBatch b.
  void DestroyBatch(uptr class_id, SizeClassAllocator *allocator,
                    TransferBatch *b) {
    if (uptr batch_class_id = per_class_[class_id].batch_class_id)
      Deallocate(allocator, batch_class_id, b);
  }

  void Destroy(SizeClassAllocator *allocator, AllocatorGlobalStats *s) {
    Drain(allocator);
    if (s)
      s->Unregister(&stats_);
  }

  void *Allocate(SizeClassAllocator *allocator, uptr class_id) {
    CHECK_NE(class_id, 0UL);
    CHECK_LT(class_id, kNumClasses);
    PerClass *c = &per_class_[class_id];
    if (UNLIKELY(c->count == 0)) {
      if (UNLIKELY(!Refill(allocator, class_id)))
        return nullptr;
    }
    stats_.Add(AllocatorStatAllocated, c->class_size);
    void *res = c->batch[--c->count];
    PREFETCH(c->batch[c->count - 1]);
    return res;
  }

  void Deallocate(SizeClassAllocator *allocator, uptr class_id, void *p) {
    CHECK_NE(class_id, 0UL);
    CHECK_LT(class_id, kNumClasses);
    // If the first allocator call on a new thread is a deallocation, then
    // max_count will be zero, leading to check failure.
    InitCache();
    PerClass *c = &per_class_[class_id];
    stats_.Sub(AllocatorStatAllocated, c->class_size);
    CHECK_NE(c->max_count, 0UL);
    if (UNLIKELY(c->count == c->max_count))
      Drain(allocator, class_id);
    c->batch[c->count++] = p;
  }

  void Drain(SizeClassAllocator *allocator) {
    for (uptr i = 0; i < kNumClasses; i++) {
      PerClass *c = &per_class_[i];
      while (c->count > 0)
        Drain(allocator, i);
    }
  }

 private:
  typedef typename Allocator::SizeClassMapT SizeClassMap;
  static const uptr kBatchClassID = SizeClassMap::kBatchClassID;
  static const uptr kNumClasses = SizeClassMap::kNumClasses;
  // If kUseSeparateSizeClassForBatch is true, all TransferBatch objects are
  // allocated from kBatchClassID size class (except for those that are needed
  // for kBatchClassID itself). The goal is to have TransferBatches in a totally
  // different region of RAM to improve security.
  static const bool kUseSeparateSizeClassForBatch =
      Allocator::kUseSeparateSizeClassForBatch;

  struct PerClass {
    uptr count;
    uptr max_count;
    uptr class_size;
    uptr batch_class_id;
    void *batch[2 * TransferBatch::kMaxNumCached];
  };
  PerClass per_class_[kNumClasses];
  AllocatorStats stats_;

  void InitCache() {
    if (LIKELY(per_class_[1].max_count))
      return;
    const uptr batch_class_id = SizeClassMap::ClassID(sizeof(TransferBatch));
    for (uptr i = 0; i < kNumClasses; i++) {
      PerClass *c = &per_class_[i];
      uptr max_cached = TransferBatch::MaxCached(i);
      c->max_count = 2 * max_cached;
      c->class_size = Allocator::ClassIdToSize(i);
      // Precompute the class id to use to store batches for the current class
      // id. 0 means the class size is large enough to store a batch within one
      // of the chunks. If using a separate size class, it will always be
      // kBatchClassID, except for kBatchClassID itself.
      if (kUseSeparateSizeClassForBatch) {
        c->batch_class_id = (i == kBatchClassID) ? 0 : kBatchClassID;
      } else {
        c->batch_class_id = (c->class_size <
          TransferBatch::AllocationSizeRequiredForNElements(max_cached)) ?
              batch_class_id : 0;
      }
    }
  }

  NOINLINE bool Refill(SizeClassAllocator *allocator, uptr class_id) {
    InitCache();
    PerClass *c = &per_class_[class_id];
    TransferBatch *b = allocator->AllocateBatch(&stats_, this, class_id);
    if (UNLIKELY(!b))
      return false;
    CHECK_GT(b->Count(), 0);
    b->CopyToArray(c->batch);
    c->count = b->Count();
    DestroyBatch(class_id, allocator, b);
    return true;
  }

  NOINLINE void Drain(SizeClassAllocator *allocator, uptr class_id) {
    InitCache();
    PerClass *c = &per_class_[class_id];
    uptr cnt = Min(c->max_count / 2, c->count);
    uptr first_idx_to_drain = c->count - cnt;
    TransferBatch *b = CreateBatch(
        class_id, allocator, (TransferBatch *)c->batch[first_idx_to_drain]);
    // Failure to allocate a batch while releasing memory is non recoverable.
    // TODO(alekseys): Figure out how to do it without allocating a new batch.
    if (UNLIKELY(!b))
      DieOnFailure::OnOOM();
    b->SetFromArray(allocator->GetRegionBeginBySizeClass(class_id),
                    &c->batch[first_idx_to_drain], cnt);
    c->count -= cnt;
    allocator->DeallocateBatch(&stats_, class_id, b);
  }
};
