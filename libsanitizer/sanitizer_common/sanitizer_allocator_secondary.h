//===-- sanitizer_allocator_secondary.h -------------------------*- C++ -*-===//
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

// This class can (de)allocate only large chunks of memory using mmap/unmap.
// The main purpose of this allocator is to cover large and rare allocation
// sizes not covered by more efficient allocators (e.g. SizeClassAllocator64).
template <class MapUnmapCallback = NoOpMapUnmapCallback,
          class FailureHandlerT = ReturnNullOrDieOnFailure>
class LargeMmapAllocator {
 public:
  typedef FailureHandlerT FailureHandler;

  void InitLinkerInitialized() {
    page_size_ = GetPageSizeCached();
  }

  void Init() {
    internal_memset(this, 0, sizeof(*this));
    InitLinkerInitialized();
  }

  void *Allocate(AllocatorStats *stat, uptr size, uptr alignment) {
    CHECK(IsPowerOfTwo(alignment));
    uptr map_size = RoundUpMapSize(size);
    if (alignment > page_size_)
      map_size += alignment;
    // Overflow.
    if (map_size < size)
      return FailureHandler::OnBadRequest();
    uptr map_beg = reinterpret_cast<uptr>(
        MmapOrDieOnFatalError(map_size, "LargeMmapAllocator"));
    if (!map_beg)
      return FailureHandler::OnOOM();
    CHECK(IsAligned(map_beg, page_size_));
    MapUnmapCallback().OnMap(map_beg, map_size);
    uptr map_end = map_beg + map_size;
    uptr res = map_beg + page_size_;
    if (res & (alignment - 1))  // Align.
      res += alignment - (res & (alignment - 1));
    CHECK(IsAligned(res, alignment));
    CHECK(IsAligned(res, page_size_));
    CHECK_GE(res + size, map_beg);
    CHECK_LE(res + size, map_end);
    Header *h = GetHeader(res);
    h->size = size;
    h->map_beg = map_beg;
    h->map_size = map_size;
    uptr size_log = MostSignificantSetBitIndex(map_size);
    CHECK_LT(size_log, ARRAY_SIZE(stats.by_size_log));
    {
      SpinMutexLock l(&mutex_);
      uptr idx = n_chunks_++;
      chunks_sorted_ = false;
      CHECK_LT(idx, kMaxNumChunks);
      h->chunk_idx = idx;
      chunks_[idx] = h;
      stats.n_allocs++;
      stats.currently_allocated += map_size;
      stats.max_allocated = Max(stats.max_allocated, stats.currently_allocated);
      stats.by_size_log[size_log]++;
      stat->Add(AllocatorStatAllocated, map_size);
      stat->Add(AllocatorStatMapped, map_size);
    }
    return reinterpret_cast<void*>(res);
  }

  void Deallocate(AllocatorStats *stat, void *p) {
    Header *h = GetHeader(p);
    {
      SpinMutexLock l(&mutex_);
      uptr idx = h->chunk_idx;
      CHECK_EQ(chunks_[idx], h);
      CHECK_LT(idx, n_chunks_);
      chunks_[idx] = chunks_[n_chunks_ - 1];
      chunks_[idx]->chunk_idx = idx;
      n_chunks_--;
      chunks_sorted_ = false;
      stats.n_frees++;
      stats.currently_allocated -= h->map_size;
      stat->Sub(AllocatorStatAllocated, h->map_size);
      stat->Sub(AllocatorStatMapped, h->map_size);
    }
    MapUnmapCallback().OnUnmap(h->map_beg, h->map_size);
    UnmapOrDie(reinterpret_cast<void*>(h->map_beg), h->map_size);
  }

  uptr TotalMemoryUsed() {
    SpinMutexLock l(&mutex_);
    uptr res = 0;
    for (uptr i = 0; i < n_chunks_; i++) {
      Header *h = chunks_[i];
      CHECK_EQ(h->chunk_idx, i);
      res += RoundUpMapSize(h->size);
    }
    return res;
  }

  bool PointerIsMine(const void *p) {
    return GetBlockBegin(p) != nullptr;
  }

  uptr GetActuallyAllocatedSize(void *p) {
    return RoundUpTo(GetHeader(p)->size, page_size_);
  }

  // At least page_size_/2 metadata bytes is available.
  void *GetMetaData(const void *p) {
    // Too slow: CHECK_EQ(p, GetBlockBegin(p));
    if (!IsAligned(reinterpret_cast<uptr>(p), page_size_)) {
      Printf("%s: bad pointer %p\n", SanitizerToolName, p);
      CHECK(IsAligned(reinterpret_cast<uptr>(p), page_size_));
    }
    return GetHeader(p) + 1;
  }

  void *GetBlockBegin(const void *ptr) {
    uptr p = reinterpret_cast<uptr>(ptr);
    SpinMutexLock l(&mutex_);
    uptr nearest_chunk = 0;
    // Cache-friendly linear search.
    for (uptr i = 0; i < n_chunks_; i++) {
      uptr ch = reinterpret_cast<uptr>(chunks_[i]);
      if (p < ch) continue;  // p is at left to this chunk, skip it.
      if (p - ch < p - nearest_chunk)
        nearest_chunk = ch;
    }
    if (!nearest_chunk)
      return nullptr;
    Header *h = reinterpret_cast<Header *>(nearest_chunk);
    CHECK_GE(nearest_chunk, h->map_beg);
    CHECK_LT(nearest_chunk, h->map_beg + h->map_size);
    CHECK_LE(nearest_chunk, p);
    if (h->map_beg + h->map_size <= p)
      return nullptr;
    return GetUser(h);
  }

  void EnsureSortedChunks() {
    if (chunks_sorted_) return;
    SortArray(reinterpret_cast<uptr*>(chunks_), n_chunks_);
    for (uptr i = 0; i < n_chunks_; i++)
      chunks_[i]->chunk_idx = i;
    chunks_sorted_ = true;
  }

  // This function does the same as GetBlockBegin, but is much faster.
  // Must be called with the allocator locked.
  void *GetBlockBeginFastLocked(void *ptr) {
    mutex_.CheckLocked();
    uptr p = reinterpret_cast<uptr>(ptr);
    uptr n = n_chunks_;
    if (!n) return nullptr;
    EnsureSortedChunks();
    auto min_mmap_ = reinterpret_cast<uptr>(chunks_[0]);
    auto max_mmap_ =
        reinterpret_cast<uptr>(chunks_[n - 1]) + chunks_[n - 1]->map_size;
    if (p < min_mmap_ || p >= max_mmap_)
      return nullptr;
    uptr beg = 0, end = n - 1;
    // This loop is a log(n) lower_bound. It does not check for the exact match
    // to avoid expensive cache-thrashing loads.
    while (end - beg >= 2) {
      uptr mid = (beg + end) / 2;  // Invariant: mid >= beg + 1
      if (p < reinterpret_cast<uptr>(chunks_[mid]))
        end = mid - 1;  // We are not interested in chunks_[mid].
      else
        beg = mid;  // chunks_[mid] may still be what we want.
    }

    if (beg < end) {
      CHECK_EQ(beg + 1, end);
      // There are 2 chunks left, choose one.
      if (p >= reinterpret_cast<uptr>(chunks_[end]))
        beg = end;
    }

    Header *h = chunks_[beg];
    if (h->map_beg + h->map_size <= p || p < h->map_beg)
      return nullptr;
    return GetUser(h);
  }

  void PrintStats() {
    Printf("Stats: LargeMmapAllocator: allocated %zd times, "
           "remains %zd (%zd K) max %zd M; by size logs: ",
           stats.n_allocs, stats.n_allocs - stats.n_frees,
           stats.currently_allocated >> 10, stats.max_allocated >> 20);
    for (uptr i = 0; i < ARRAY_SIZE(stats.by_size_log); i++) {
      uptr c = stats.by_size_log[i];
      if (!c) continue;
      Printf("%zd:%zd; ", i, c);
    }
    Printf("\n");
  }

  // ForceLock() and ForceUnlock() are needed to implement Darwin malloc zone
  // introspection API.
  void ForceLock() {
    mutex_.Lock();
  }

  void ForceUnlock() {
    mutex_.Unlock();
  }

  // Iterate over all existing chunks.
  // The allocator must be locked when calling this function.
  void ForEachChunk(ForEachChunkCallback callback, void *arg) {
    EnsureSortedChunks();  // Avoid doing the sort while iterating.
    for (uptr i = 0; i < n_chunks_; i++) {
      auto t = chunks_[i];
      callback(reinterpret_cast<uptr>(GetUser(chunks_[i])), arg);
      // Consistency check: verify that the array did not change.
      CHECK_EQ(chunks_[i], t);
      CHECK_EQ(chunks_[i]->chunk_idx, i);
    }
  }

 private:
  static const int kMaxNumChunks = 1 << FIRST_32_SECOND_64(15, 18);
  struct Header {
    uptr map_beg;
    uptr map_size;
    uptr size;
    uptr chunk_idx;
  };

  Header *GetHeader(uptr p) {
    CHECK(IsAligned(p, page_size_));
    return reinterpret_cast<Header*>(p - page_size_);
  }
  Header *GetHeader(const void *p) {
    return GetHeader(reinterpret_cast<uptr>(p));
  }

  void *GetUser(Header *h) {
    CHECK(IsAligned((uptr)h, page_size_));
    return reinterpret_cast<void*>(reinterpret_cast<uptr>(h) + page_size_);
  }

  uptr RoundUpMapSize(uptr size) {
    return RoundUpTo(size, page_size_) + page_size_;
  }

  uptr page_size_;
  Header *chunks_[kMaxNumChunks];
  uptr n_chunks_;
  bool chunks_sorted_;
  struct Stats {
    uptr n_allocs, n_frees, currently_allocated, max_allocated, by_size_log[64];
  } stats;
  SpinMutex mutex_;
};
