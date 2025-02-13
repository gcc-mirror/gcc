// <memory_resource> implementation -*- C++ -*-

// Copyright (C) 2018-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <memory_resource>
#include <algorithm>			// lower_bound, rotate
#include <atomic>
#include <bit>				// has_single_bit, bit_ceil, bit_width
#include <new>
#include <bits/move.h>			// std::__exchange
#if ATOMIC_POINTER_LOCK_FREE != 2
# include <bits/std_mutex.h>	// std::mutex, std::lock_guard
#endif

#if __has_cpp_attribute(clang::require_constant_initialization)
#  define __constinit [[clang::require_constant_initialization]]
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace pmr
{
  // This was defined inline in 9.1 and 9.2 so code compiled by those
  // versions will not use this symbol.
  memory_resource::~memory_resource() = default;

  namespace
  {
    class newdel_res_t final : public memory_resource
    {
      void*
      do_allocate(size_t __bytes, size_t __alignment) override
      { return ::operator new(__bytes, std::align_val_t(__alignment)); }

      void
      do_deallocate(void* __p, size_t __bytes, size_t __alignment) noexcept
      override
      { ::operator delete(__p, __bytes, std::align_val_t(__alignment)); }

      bool
      do_is_equal(const memory_resource& __other) const noexcept override
      { return &__other == this; }
    };

    class null_res_t final : public memory_resource
    {
      void*
      do_allocate(size_t, size_t) override
      { std::__throw_bad_alloc(); }

      void
      do_deallocate(void*, size_t, size_t) noexcept override
      { }

      bool
      do_is_equal(const memory_resource& __other) const noexcept override
      { return &__other == this; }
    };

    template<typename T>
      struct constant_init
      {
	union {
	  T obj;
	};
	constexpr constant_init() : obj() { }

	template<typename U>
	  explicit constexpr constant_init(U arg) : obj(arg) { }

	~constant_init() { /* do nothing, union member is not destroyed */ }
      };

    __constinit constant_init<newdel_res_t> newdel_res{};
    __constinit constant_init<null_res_t> null_res{};

#ifndef _GLIBCXX_HAS_GTHREADS
# define _GLIBCXX_ATOMIC_MEM_RES_CAN_BE_CONSTANT_INITIALIZED
    // Single-threaded, no need for synchronization
    struct atomic_mem_res
    {
      constexpr
      atomic_mem_res(memory_resource* r) : val(r) { }

      memory_resource* val;

      memory_resource* load(std::memory_order) const
      {
	return val;
      }

      memory_resource* exchange(memory_resource* r, std::memory_order)
      {
	return std::__exchange(val, r);
      }
    };
#elif ATOMIC_POINTER_LOCK_FREE == 2
    using atomic_mem_res = atomic<memory_resource*>;
# define _GLIBCXX_ATOMIC_MEM_RES_CAN_BE_CONSTANT_INITIALIZED
#else
    // Can't use pointer-width atomics, define a type using a mutex instead:
    struct atomic_mem_res
    {
# ifdef __GTHREAD_MUTEX_INIT
#  define _GLIBCXX_ATOMIC_MEM_RES_CAN_BE_CONSTANT_INITIALIZED
      // std::mutex has constexpr constructor
      constexpr
# endif
      atomic_mem_res(memory_resource* r) : val(r) { }

      mutex mx;
      memory_resource* val;

      memory_resource* load(std::memory_order)
      {
	lock_guard<mutex> lock(mx);
	return val;
      }

      memory_resource* exchange(memory_resource* r, std::memory_order)
      {
	lock_guard<mutex> lock(mx);
	return std::__exchange(val, r);
      }
    };
#endif

#ifdef _GLIBCXX_ATOMIC_MEM_RES_CAN_BE_CONSTANT_INITIALIZED
    __constinit constant_init<atomic_mem_res> default_res{&newdel_res.obj};
#else
# pragma GCC diagnostic ignored "-Wprio-ctor-dtor"
    struct {
      atomic_mem_res obj = &newdel_res.obj;
    } default_res __attribute__ ((init_priority (100)));
#endif
  } // namespace

  memory_resource*
  new_delete_resource() noexcept
  { return &newdel_res.obj; }

  memory_resource*
  null_memory_resource() noexcept
  { return &null_res.obj; }

  memory_resource*
  set_default_resource(memory_resource* r) noexcept
  {
    if (r == nullptr)
      r = new_delete_resource();
    return default_res.obj.exchange(r, std::memory_order_acq_rel);
  }

  memory_resource*
  get_default_resource() noexcept
  { return default_res.obj.load(std::memory_order_acquire); }

  // Member functions for std::pmr::monotonic_buffer_resource

  // This was defined inline in 9.1 and 9.2 so code compiled by those
  // versions will not use this symbol.
  monotonic_buffer_resource::~monotonic_buffer_resource() { release(); }

  namespace {

  // aligned_size<N> stores the size and alignment of a memory allocation.
  // The size must be a multiple of N, leaving the low log2(N) bits free
  // to store the base-2 logarithm of the alignment.
  // For example, allocate(1024, 32) is stored as 1024 + log2(32) = 1029.
  template<unsigned N>
  struct aligned_size
  {
    // N must be a power of two
    static_assert( std::__popcount(N) == 1 );

    static constexpr size_t _S_align_mask = N - 1;
    static constexpr size_t _S_size_mask = ~_S_align_mask;

    constexpr
    aligned_size(size_t sz, size_t align) noexcept
    : value(sz | (std::__bit_width(align) - 1u))
    {
      __glibcxx_assert(size() == sz); // sz must be a multiple of N
    }

    constexpr size_t
    size() const noexcept
    { return value & _S_size_mask; }

    constexpr size_t
    alignment() const noexcept
    { return size_t(1) << (value & _S_align_mask); }

    size_t value; // size | log2(alignment)
  };

  // Round n up to a multiple of alignment, which must be a power of two.
  constexpr size_t aligned_ceil(size_t n, size_t alignment)
  {
    return (n + alignment - 1) & ~(alignment - 1);
  }

  } // namespace

  // Memory allocated by the upstream resource is managed in a linked list
  // of _Chunk objects. A _Chunk object recording the size and alignment of
  // the allocated block and a pointer to the previous chunk is placed
  // at end of the block.
  class monotonic_buffer_resource::_Chunk
  {
  public:
    // Return the address and size of a block of memory allocated from __r,
    // of at least __size bytes and aligned to __align.
    // Add a new _Chunk to the front of the linked list at __head.
    static pair<void*, size_t>
    allocate(memory_resource* __r, size_t __size, size_t __align,
	     _Chunk*& __head)
    {
      const size_t __orig_size = __size;

      // Add space for the _Chunk object and round up to 64 bytes.
      __size = aligned_ceil(__size + sizeof(_Chunk), 64);

      // Check for unsigned wraparound
      if (__size < __orig_size) [[unlikely]]
	{
	  // monotonic_buffer_resource::do_allocate is not allowed to throw.
	  // If the required size is too large for size_t then ask the
	  // upstream resource for an impossibly large size and alignment.
	  __size = -1;
	  __align = ~(size_t(-1) >> 1);
	}

      void* __p = __r->allocate(__size, __align);

      // Add a chunk defined by (__p, __size, __align) to linked list __head.
      // We know the end of the buffer is suitably-aligned for a _Chunk
      // because the caller ensured __align is at least alignof(max_align_t).
      void* const __back = (char*)__p + __size - sizeof(_Chunk);
      __head = ::new(__back) _Chunk(__size, __align, __head);
      return { __p, __size - sizeof(_Chunk) };
    }

    // Return every chunk in linked list __head to resource __r.
    static void
    release(_Chunk*& __head, memory_resource* __r) noexcept
    {
      _Chunk* __next = __head;
      __head = nullptr;
      while (__next)
	{
	  _Chunk* __ch = __next;
	  __next = __ch->_M_next;
	  size_t __size = __ch->_M_size.size();
	  size_t __align = __ch->_M_size.alignment();
	  void* __start = (char*)(__ch + 1) - __size;
	  __r->deallocate(__start, __size, __align);
	}
    }

  private:
    _Chunk(size_t __size, size_t __align, _Chunk* __next) noexcept
    : _M_size(__size, __align), _M_next(__next)
    { }

    aligned_size<64> _M_size;
    _Chunk* _M_next;
  };

  void
  monotonic_buffer_resource::_M_new_buffer(size_t bytes, size_t alignment)
  {
    const size_t n = std::max(bytes, _M_next_bufsiz);
    const size_t m = aligned_ceil(alignment, alignof(std::max_align_t));
    auto [p, size] = _Chunk::allocate(_M_upstream, n, m, _M_head);
    _M_current_buf = p;
    _M_avail = size;
    _M_next_bufsiz *= _S_growth_factor;
  }

  void
  monotonic_buffer_resource::_M_release_buffers() noexcept
  {
    _Chunk::release(_M_head, _M_upstream);
  }

  // Helper types for synchronized_pool_resource & unsynchronized_pool_resource

  namespace {

  // Simple bitset with runtime size.
  // Tracks which blocks in a pool chunk are used/unused.
  struct bitset
  {
    using word = uint64_t;
    using size_type // unsigned integer type with no more than 32 bits
      = conditional_t<numeric_limits<size_t>::digits <= 32, size_t, uint32_t>;

    static constexpr unsigned bits_per_word = numeric_limits<word>::digits;

    // The bitset does not own p
    bitset(void* p, size_type num_blocks)
    : _M_words(static_cast<word*>(p)), _M_size(num_blocks),
      _M_next_word(0)
    {
      const size_type last_word = num_blocks / bits_per_word;
      __builtin_memset(_M_words, 0, last_word * sizeof(*_M_words));
      // Set bits beyond _M_size, so they are not treated as free blocks:
      if (const size_type extra_bits = num_blocks % bits_per_word)
	_M_words[last_word] = word(-1) << extra_bits;
      __glibcxx_assert( empty() );
      __glibcxx_assert( free() == num_blocks );
    }

    bitset() = default;
    ~bitset() = default;

    // Number of blocks
    size_type size() const noexcept { return _M_size; }

    // Number of free blocks (unset bits)
    size_type free() const noexcept
    {
      size_type n = 0;
      for (size_type i = _M_next_word; i < nwords(); ++i)
	n += (bits_per_word - std::__popcount(_M_words[i]));
      return n;
    }

    // True if there are no free blocks (all bits are set)
    bool full() const noexcept
    {
      if (_M_next_word >= nwords())
	return true;
      // For a bitset with size() > (max_blocks_per_chunk() - 64) we will
      // have nwords() == (max_word_index() + 1) and so _M_next_word will
      // never be equal to nwords().
      // In that case, check if the last word is full:
      if (_M_next_word == max_word_index())
	return _M_words[_M_next_word] == word(-1);
      return false;
    }

    // True if size() != 0 and all blocks are free (no bits are set).
    bool empty() const noexcept
    {
      if (nwords() == 0)
	return false;
      if (_M_next_word != 0)
	return false;
      for (size_type i = 0; i < nwords() - 1; ++i)
	if (_M_words[i] != 0)
	  return false;
      word last = _M_words[nwords() - 1];
      if (const size_type extra_bits = size() % bits_per_word)
	last <<= (bits_per_word - extra_bits);
      return last == 0;
    }

    void reset() noexcept
    {
      _M_words = nullptr;
      _M_size = _M_next_word = 0;
    }

    bool operator[](size_type n) const noexcept
    {
      __glibcxx_assert( n < _M_size );
      const size_type wd = n / bits_per_word;
      const word bit = word(1) << (n % bits_per_word);
      return _M_words[wd] & bit;
    }

    size_type get_first_unset() noexcept
    {
      const size_type wd = _M_next_word;
      if (wd < nwords())
	{
	  const size_type n = std::__countr_one(_M_words[wd]);
	  if (n < bits_per_word)
	    {
	      const word bit = word(1) << n;
	      _M_words[wd] |= bit;
	      update_next_word();
	      return (wd * bits_per_word) + n;
	    }
	}
      return size_type(-1);
    }

    void set(size_type n) noexcept
    {
      __glibcxx_assert( n < _M_size );
      const size_type wd = n / bits_per_word;
      const word bit = word(1) << (n % bits_per_word);
      _M_words[wd] |= bit;
      if (wd == _M_next_word)
	update_next_word();
    }

    void clear(size_type n) noexcept
    {
      __glibcxx_assert( n < _M_size );
      const size_type wd = n / bits_per_word;
      const word bit = word(1) << (n % bits_per_word);
      _M_words[wd] &= ~bit;
      if (wd < _M_next_word)
	_M_next_word = wd;
    }

    // Update _M_next_word to refer to the next word with an unset bit.
    // The size of the _M_next_word bit-field means it cannot represent
    // the maximum possible nwords() value. To avoid wraparound to zero
    // this function saturates _M_next_word at max_word_index().
    void update_next_word() noexcept
    {
      size_type next = _M_next_word;
      while (_M_words[next] == word(-1) && ++next < nwords())
	{ }
      _M_next_word = std::min(next, max_word_index());
    }

    void swap(bitset& b) noexcept
    {
      std::swap(_M_words, b._M_words);
      size_type tmp = _M_size;
      _M_size = b._M_size;
      b._M_size = tmp;
      tmp = _M_next_word;
      _M_next_word = b._M_next_word;
      b._M_next_word = tmp;
    }

    size_type nwords() const noexcept
    { return (_M_size + bits_per_word - 1) / bits_per_word; }

    // Maximum value that can be stored in bitset::_M_size member (approx 500k)
    static constexpr size_type max_blocks_per_chunk() noexcept
    { return (size_type(1) << _S_size_digits) - 1; }

    // Maximum value that can be stored in bitset::_M_next_word member (8191).
    static constexpr size_type max_word_index() noexcept
    { return (max_blocks_per_chunk() + bits_per_word - 1) / bits_per_word; }

    word* data() const noexcept { return _M_words; }

  private:
    static constexpr unsigned _S_size_digits
      = (numeric_limits<size_type>::digits
	  + std::__bit_width(bits_per_word) - 1) / 2;

    word* _M_words = nullptr;
    // Number of blocks represented by the bitset:
    size_type _M_size : _S_size_digits;
    // Index of the first word with unset bits:
    size_type _M_next_word : numeric_limits<size_type>::digits - _S_size_digits;
  };

  // A "chunk" belonging to a pool.
  // A chunk contains many blocks of the same size.
  // Derived from bitset to reuse its tail-padding.
  struct chunk : bitset
  {
    chunk() = default;

    // p points to the start of a chunk of size bytes in length.
    // The chunk has space for n blocks, followed by a bitset of size n
    // that begins at address words.
    // This object does not own p or words, the caller will free it.
    chunk(void* p, uint32_t bytes, void* words, size_t n)
    : bitset(words, n),
      _M_bytes(bytes),
      _M_p(static_cast<std::byte*>(p))
    { __glibcxx_assert(bytes <= chunk::max_bytes_per_chunk()); }

    chunk(chunk&& c) noexcept
    : bitset(std::move(c)), _M_bytes(c._M_bytes), _M_p(c._M_p)
    {
      c._M_bytes = 0;
      c._M_p = nullptr;
      c.reset();
    }

    chunk& operator=(chunk&& c) noexcept
    {
      swap(c);
      return *this;
    }

    // Allocated size of chunk:
    bitset::size_type _M_bytes = 0;
    // Start of allocated chunk:
    std::byte* _M_p = nullptr;

    // True if there are free blocks in this chunk
    using bitset::full;
    // Number of blocks in this chunk
    using bitset::size;

    static constexpr uint32_t max_bytes_per_chunk() noexcept
    { return numeric_limits<decltype(_M_bytes)>::max(); }

    // Determine if block with address p and size block_size
    // is contained within this chunk.
    bool owns(void* p, size_t block_size)
    {
      std::less_equal<uintptr_t> less_equal;
      return less_equal(reinterpret_cast<uintptr_t>(_M_p),
			reinterpret_cast<uintptr_t>(p))
	&& less_equal(reinterpret_cast<uintptr_t>(p) + block_size,
		      reinterpret_cast<uintptr_t>(bitset::data()));
    }

    // Allocate next available block of block_size bytes from this chunk.
    void* reserve(size_t block_size) noexcept
    {
      const size_type n = get_first_unset();
      if (n == size_type(-1))
	return nullptr;
      return _M_p + (n * block_size);
    }

    // Deallocate a single block of block_size bytes
    void release(void* vp, size_t block_size)
    {
      __glibcxx_assert( owns(vp, block_size) );
      const size_t offset = static_cast<std::byte*>(vp) - _M_p;
      // Pointer is correctly aligned for a block in this chunk:
      __glibcxx_assert( (offset % block_size) == 0 );
      // Block has been allocated:
      __glibcxx_assert( (*this)[offset / block_size] == true );
      bitset::clear(offset / block_size);
    }

    // Deallocate a single block if it belongs to this chunk.
    bool try_release(void* p, size_t block_size)
    {
      if (!owns(p, block_size))
	return false;
      release(p, block_size);
      return true;
    }

    void swap(chunk& c) noexcept
    {
      std::swap(_M_bytes, c._M_bytes);
      std::swap(_M_p, c._M_p);
      bitset::swap(c);
    }

    bool operator<(const chunk& c) const noexcept
    { return std::less<const void*>{}(_M_p, c._M_p); }

    friend void swap(chunk& l, chunk& r) { l.swap(r); }

    friend bool operator<(const void* p, const chunk& c) noexcept
    { return std::less<const void*>{}(p, c._M_p); }
  };

  // For 64-bit pointers this is the size of three pointers i.e. 24 bytes.
  // For 32-bit and 20-bit pointers it's four pointers (16 bytes).
  // For 16-bit pointers it's five pointers (10 bytes).
  // TODO pad 64-bit to 4*sizeof(void*) to avoid splitting across cache lines?
  static_assert(sizeof(chunk)
      == 2 * sizeof(bitset::size_type) + 2 * sizeof(void*));

  // An oversized allocation that doesn't fit in a pool.
  struct big_block
  {
    // The minimum size of a big block.
    // All big_block allocations will be a multiple of this value.
    // Use bit_ceil to get a power of two even for e.g. 20-bit size_t.
    static constexpr size_t min
      = __bit_ceil((unsigned)numeric_limits<size_t>::digits);

    constexpr
    big_block(size_t bytes, size_t alignment)
    : _M_size(alloc_size(bytes), alignment)
    {
      // Check for unsigned wraparound
      if (size() < bytes) [[unlikely]]
	{
	  // (sync|unsync)_pool_resource::do_allocate is not allowed to throw.
	  // If the required size is too large for size_t then ask the
	  // upstream resource for an impossibly large size and alignment.
	  _M_size.value = -1;
	}
    }

    void* pointer = nullptr;
    aligned_size<min> _M_size;

    constexpr size_t size() const noexcept
    {
      if (_M_size.value == size_t(-1)) [[unlikely]]
	return size_t(-1);
      return _M_size.size();
    }

    size_t align() const noexcept
    { return _M_size.alignment(); }

    // Calculate size to be allocated instead of requested number of bytes.
    // The requested value will be rounded up to a multiple of big_block::min,
    // so the low bits are all zero and can be used to hold the alignment.
    static constexpr size_t alloc_size(size_t bytes) noexcept
    { return aligned_ceil(bytes, min); }

    friend bool operator<(void* p, const big_block& b) noexcept
    { return less<void*>{}(p, b.pointer); }

    friend bool operator<(const big_block& b, void* p) noexcept
    { return less<void*>{}(b.pointer, p); }
  };

  static_assert(sizeof(big_block) == (2 * sizeof(void*)));

  } // namespace

  // A pool that serves blocks of a particular size.
  // Each pool manages a number of chunks.
  // When a pool is full it is replenished by allocating another chunk.
  struct __pool_resource::_Pool
  {
    // Smallest supported block size
    static constexpr unsigned _S_min_block
      = std::max(sizeof(void*), alignof(bitset::word));

    _Pool(size_t __block_size, size_t __blocks_per_chunk)
    : _M_chunks(),
      _M_block_sz(__block_size),
      _M_blocks_per_chunk(__blocks_per_chunk)
    { }

    // Must call release(r) before destruction!
    ~_Pool() { __glibcxx_assert(_M_chunks.empty()); }

    _Pool(_Pool&&) noexcept = default;
    _Pool& operator=(_Pool&&) noexcept = default;

    // Size of blocks in this pool
    size_t block_size() const noexcept
    { return _M_block_sz; }

    // Allocate a block if the pool is not full, otherwise return null.
    void* try_allocate() noexcept
    {
      const size_t blocksz = block_size();
      if (!_M_chunks.empty())
	{
	  auto& last = _M_chunks.back();
	  if (void* p = last.reserve(blocksz))
	    return p;
	  // TODO last is full, so move another chunk to the back instead?
	  for (auto it = _M_chunks.begin(); it != &last; ++it)
	    if (void* p = it->reserve(blocksz))
	      return p;
	}
      return nullptr;
    }

    // Allocate a block from the pool, replenishing from upstream if needed.
    void* allocate(memory_resource* r, const pool_options& opts)
    {
      if (void* p = try_allocate())
	return p;
      replenish(r, opts);
      return _M_chunks.back().reserve(block_size());
    }

    // Return a block to the pool.
    bool deallocate(memory_resource*, void* p)
    {
      const size_t blocksz = block_size();
      if (__builtin_expect(!_M_chunks.empty(), true))
	{
	  auto& last = _M_chunks.back();
	  if (last.try_release(p, blocksz))
	    return true;
	  auto it = std::upper_bound(_M_chunks.begin(), &last, p);
	  if (it != _M_chunks.begin())
	    {
	      it--;
	      if (it->try_release(p, blocksz))
		// If chunk is empty could return to upstream, but we don't
		// currently do that. Pools only increase in size.
		return true;
	    }
	}
      return false;
    }

    void replenish(memory_resource* __r, const pool_options& __opts)
    {
      using word = chunk::word;
      const size_t __blocks = _M_blocks_per_chunk;
      const auto __bits = chunk::bits_per_word;
      const size_t __words = (__blocks + __bits - 1) / __bits;
      const size_t __block_size = block_size();
      size_t __bytes = __blocks * __block_size + __words * sizeof(word);
      size_t __alignment = std::__bit_ceil(__block_size);
      void* __p = __r->allocate(__bytes, __alignment);
      __try
	{
	  size_t __n = __blocks * __block_size;
	  void* __pwords = static_cast<char*>(__p) + __n;
	  _M_chunks.insert(chunk(__p, __bytes, __pwords, __blocks), __r);
	}
      __catch (...)
	{
	  __r->deallocate(__p, __bytes, __alignment);
	}
      if (_M_blocks_per_chunk < __opts.max_blocks_per_chunk)
	{
	  const size_t max_blocks
	    = (chunk::max_bytes_per_chunk() - sizeof(word))
	    / (__block_size + 0.125);
	  _M_blocks_per_chunk = std::min({
	      max_blocks,
	      __opts.max_blocks_per_chunk,
	      size_t(_M_blocks_per_chunk * 2)
	  });
	}
    }

    void release(memory_resource* __r)
    {
      const size_t __alignment = std::__bit_ceil(block_size());
      for (auto& __c : _M_chunks)
	if (__c._M_p)
	  __r->deallocate(__c._M_p, __c._M_bytes, __alignment);
      _M_chunks.clear(__r);
    }

    // A "resourceless vector" instead of pmr::vector, to save space.
    // All resize operations need to be passed a memory resource, which
    // obviously needs to be the same one every time.
    // Chunks are kept sorted by address of their first block, except for
    // the most recently-allocated Chunk which is at the end of the vector.
    struct vector
    {
      using value_type = chunk;
      using size_type = unsigned;
      using iterator = value_type*;

      // A vector owns its data pointer but not memory held by its elements.
      chunk* data = nullptr;
      size_type size = 0;
      size_type capacity = 0;

      vector() = default;

      vector(size_type __n, memory_resource* __r)
      : data(polymorphic_allocator<value_type>(__r).allocate(__n)),
	capacity(__n)
      { }

      // Must call clear(r) before destruction!
      ~vector() { __glibcxx_assert(data == nullptr); }

      vector(vector&& __rval) noexcept
	: data(__rval.data), size(__rval.size), capacity(__rval.capacity)
      {
	__rval.data = nullptr;
	__rval.capacity = __rval.size = 0;
      }

      vector& operator=(vector&& __rval) noexcept
      {
	__glibcxx_assert(data == nullptr);
	data = __rval.data;
	size = __rval.size;
	capacity = __rval.capacity;
	__rval.data = nullptr;
	__rval.capacity = __rval.size = 0;
	return *this;
      }

      // void resize(size_type __n, memory_resource* __r);
      // void reserve(size_type __n, memory_resource* __r);

      void clear(memory_resource* __r)
      {
	if (!data)
	  return;
	// Chunks must be individually freed before clearing the vector.
	std::destroy(begin(), end());
	polymorphic_allocator<value_type>(__r).deallocate(data, capacity);
	data = nullptr;
	capacity = size = 0;
      }

      // Sort existing elements then insert new one at the end.
      iterator insert(chunk&& c, memory_resource* r)
      {
	if (size < capacity)
	  {
	    if (size > 1)
	      {
		auto mid = end() - 1;
		std::rotate(std::lower_bound(begin(), mid, *mid), mid, end());
	      }
	  }
	else if (size > 0)
	  {
	    polymorphic_allocator<value_type> __alloc(r);
	    auto __mid = std::lower_bound(begin(), end() - 1, back());
	    auto __p = __alloc.allocate(capacity * 1.5);
	    // move [begin,__mid) to new storage
	    auto __p2 = std::move(begin(), __mid, __p);
	    // move end-1 to new storage
	    *__p2 = std::move(back());
	    // move [__mid,end-1) to new storage
	    std::move(__mid, end() - 1, ++__p2);
	    std::destroy(begin(), end());
	    __alloc.deallocate(data, capacity);
	    data = __p;
	    capacity *= 1.5;
	  }
	else
	  {
	    polymorphic_allocator<value_type> __alloc(r);
	    data = __alloc.allocate(capacity = 8);
	  }
	auto back = ::new (data + size) chunk(std::move(c));
	__glibcxx_assert(std::is_sorted(begin(), back));
	++size;
	return back;
      }

      iterator begin() const { return data; }
      iterator end() const { return data + size; }

      bool empty() const noexcept { return size == 0; }

      value_type& back() { return data[size - 1]; }
    };

    vector _M_chunks;
    unsigned _M_block_sz; 	// size of blocks allocated from this pool
    unsigned _M_blocks_per_chunk;	// number of blocks to allocate next
  };

  // An oversized allocation that doesn't fit in a pool.
  struct __pool_resource::_BigBlock : big_block
  {
    using big_block::big_block;
  };

  namespace {

  constexpr size_t pool_sizes[] = {
      8, 16, 24,
      32, 48,
      64, 80, 96, 112,
      128, 192,
      256, 320, 384, 448,
      512, 768,
#if __SIZE_WIDTH__ > 16
      // Use bigger pools if size_t has at least 20 bits.
      1024, 1536,
      2048, 3072,
#if __INT_WIDTH__ >= 32
      // Use even bigger pools if int has at least 32 bits.
      1<<12, 1<<13, 1<<14,
      1<<15, 1<<16, 1<<17,
      1<<20, 1<<21, 1<<22 // 4MB should be enough for anybody
#endif
#endif
  };

  pool_options
  munge_options(pool_options opts)
  {
    // The values in the returned struct may differ from those supplied
    // to the pool resource constructor in that values of zero will be
    // replaced with implementation-defined defaults, and sizes may be
    // rounded to unspecified granularity.

    // max_blocks_per_chunk sets the absolute maximum for the pool resource.
    // Each pool might have a smaller maximum, because pools for very large
    // objects might impose  smaller limit.
    if (opts.max_blocks_per_chunk == 0)
      {
	// Pick a default that depends on the number of bits in size_t.
	opts.max_blocks_per_chunk = __SIZE_WIDTH__ << 8;
      }
    else
      {
	// Round to preferred granularity.
	if (opts.max_blocks_per_chunk < size_t(-4))
	  {
	    // round up
	    opts.max_blocks_per_chunk
	      = aligned_ceil(opts.max_blocks_per_chunk, 4);
	  }
	else
	  {
	    // round down
	    opts.max_blocks_per_chunk &= ~size_t(3);
	  }
      }

    if (opts.max_blocks_per_chunk > chunk::max_blocks_per_chunk())
      {
	opts.max_blocks_per_chunk = chunk::max_blocks_per_chunk();
      }

    // largest_required_pool_block specifies the largest block size that will
    // be allocated from a pool. Larger allocations will come directly from
    // the upstream resource and so will not be pooled.
    if (opts.largest_required_pool_block == 0)
      {
	// Pick a sensible default that depends on the number of bits in size_t
	// (pools with larger block sizes must be explicitly requested by
	// using a non-zero value for largest_required_pool_block).
	opts.largest_required_pool_block = __SIZE_WIDTH__ << 6;
      }
    else
      {
	// Round to preferred granularity
	static_assert(std::__has_single_bit(pool_sizes[0]));
	opts.largest_required_pool_block
	  = aligned_ceil(opts.largest_required_pool_block, pool_sizes[0]);
      }

    if (opts.largest_required_pool_block < big_block::min)
      {
	opts.largest_required_pool_block = big_block::min;
      }
    else if (opts.largest_required_pool_block > std::end(pool_sizes)[-1])
      {
	// Setting _M_opts to the largest pool allows users to query it:
	opts.largest_required_pool_block = std::end(pool_sizes)[-1];
      }
    return opts;
  }

  inline int
  pool_index(size_t block_size, int npools)
  {
    auto p = std::lower_bound(pool_sizes, pool_sizes + npools, block_size);
    int n = p - pool_sizes;
    if (n != npools)
      return n;
    return -1;
  }

  inline int
  select_num_pools(const pool_options& opts)
  {
    auto p = std::lower_bound(std::begin(pool_sizes), std::end(pool_sizes),
			      opts.largest_required_pool_block);
    const int n = p - std::begin(pool_sizes);
    if (p == std::end(pool_sizes))
      return n;
    return n + 1;
  }

#ifdef _GLIBCXX_HAS_GTHREADS
  using shared_lock = std::shared_lock<shared_mutex>;
  using exclusive_lock = lock_guard<shared_mutex>;
#endif

  } // namespace

  __pool_resource::
  __pool_resource(const pool_options& opts, memory_resource* upstream)
  : _M_opts(munge_options(opts)), _M_unpooled(upstream),
    _M_npools(select_num_pools(_M_opts))
  { }

  __pool_resource::~__pool_resource() { release(); }

  void
  __pool_resource::release() noexcept
  {
    memory_resource* res = resource();
    // deallocate oversize allocations
    for (auto& b : _M_unpooled)
      res->deallocate(b.pointer, b.size(), b.align());
    pmr::vector<_BigBlock>{res}.swap(_M_unpooled);
  }

  void*
  __pool_resource::allocate(size_t bytes, size_t alignment)
  {
    auto& b = _M_unpooled.emplace_back(bytes, alignment);
    __try {
      // N.B. need to allocate b.size(), which might be larger than bytes.
      // Also use b.align() instead of alignment parameter, which will be
      // an impossibly large value if (bytes+bookkeeping) > SIZE_MAX.
      void* p = resource()->allocate(b.size(), b.align());
      b.pointer = p;
      if (_M_unpooled.size() > 1)
	{
	  const auto mid = _M_unpooled.end() - 1;
	  // move to right position in vector
	  std::rotate(std::lower_bound(_M_unpooled.begin(), mid, p),
		      mid, _M_unpooled.end());
	}
      return p;
    } __catch(...) {
      _M_unpooled.pop_back();
      __throw_exception_again;
    }
  }

  void
  __pool_resource::deallocate(void* p, size_t bytes [[maybe_unused]],
			      size_t alignment [[maybe_unused]])
  {
    const auto it
      = std::lower_bound(_M_unpooled.begin(), _M_unpooled.end(), p);
    __glibcxx_assert(it != _M_unpooled.end() && it->pointer == p);
    if (it != _M_unpooled.end() && it->pointer == p) // [[likely]]
      {
	const auto b = *it;
	__glibcxx_assert(b.size() == b.alloc_size(bytes));
	__glibcxx_assert(b.align() == alignment);
	_M_unpooled.erase(it);
	// N.B. need to deallocate b.size(), which might be larger than bytes.
	resource()->deallocate(p, b.size(), b.align());
      }
  }

  // Create array of pools, allocated from upstream resource.
  auto
  __pool_resource::_M_alloc_pools()
  -> _Pool*
  {
    polymorphic_allocator<_Pool> alloc{resource()};
    _Pool* p = alloc.allocate(_M_npools);
    for (int i = 0; i < _M_npools; ++i)
      {
	// For last pool use largest_required_pool_block
	const size_t block_size = (i+1 == _M_npools)
	  ? _M_opts.largest_required_pool_block
	  : pool_sizes[i];

	// Decide on initial number of blocks per chunk.
	// At least 16 blocks per chunk seems reasonable,
	// more for smaller blocks:
	size_t blocks_per_chunk = 1024 / block_size;
	blocks_per_chunk = std::max(size_t(16), blocks_per_chunk);
	// But don't exceed the requested max_blocks_per_chunk:
	blocks_per_chunk
	  = std::min(blocks_per_chunk, _M_opts.max_blocks_per_chunk);
	// Allow space for bitset to track which blocks are used/unused:
	blocks_per_chunk *= 1 - 1.0 / (__CHAR_BIT__ * block_size);
	// Construct a _Pool for the given block size and initial chunk size:
	alloc.construct(p + i, block_size, blocks_per_chunk);
      }
    return p;
  }

#ifdef _GLIBCXX_HAS_GTHREADS
  // synchronized_pool_resource members.

  /* Notes on implementation and thread safety:
   *
   * Each synchronized_pool_resource manages an linked list of N+1 _TPools
   * objects, where N is the number of threads using the pool resource.
   * Each _TPools object has its own set of pools, with their own chunks.
   * The first element of the list, _M_tpools[0], can be used by any thread.
   * The rest of the list contains a _TPools object for each thread,
   * accessed via the thread-specific key _M_key (and referred to for
   * exposition as _M_tpools[_M_key]).
   * The first element, _M_tpools[0], contains "orphaned chunks" which were
   * allocated by a thread which has since exited, and so there is no
   * _M_tpools[_M_key] for that thread. Orphaned chunks are never reused,
   * they're only held in _M_tpools[0] so they can be deallocated.
   * A thread can access its own thread-specific set of pools via _M_key
   * while holding a shared lock on _M_mx. Accessing _M_impl._M_unpooled
   * or _M_tpools[0] or any other thread's _M_tpools[_M_key] requires an
   * exclusive lock.
   * The upstream_resource() pointer can be obtained without a lock, but
   * any dereference of that pointer requires an exclusive lock.
   * The _M_impl._M_opts and _M_impl._M_npools members are immutable,
   * and can safely be accessed concurrently.
   *
   * In a single-threaded program (i.e. __gthread_active_p() == false)
   * the pool resource only needs one set of pools and never has orphaned
   * chunks, so just uses _M_tpools[0] directly, and _M_tpools->next is null.
   */

  extern "C" {
    static void destroy_TPools(void*);
  }

  struct synchronized_pool_resource::_TPools
  {
    // Exclusive lock must be held in the thread where this constructor runs.
    explicit
    _TPools(synchronized_pool_resource& owner, exclusive_lock&)
    : owner(owner), pools(owner._M_impl._M_alloc_pools())
    {
      // __builtin_printf("%p constructing\n", this);
      __glibcxx_assert(pools);
    }

    // Exclusive lock must be held in the thread where this destructor runs.
    ~_TPools()
    {
      __glibcxx_assert(pools);
      if (pools)
	{
	  memory_resource* r = owner.upstream_resource();
	  for (int i = 0; i < owner._M_impl._M_npools; ++i)
	    pools[i].release(r);
	  std::destroy_n(pools, owner._M_impl._M_npools);
	  polymorphic_allocator<__pool_resource::_Pool> a(r);
	  a.deallocate(pools, owner._M_impl._M_npools);
	}
      if (prev)
	prev->next = next;
      if (next)
	next->prev = prev;
    }

    // Exclusive lock must be held in the thread where this function runs.
    void move_nonempty_chunks()
    {
      __glibcxx_assert(pools);
      __glibcxx_assert(__gthread_active_p());
      if (!pools)
	return;
      memory_resource* const r = owner.upstream_resource();
      auto* const shared = owner._M_tpools->pools;
      // move all non-empty chunks to the shared _TPools
      for (int i = 0; i < owner._M_impl._M_npools; ++i)
	for (auto& c : pools[i]._M_chunks)
	  if (!c.empty())
	    shared[i]._M_chunks.insert(std::move(c), r);
    }

    synchronized_pool_resource& owner;
    __pool_resource::_Pool* pools = nullptr;
    _TPools* prev = nullptr;
    _TPools* next = nullptr;

    static void destroy(_TPools* p)
    {
      exclusive_lock l(p->owner._M_mx);
      // __glibcxx_assert(p != p->owner._M_tpools);
      p->move_nonempty_chunks();
      polymorphic_allocator<_TPools> a(p->owner.upstream_resource());
      p->~_TPools();
      a.deallocate(p, 1);
    }
  };

  // Called when a thread exits
  extern "C" {
    static void destroy_TPools(void* p)
    {
      using _TPools = synchronized_pool_resource::_TPools;
      _TPools::destroy(static_cast<_TPools*>(p));
    }
  }

  // Constructor
  synchronized_pool_resource::
  synchronized_pool_resource(const pool_options& opts,
			     memory_resource* upstream)
  : _M_impl(opts, upstream)
  {
    if (__gthread_active_p())
      if (int err = __gthread_key_create(&_M_key, destroy_TPools))
	__throw_system_error(err);
    exclusive_lock l(_M_mx);
    _M_tpools = _M_alloc_shared_tpools(l);
  }

  // Destructor
  synchronized_pool_resource::~synchronized_pool_resource()
  {
    release();
    if (__gthread_active_p())
      __gthread_key_delete(_M_key); // does not run destroy_TPools
  }

  void
  synchronized_pool_resource::release()
  {
    exclusive_lock l(_M_mx);
    if (_M_tpools)
      {
	if (__gthread_active_p())
	  {
	    __gthread_key_delete(_M_key); // does not run destroy_TPools
	    __gthread_key_create(&_M_key, destroy_TPools);
	  }
	polymorphic_allocator<_TPools> a(upstream_resource());
	// destroy+deallocate each _TPools
	do
	  {
	    _TPools* p = _M_tpools;
	    _M_tpools = _M_tpools->next;
	    p->~_TPools();
	    a.deallocate(p, 1);
	  }
	while (_M_tpools);
      }
    // release unpooled memory
    _M_impl.release();
  }

  // Caller must hold shared or exclusive lock to ensure the pointer
  // isn't invalidated before it can be used.
  auto
  synchronized_pool_resource::_M_thread_specific_pools() noexcept
  {
    __pool_resource::_Pool* pools = nullptr;
    __glibcxx_assert(__gthread_active_p());
    if (auto tp = static_cast<_TPools*>(__gthread_getspecific(_M_key)))
      {
	pools = tp->pools;
	// __glibcxx_assert(tp->pools);
      }
    return pools;
  }

  // Override for memory_resource::do_allocate
  void*
  synchronized_pool_resource::
  do_allocate(size_t bytes, size_t alignment)
  {
    const auto block_size = std::max(bytes, alignment);
    const pool_options opts = _M_impl._M_opts;
    if (block_size <= opts.largest_required_pool_block)
      {
	const ptrdiff_t index = pool_index(block_size, _M_impl._M_npools);
	if (__gthread_active_p())
	  {
	    // Try to allocate from the thread-specific pool.
	    shared_lock l(_M_mx);
	    if (auto pools = _M_thread_specific_pools()) // [[likely]]
	      {
		// Need exclusive lock to replenish so use try_allocate:
		if (void* p = pools[index].try_allocate())
		  return p;
		// Need to take exclusive lock and replenish pool.
	      }
	    // Need to allocate or replenish thread-specific pools using
	    // upstream resource, so need to hold exclusive lock.
	  }
	else // single-threaded
	  {
	    if (!_M_tpools) // [[unlikely]]
	      {
		exclusive_lock dummy(_M_mx);
		_M_tpools = _M_alloc_shared_tpools(dummy);
	      }
	    return _M_tpools->pools[index].allocate(upstream_resource(), opts);
	  }

	// N.B. Another thread could call release() now lock is not held.
	exclusive_lock excl(_M_mx);
	if (!_M_tpools) // [[unlikely]]
	  _M_tpools = _M_alloc_shared_tpools(excl);
	auto pools = _M_thread_specific_pools();
	if (!pools)
	  pools = _M_alloc_tpools(excl)->pools;
	return pools[index].allocate(upstream_resource(), opts);
      }
    exclusive_lock l(_M_mx);
    return _M_impl.allocate(bytes, alignment); // unpooled allocation
  }

  // Override for memory_resource::do_deallocate
  void
  synchronized_pool_resource::
  do_deallocate(void* p, size_t bytes, size_t alignment)
  {
    size_t block_size = std::max(bytes, alignment);
    if (block_size <= _M_impl._M_opts.largest_required_pool_block)
      {
	const ptrdiff_t index = pool_index(block_size, _M_impl._M_npools);
	__glibcxx_assert(index != -1);
	if (__gthread_active_p())
	  {
	    shared_lock l(_M_mx);
	    if (auto pools = _M_thread_specific_pools())
	      {
		// No need to lock here, no other thread is accessing this pool.
		if (pools[index].deallocate(upstream_resource(), p))
		  return;
	      }
	    // Block might have come from a different thread's pool,
	    // take exclusive lock and check every pool.
	  }
	else // single-threaded
	  {
	    __glibcxx_assert(_M_tpools != nullptr);
	    if (_M_tpools) // [[likely]]
	      _M_tpools->pools[index].deallocate(upstream_resource(), p);
	    return;
	  }

	// TODO store {p, bytes, alignment} somewhere and defer returning
	// the block to the correct thread-specific pool until we next
	// take the exclusive lock.

	exclusive_lock excl(_M_mx);
	auto my_pools = _M_thread_specific_pools();
	for (_TPools* t = _M_tpools; t != nullptr; t = t->next)
	  {
	    if (t->pools != my_pools)
	      if (t->pools) // [[likely]]
		{
		  if (t->pools[index].deallocate(upstream_resource(), p))
		    return;
		}
	  }
	// Not necessarily an error to reach here, release() could have been
	// called on another thread between releasing the shared lock and
	// acquiring the exclusive lock.
	return;
      }
    exclusive_lock l(_M_mx);
    _M_impl.deallocate(p, bytes, alignment);
  }

  // Allocate a thread-specific _TPools object and add it to the linked list.
  auto
  synchronized_pool_resource::_M_alloc_tpools(exclusive_lock& l)
  -> _TPools*
  {
    __glibcxx_assert(_M_tpools != nullptr);
    __glibcxx_assert(__gthread_active_p());
    // dump_list(_M_tpools);
    polymorphic_allocator<_TPools> a(upstream_resource());
    _TPools* p = a.allocate(1);
    bool constructed = false;
    __try
      {
	a.construct(p, *this, l);
	constructed = true;
	// __glibcxx_assert(__gthread_getspecific(_M_key) == nullptr);
	if (int err = __gthread_setspecific(_M_key, p))
	  __throw_system_error(err);
      }
    __catch(...)
      {
	if (constructed)
	  a.destroy(p);
	a.deallocate(p, 1);
	__throw_exception_again;
      }
    p->prev = _M_tpools;
    p->next = _M_tpools->next;
    _M_tpools->next = p;
    if (p->next)
      p->next->prev = p;
    return p;
  }

  // Allocate the shared _TPools object, _M_tpools[0]
  auto
  synchronized_pool_resource::_M_alloc_shared_tpools(exclusive_lock& l)
  -> _TPools*
  {
    __glibcxx_assert(_M_tpools == nullptr);
    polymorphic_allocator<_TPools> a(upstream_resource());
    _TPools* p = a.allocate(1);
    __try
      {
	a.construct(p, *this, l);
      }
    __catch(...)
      {
	a.deallocate(p, 1);
	__throw_exception_again;
      }
    // __glibcxx_assert(p->next == nullptr);
    // __glibcxx_assert(p->prev == nullptr);
    return p;
  }
#endif // _GLIBCXX_HAS_GTHREADS

  // unsynchronized_pool_resource member functions

  // Constructor
  unsynchronized_pool_resource::
  unsynchronized_pool_resource(const pool_options& opts,
			       memory_resource* upstream)
  : _M_impl(opts, upstream), _M_pools(_M_impl._M_alloc_pools())
  { }

  // Destructor
  unsynchronized_pool_resource::~unsynchronized_pool_resource()
  { release(); }

  // Return all memory to upstream resource.
  void
  unsynchronized_pool_resource::release()
  {
    // release pooled memory
    if (_M_pools)
      {
	memory_resource* res = upstream_resource();
	polymorphic_allocator<_Pool> alloc{res};
	for (int i = 0; i < _M_impl._M_npools; ++i)
	  {
	    _M_pools[i].release(res);
	    alloc.destroy(_M_pools + i);
	  }
	alloc.deallocate(_M_pools, _M_impl._M_npools);
	_M_pools = nullptr;
      }

    // release unpooled memory
    _M_impl.release();
  }

  // Find the right pool for a block of size block_size.
  auto
  unsynchronized_pool_resource::_M_find_pool(size_t block_size) noexcept
  {
    __pool_resource::_Pool* pool = nullptr;
    if (_M_pools) // [[likely]]
      {
	int index = pool_index(block_size, _M_impl._M_npools);
	if (index != -1)
	  pool = _M_pools + index;
      }
    return pool;
  }

  // Override for memory_resource::do_allocate
  void*
  unsynchronized_pool_resource::do_allocate(size_t bytes, size_t alignment)
  {
    const auto block_size = std::max(bytes, alignment);
    if (block_size <= _M_impl._M_opts.largest_required_pool_block)
      {
	// Recreate pools if release() has been called:
	if (__builtin_expect(_M_pools == nullptr, false))
	  _M_pools = _M_impl._M_alloc_pools();
	if (auto pool = _M_find_pool(block_size))
	  return pool->allocate(upstream_resource(), _M_impl._M_opts);
      }
    return _M_impl.allocate(bytes, alignment);
  }

  // Override for memory_resource::do_deallocate
  void
  unsynchronized_pool_resource::
  do_deallocate(void* p, size_t bytes, size_t alignment)
  {
    size_t block_size = std::max(bytes, alignment);
    if (block_size <= _M_impl._M_opts.largest_required_pool_block)
      {
	if (auto pool = _M_find_pool(block_size))
	  {
	    pool->deallocate(upstream_resource(), p);
	    return;
	  }
      }
    _M_impl.deallocate(p, bytes, alignment);
  }

} // namespace pmr
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
