// <memory_resource> implementation -*- C++ -*-

// Copyright (C) 2018 Free Software Foundation, Inc.
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
#include <atomic>
#include <new>
#if ATOMIC_POINTER_LOCK_FREE != 2
# include <bits/std_mutex.h>	// std::mutex, std::lock_guard
# include <bits/move.h>		// std::exchange
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace pmr
{
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
	  unsigned char unused;
	  T obj;
	};
	constexpr constant_init() : obj() { }

	template<typename U>
	  explicit constexpr constant_init(U arg) : obj(arg) { }

	~constant_init() { /* do nothing, union member is not destroyed */ }
      };

    constant_init<newdel_res_t> newdel_res{};
    constant_init<null_res_t> null_res{};
#if ATOMIC_POINTER_LOCK_FREE == 2
    using atomic_mem_res = atomic<memory_resource*>;
# define _GLIBCXX_ATOMIC_MEM_RES_CAN_BE_CONSTANT_INITIALIZED
#elif defined(_GLIBCXX_HAS_GTHREADS)
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

      memory_resource* load()
      {
	lock_guard<mutex> lock(mx);
	return val;
      }

      memory_resource* exchange(memory_resource* r)
      {
	lock_guard<mutex> lock(mx);
	return std::exchange(val, r);
      }
    };
#else
# define _GLIBCXX_ATOMIC_MEM_RES_CAN_BE_CONSTANT_INITIALIZED
    // Single-threaded, no need for synchronization
    struct atomic_mem_res
    {
      constexpr
      atomic_mem_res(memory_resource* r) : val(r) { }

      memory_resource* val;

      memory_resource* load() const
      {
	return val;
      }

      memory_resource* exchange(memory_resource* r)
      {
	return std::exchange(val, r);
      }
    };
#endif // ATOMIC_POINTER_LOCK_FREE == 2

#ifdef _GLIBCXX_ATOMIC_MEM_RES_CAN_BE_CONSTANT_INITIALIZED
    constant_init<atomic_mem_res> default_res{&newdel_res.obj};
#else
# include "default_resource.h"
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
    return default_res.obj.exchange(r);
  }

  memory_resource*
  get_default_resource() noexcept
  { return default_res.obj.load(); }

  // Member functions for std::pmr::monotonic_buffer_resource

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
      __size = std::__ceil2(__size + sizeof(_Chunk));
      void* __p = __r->allocate(__size, __align);
      // Add a chunk defined by (__p, __size, __align) to linked list __head.
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
	  __builtin_memcpy(&__next, __ch->_M_next, sizeof(_Chunk*));

	  __glibcxx_assert(__ch->_M_canary != 0);
	  __glibcxx_assert(__ch->_M_canary == (__ch->_M_size|__ch->_M_align));

	  if (__ch->_M_canary != (__ch->_M_size | __ch->_M_align))
	    return; // buffer overflow detected!

	  size_t __size = (1u << __ch->_M_size);
	  size_t __align = (1u << __ch->_M_align);
	  void* __start = (char*)(__ch + 1) - __size;
	  __r->deallocate(__start, __size, __align);
	}
    }

  private:
    _Chunk(size_t __size, size_t __align, _Chunk* __next) noexcept
    : _M_size(std::__log2p1(__size) - 1),
      _M_align(std::__log2p1(__align) - 1)
    {
      __builtin_memcpy(_M_next, &__next, sizeof(__next));
      _M_canary = _M_size | _M_align;
    }

    unsigned char _M_canary;
    unsigned char _M_size;
    unsigned char _M_align;
    unsigned char _M_next[sizeof(_Chunk*)];
  };

  void
  monotonic_buffer_resource::_M_new_buffer(size_t bytes, size_t alignment)
  {
    // Need to check this somewhere, so put it here:
    static_assert(alignof(monotonic_buffer_resource::_Chunk) == 1);

    const size_t n = std::max(bytes, _M_next_bufsiz);
    const size_t m = std::max(alignment, alignof(std::max_align_t));
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

} // namespace pmr
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std


