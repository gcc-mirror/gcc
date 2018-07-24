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
    constant_init<atomic<memory_resource*>> default_res{&newdel_res.obj};
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

} // namespace pmr
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std


