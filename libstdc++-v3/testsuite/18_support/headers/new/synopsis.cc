// { dg-do compile { target c++11 } }

// Copyright (C) 2007-2020 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <new>

namespace std {
  class bad_alloc;
  class bad_array_new_length;
  struct nothrow_t;
  extern const nothrow_t nothrow;
  typedef void (*new_handler)();
  new_handler get_new_handler() noexcept;
  new_handler set_new_handler(new_handler new_p) noexcept;

#if __cplusplus > 201402L
  enum class align_val_t : size_t;
  // _GLIBCXX17_INLINE constexpr size_t
  //    hardware_destructive_interference_size;
  // _GLIBCXX17_INLINE constexpr size_t
  //    hardware_constructive_interference_size;
#endif
}

void* operator new(std::size_t size);
void* operator new(std::size_t size, const std::nothrow_t&) noexcept;
void  operator delete(void* ptr) throw();
void  operator delete(void* ptr, const std::nothrow_t&) throw();
void* operator new[](std::size_t size);
void* operator new[](std::size_t size, const std::nothrow_t&) throw();
void  operator delete[](void* ptr) throw();
void  operator delete[](void* ptr, const std::nothrow_t&) throw();

void* operator new  (std::size_t size, void* ptr) throw();
void* operator new[](std::size_t size, void* ptr) throw();
void  operator delete  (void* ptr, void*) throw();
void  operator delete[](void* ptr, void*) throw();

#if __cplusplus >= 201402L
// C++14 sized deallocation functions
void  operator delete(void* ptr, std::size_t size) noexcept;
void  operator delete(void* ptr, std::size_t size,
                      const std::nothrow_t&) noexcept;
void  operator delete[](void* ptr, std::size_t size) noexcept;
void  operator delete[](void* ptr, std::size_t size,
                        const std::nothrow_t&) noexcept;
#endif

#if __cplusplus > 201402L
// C++17 (de)allocation functions for types with new-extended alignment
void* operator new(std::size_t, std::align_val_t);
void* operator new(std::size_t, std::align_val_t,
                   const std::nothrow_t&) noexcept;
void  operator delete(void*, std::align_val_t) noexcept;
void  operator delete(void*, std::size_t, std::align_val_t) noexcept;
void  operator delete(void*, std::align_val_t,
                      const std::nothrow_t&) noexcept;

void* operator new[](std::size_t, std::align_val_t);
void* operator new[](std::size_t, std::align_val_t,
                     const std::nothrow_t&) noexcept;
void  operator delete[](void*, std::align_val_t) noexcept;
void  operator delete[](void*, std::size_t, std::align_val_t) noexcept;
void  operator delete[](void*, std::align_val_t,
                        const std::nothrow_t&) noexcept;
#endif
