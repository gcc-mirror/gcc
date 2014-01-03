// { dg-do compile }
// { dg-options "-std=gnu++11" }

// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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
  struct nothrow_t;
  extern const nothrow_t nothrow;
  typedef void (*new_handler)();
  new_handler set_new_handler(new_handler new_p) throw();
  new_handler get_new_handler() noexcept;
}

void* operator new(std::size_t size) throw(std::bad_alloc);
void* operator new(std::size_t size, const std::nothrow_t&) throw();
void  operator delete(void* ptr) throw();
void  operator delete(void* ptr, const std::nothrow_t&) throw();
void* operator new[](std::size_t size) throw(std::bad_alloc);
void* operator new[](std::size_t size, const std::nothrow_t&) throw();
void  operator delete[](void* ptr) throw();
void  operator delete[](void* ptr, const std::nothrow_t&) throw();

void* operator new  (std::size_t size, void* ptr) throw();
void* operator new[](std::size_t size, void* ptr) throw();
void  operator delete  (void* ptr, void*) throw();
void  operator delete[](void* ptr, void*) throw();
