// { dg-options "-std=gnu++11" }

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

struct X { };

using spcd = std::_Sp_counted_deleter<X*, std::default_delete<X>,
std::allocator<void>, std::__default_lock_policy>;

namespace std
{
  template<>
    struct allocator<spcd>
    {
      using value_type = spcd;

      allocator() = default;

      template<typename U>
        allocator(const allocator<U>&) { }

      value_type* allocate(size_t n)
      {
        if (n != 1)
          throw bad_alloc();
        allocated = true;
        return static_cast<value_type*>((void*)(storage));
      }

      void deallocate(value_type* p, size_t n)
      {
        VERIFY(n == 1 && p == (void*)storage && allocated);
        allocated = false;
      }

      static char storage[sizeof(spcd)];
      static bool allocated;
    };

  char allocator<spcd>::storage[];
  bool allocator<spcd>::allocated = false;
}

int main()
{
  std::shared_ptr<X> s( std::unique_ptr<X>(new X) );
  VERIFY( std::allocator<spcd>::allocated );
  s.reset();
  VERIFY( !std::allocator<spcd>::allocated );
}
