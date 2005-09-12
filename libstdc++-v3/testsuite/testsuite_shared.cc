// Copyright (C) 2004, 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <string>
#include <ext/mt_allocator.h>

// libstdc++/22309
extern "C" void
foo()
{
  typedef char value_t;

  typedef __gnu_cxx::__common_pool_policy<__gnu_cxx::__pool, true> policy_t;
  typedef __gnu_cxx::__mt_alloc<value_t, policy_t> allocator_t;

  typedef std::char_traits<value_t> traits_t; 
  typedef std::basic_string<value_t, traits_t, allocator_t> string_t;

  string_t s;
  s += "west beach, indiana dunes";
}
