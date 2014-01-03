// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

// IA 64 C++ ABI - 5.1 External Names (a.k.a. Mangling)

#include <testsuite_hooks.h>

// libcwd tests
int main()
{
  using namespace __gnu_test;


/*

namespace libcw {
  namespace debug {
    class no_alloc_checking_allocator { };
  }
}
namespace std {
  template<typename T>
    class char_traits { };
  template<typename T, class _Traits, class _Alloc>
    class basic_string {
    public:
      template<typename T2>
	char* _S_construct(T2, T2, _Alloc const&) { return (char*)0; }
    };
}

void l(void)
{
  // Instantiation.
  std::basic_string<char, std::char_traits<char>, 
                    libcw::debug::no_alloc_checking_allocator> dummy;
  char* cp;
  libcw::debug::no_alloc_checking_allocator alloc;
  dummy._S_construct(cp, cp, alloc);
}
*/
  verify_demangle("_ZNSbIcSt11char_traitsIcEN5libcw5debug27no_alloc_checking_allocatorEE12_S_constructIPcEES6_T_S7_RKS3_", "char* std::basic_string<char, std::char_traits<char>, libcw::debug::no_alloc_checking_allocator>::_S_construct<char*>(char*, char*, libcw::debug::no_alloc_checking_allocator const&)");

  return 0;
}
