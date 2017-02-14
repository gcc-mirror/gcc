// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003-2017 Free Software Foundation, Inc.
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
namespace std {
  template<bool b, int i>
    class __default_alloc_template { };

  template<typename CHAR>
    class string_char_traits { };

  template<class BASIC_STRING, class ADAPTOR>
    struct _Alloc_traits {
      static char _S_instanceless;
    };
  template<class BASIC_STRING, class ADAPTOR>
    char _Alloc_traits<BASIC_STRING, ADAPTOR>::_S_instanceless;
}
namespace libcw {
  namespace debug {
    namespace _private_ {
      template<typename CHAR, class ALLOCATOR, bool b>
	class allocator_adaptor { };
    }
  }
}

char x;
void q(void)
{
  std::_Alloc_traits<
    std::basic_string<
      char,
      std::string_char_traits<char>,
      libcw::debug::_private_::allocator_adaptor<
	char,
        std::__default_alloc_template<false, 327664>,
	true
      >
    >,
    libcw::debug::_private_::allocator_adaptor<
      std::basic_string<
	char,
        std::string_char_traits<char>,
	libcw::debug::_private_::allocator_adaptor<
	  char,
	  std::__default_alloc_template<false, 327664>,
	  true
	>
      >,
      std::__default_alloc_template<false, 327664>,
      true
    >
  > dummy1;
  x = dummy1._S_instanceless;
}
*/
    // cplus-dem FAIL 
    verify_demangle("_ZNSt13_Alloc_traitsISbIcSt18string_char_traitsIcEN5libcw5debug9_private_17allocator_adaptorIcSt24__default_alloc_templateILb0ELi327664EELb1EEEENS5_IS9_S7_Lb1EEEE15_S_instancelessE","std::_Alloc_traits<std::basic_string<char, std::string_char_traits<char>, libcw::debug::_private_::allocator_adaptor<char, std::__default_alloc_template<false, 327664>, true> >, libcw::debug::_private_::allocator_adaptor<std::basic_string<char, std::string_char_traits<char>, libcw::debug::_private_::allocator_adaptor<char, std::__default_alloc_template<false, 327664>, true> >, std::__default_alloc_template<false, 327664>, true> >::_S_instanceless");

  return 0;
}
