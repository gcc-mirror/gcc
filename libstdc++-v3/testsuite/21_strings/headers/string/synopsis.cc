// { dg-do compile }
// { dg-require-normal-namespace "" }

// Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

#include <string>

#if __cplusplus >= 201103L
# define NOTHROW noexcept
#else
# define NOTHROW
#endif

#if __cplusplus >= 202002L
# define CONSTEXPR constexpr
#else
# define CONSTEXPR
#endif

namespace std {
  //  lib.char.traits, character traits:
  template<class charT>
  struct char_traits;
  template <> struct char_traits<char>;
  template <> struct char_traits<wchar_t>;

_GLIBCXX_BEGIN_NAMESPACE_CXX11
  //  lib.basic.string, basic_string:
  template<class charT, class traits, class Allocator >
  class basic_string;
_GLIBCXX_END_NAMESPACE_CXX11

  template<class charT, class traits, class Allocator>
  CONSTEXPR
  basic_string<charT,traits,Allocator>
  operator+(const basic_string<charT,traits,Allocator>& lhs,
	    const basic_string<charT,traits,Allocator>& rhs);
  template<class charT, class traits, class Allocator>
  CONSTEXPR
  basic_string<charT,traits,Allocator>
  operator+(const charT* lhs,
	    const basic_string<charT,traits,Allocator>& rhs);
  template<class charT, class traits, class Allocator>
  CONSTEXPR
  basic_string<charT,traits,Allocator>
  operator+(charT lhs, const basic_string<charT,traits,Allocator>& rhs);
  template<class charT, class traits, class Allocator>
  CONSTEXPR
  basic_string<charT,traits,Allocator>
  operator+(const basic_string<charT,traits,Allocator>& lhs,
	    const charT* rhs);
  template<class charT, class traits, class Allocator>
  CONSTEXPR
  basic_string<charT,traits,Allocator>
  operator+(const basic_string<charT,traits,Allocator>& lhs, charT rhs);

  template<class charT, class traits, class Allocator>
  CONSTEXPR
  bool operator==(const basic_string<charT,traits,Allocator>& lhs,
		  const basic_string<charT,traits,Allocator>& rhs) NOTHROW;
  template<class charT, class traits, class Allocator>
  CONSTEXPR
  bool operator==(const basic_string<charT,traits,Allocator>& lhs,
		  const charT* rhs);

#if __cpp_lib_three_way_comparison
  template<class charT, class traits, class Allocator>
  constexpr
  bool operator<=>(const basic_string<charT,traits,Allocator>& lhs,
		   const basic_string<charT,traits,Allocator>& rhs) NOTHROW;
  template<class charT, class traits, class Allocator>
  constexpr
  bool operator<=>(const basic_string<charT,traits,Allocator>& lhs,
		   const charT* rhs);
#else
  template<class charT, class traits, class Allocator>
  CONSTEXPR
  bool operator==(const charT* lhs,
		  const basic_string<charT,traits,Allocator>& rhs);
  template<class charT, class traits, class Allocator>
  bool operator!=(const basic_string<charT,traits,Allocator>& lhs,
		  const basic_string<charT,traits,Allocator>& rhs) NOTHROW;
  template<class charT, class traits, class Allocator>
  bool operator!=(const charT* lhs,
		  const basic_string<charT,traits,Allocator>& rhs);
  template<class charT, class traits, class Allocator>
  bool operator!=(const basic_string<charT,traits,Allocator>& lhs,
		  const charT* rhs);

  template<class charT, class traits, class Allocator>
  bool operator< (const basic_string<charT,traits,Allocator>& lhs,
		  const basic_string<charT,traits,Allocator>& rhs) NOTHROW;
  template<class charT, class traits, class Allocator>
  bool operator< (const basic_string<charT,traits,Allocator>& lhs,
		  const charT* rhs);
  template<class charT, class traits, class Allocator>
  bool operator< (const charT* lhs,
		  const basic_string<charT,traits,Allocator>& rhs);
  template<class charT, class traits, class Allocator>
  bool operator> (const basic_string<charT,traits,Allocator>& lhs,
		  const basic_string<charT,traits,Allocator>& rhs) NOTHROW;
  template<class charT, class traits, class Allocator>
  bool operator> (const basic_string<charT,traits,Allocator>& lhs,
		  const charT* rhs);
  template<class charT, class traits, class Allocator>
  bool operator> (const charT* lhs,
		  const basic_string<charT,traits,Allocator>& rhs);

  template<class charT, class traits, class Allocator>
  bool operator<=(const basic_string<charT,traits,Allocator>& lhs,
		  const basic_string<charT,traits,Allocator>& rhs) NOTHROW;
  template<class charT, class traits, class Allocator>
  bool operator<=(const basic_string<charT,traits,Allocator>& lhs,
		  const charT* rhs);
  template<class charT, class traits, class Allocator>
  bool operator<=(const charT* lhs,
		  const basic_string<charT,traits,Allocator>& rhs);
  template<class charT, class traits, class Allocator>
  bool operator>=(const basic_string<charT,traits,Allocator>& lhs,
		  const basic_string<charT,traits,Allocator>& rhs) NOTHROW;
  template<class charT, class traits, class Allocator>
  bool operator>=(const basic_string<charT,traits,Allocator>& lhs,
		  const charT* rhs);
  template<class charT, class traits, class Allocator>
  bool operator>=(const charT* lhs,
		  const basic_string<charT,traits,Allocator>& rhs);
#endif

  //  lib.string.special:
  template<class charT, class traits, class Allocator>
  CONSTEXPR
  void swap(basic_string<charT,traits,Allocator>& lhs,
	    basic_string<charT,traits,Allocator>& rhs)
#if __cplusplus >= 201103L
  noexcept(noexcept(lhs.swap(rhs)))
#endif
  ;

  template<class charT, class traits, class Allocator>
  basic_istream<charT,traits>&
  operator>>(basic_istream<charT,traits>& is,
	     basic_string<charT,traits,Allocator>& str);
  template<class charT, class traits, class Allocator>
  basic_ostream<charT, traits>&
  operator<<(basic_ostream<charT, traits>& os,
	     const basic_string<charT,traits,Allocator>& str);
  template<class charT, class traits, class Allocator>
  basic_istream<charT,traits>&
  getline(basic_istream<charT,traits>& is,
	  basic_string<charT,traits,Allocator>& str,
	  charT  delim);
  template<class charT, class traits, class Allocator>
  basic_istream<charT,traits>&
  getline(basic_istream<charT,traits>& is,
	  basic_string<charT,traits,Allocator>& str);

  typedef basic_string<char> string;
  typedef basic_string<wchar_t> wstring;
}
