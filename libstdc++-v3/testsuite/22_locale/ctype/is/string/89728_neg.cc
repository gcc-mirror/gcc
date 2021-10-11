// { dg-do compile }

// Copyright (C) 2021 Free Software Foundation, Inc.
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

// { dg-error "complete" "" { target *-*-* } 0 }

#include <locale>

template <class Char, int I>
struct trait: std::char_traits<Char> {};

template <class Char, int I>
std::basic_string<Char, trait<Char, I> > make_str()
{
  return std::basic_string<Char, trait<Char, I> >();
}

void test01()
{
  const std::locale& loc = std::locale::classic();

  std::isspace(std::string(), loc);		// { dg-error "required from here" }
  std::isprint(make_str<char, 0>(), loc);	// { dg-error "required from here" }
  std::iscntrl(make_str<char, 1>(), loc);	// { dg-error "required from here" }
  std::isupper(make_str<char, 2>(), loc);	// { dg-error "required from here" }
  std::islower(make_str<char, 3>(), loc);	// { dg-error "required from here" }
  std::isalpha(make_str<char, 4>(), loc);	// { dg-error "required from here" }
  std::isdigit(make_str<char, 5>(), loc);	// { dg-error "required from here" }
  std::ispunct(make_str<char, 6>(), loc);	// { dg-error "required from here" }
  std::isxdigit(make_str<char, 7>(), loc);	// { dg-error "required from here" }
  std::isalnum(make_str<char, 8>(), loc);	// { dg-error "required from here" }
  std::isgraph(make_str<char, 9>(), loc);	// { dg-error "required from here" }
#if __cplusplus >= 201103
  std::isblank(make_str<char, 10>(), loc);	// { dg-error "required from here" "" { target c++11 } }
#endif
  std::toupper(make_str<char, 11>(), loc);	// { dg-error "required from here" }
  std::tolower(make_str<char, 12>(), loc);	// { dg-error "required from here" }
}

void test02()
{
  const std::locale& loc = std::locale::classic();

  std::isspace(std::wstring(), loc);		// { dg-error "required from here" }
  std::isprint(make_str<wchar_t, 0>(), loc);	// { dg-error "required from here" }
  std::iscntrl(make_str<wchar_t, 1>(), loc);	// { dg-error "required from here" }
  std::isupper(make_str<wchar_t, 2>(), loc);	// { dg-error "required from here" }
  std::islower(make_str<wchar_t, 3>(), loc);	// { dg-error "required from here" }
  std::isalpha(make_str<wchar_t, 4>(), loc);	// { dg-error "required from here" }
  std::isdigit(make_str<wchar_t, 5>(), loc);	// { dg-error "required from here" }
  std::ispunct(make_str<wchar_t, 6>(), loc);	// { dg-error "required from here" }
  std::isxdigit(make_str<wchar_t, 7>(), loc);	// { dg-error "required from here" }
  std::isalnum(make_str<wchar_t, 8>(), loc);	// { dg-error "required from here" }
  std::isgraph(make_str<wchar_t, 9>(), loc);	// { dg-error "required from here" }
#if __cplusplus >= 201103
  std::isblank(make_str<wchar_t, 10>(), loc);	// { dg-error "required from here" "" { target c++11 } }
#endif
  std::toupper(make_str<wchar_t, 11>(), loc);	// { dg-error "required from here" }
  std::tolower(make_str<wchar_t, 12>(), loc);	// { dg-error "required from here" }
}
