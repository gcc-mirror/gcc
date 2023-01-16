// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// C++20 29.8.4.2  basic_ostringstream constructors  [ostringstream.cons]

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-effective-target cxx11_abi }

#include <sstream>
#include <string>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01()
{
  std::wostringstream::allocator_type a;
  std::wostringstream stm(std::ios_base::out, a);
}

auto const cstr = L"This is a test";

void
test02()
{
  std::wstring s1(cstr);
  std::wostringstream stm(std::move(s1));
  VERIFY( s1.empty() );

  std::wstring s2(cstr);
  VERIFY( stm.str() == s2 );
}

void
test03()
{
  using C = wchar_t;
  using alloc_type = __gnu_test::uneq_allocator<C>;
  using traits_type = std::char_traits<C>;
  using string = std::basic_string<C, traits_type, alloc_type>;
  using ostringstream = std::basic_ostringstream<C, traits_type, alloc_type>;

  auto const mode = std::ios_base::in;
  alloc_type a1(1);
  const string s1(cstr, a1);

  // basic_ostringstream()
  {
    alloc_type a0;
    ostringstream ss;
    VERIFY( ss.str().empty() );
    VERIFY( ss.rdbuf()->get_allocator() == a0 );
    VERIFY( ss.str().get_allocator() == a0 );
  }

  // basic_ostringstream(openmode)
  {
    alloc_type a0;
    ostringstream ss(mode);
    VERIFY( ss.str().empty() );
    VERIFY( ss.rdbuf()->get_allocator() == a0 );
    VERIFY( ss.str().get_allocator() == a0 );
  }

  // basic_ostringstream(const basic_string<C,T,A>&, openmode = in)
  {
    ostringstream ss(s1);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_ostringstream(const basic_string<C,T,A>&, openmode = in)
  {
    ostringstream ss(s1, mode);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_ostringstream(openmode, const A&)
  {
    ostringstream ss(mode, a1);
    VERIFY( ss.str().empty() );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_ostringstream(basic_string<C,T,A>&&, openmode = in)
  {
    ostringstream ss(string{s1});
    VERIFY( ss.str() == s1 );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_ostringstream(basic_string<C,T,A>&&, openmode = in)
  {
    ostringstream ss(string(s1), mode);
    VERIFY( ss.str() == s1 );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_ostringstream(const basic_string<C,T,SA>&, const A&)
  {
    alloc_type a2(2);
    ostringstream ss(s1, a2);
    VERIFY( ss.str() == s1 );
    VERIFY( ss.rdbuf()->get_allocator() == a2 );
    VERIFY( ss.str().get_allocator() == a2 );
  }

  // basic_ostringstream(const basic_string<C,T,SA>&, const A&)
  {
    alloc_type a2(2);
    const std::wstring s2 = cstr;
    ostringstream ss(s2, a2);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a2 );
    VERIFY( ss.str().get_allocator() == a2 );
  }

  // basic_ostringstream(const basic_string<C,T,SA>&, openmode, const A&)
  {
    alloc_type a2(2);
    ostringstream ss(s1, mode, a2);
    VERIFY( ss.str() == s1 );
    VERIFY( ss.rdbuf()->get_allocator() == a2 );
    VERIFY( ss.str().get_allocator() == a2 );
  }

  // basic_ostringstream(const basic_string<C,T,SA>&, openmode, const A&)
  {
    alloc_type a2(2);
    const std::wstring s2 = cstr;
    ostringstream ss(s2, mode, a2);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a2 );
    VERIFY( ss.str().get_allocator() == a2 );
  }

  // basic_ostringstream(const basic_string<C,T,SA>&, openmode = in)
  {
    alloc_type a0;
    const std::wstring s2 = cstr;
    ostringstream ss(s2, mode);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a0 );
    VERIFY( ss.str().get_allocator() == a0 );
  }
}
int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
