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

// C++20 29.8.3.2  basic_istringstream constructors  [istringstream.cons]

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
  std::istringstream::allocator_type a;
  std::istringstream stm(std::ios_base::in, a);
}

auto const cstr = "This is a test string";

void
test02()
{
  std::string s1(cstr);
  std::istringstream stm(std::move(s1));
  VERIFY( s1.empty() );

  std::string s2(cstr);
  VERIFY( stm.str() == s2 );
}

void
test03()
{
  using C = char;
  using alloc_type = __gnu_test::uneq_allocator<C>;
  using traits_type = std::char_traits<C>;
  using string = std::basic_string<C, traits_type, alloc_type>;
  using istringstream = std::basic_istringstream<C, traits_type, alloc_type>;

  auto const mode = std::ios_base::in;
  alloc_type a1(1);
  const string s1(cstr, a1);

  // basic_istringstream()
  {
    alloc_type a0;
    istringstream ss;
    VERIFY( ss.str().empty() );
    VERIFY( ss.rdbuf()->get_allocator() == a0 );
    VERIFY( ss.str().get_allocator() == a0 );
  }

  // basic_istringstream(openmode)
  {
    alloc_type a0;
    istringstream ss(mode);
    VERIFY( ss.str().empty() );
    VERIFY( ss.rdbuf()->get_allocator() == a0 );
    VERIFY( ss.str().get_allocator() == a0 );
  }

  // basic_istringstream(const basic_string<C,T,A>&, openmode = in)
  {
    istringstream ss(s1);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_istringstream(const basic_string<C,T,A>&, openmode = in)
  {
    istringstream ss(s1, mode);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_istringstream(openmode, const A&)
  {
    istringstream ss(mode, a1);
    VERIFY( ss.str().empty() );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_istringstream(basic_string<C,T,A>&&, openmode = in)
  {
    istringstream ss(string{s1});
    VERIFY( ss.str() == s1 );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_istringstream(basic_string<C,T,A>&&, openmode = in)
  {
    istringstream ss(string(s1), mode);
    VERIFY( ss.str() == s1 );
    VERIFY( ss.rdbuf()->get_allocator() == a1 );
    VERIFY( ss.str().get_allocator() == a1 );
  }

  // basic_istringstream(const basic_string<C,T,SA>&, const A&)
  {
    alloc_type a2(2);
    istringstream ss(s1, a2);
    VERIFY( ss.str() == s1 );
    VERIFY( ss.rdbuf()->get_allocator() == a2 );
    VERIFY( ss.str().get_allocator() == a2 );
  }

  // basic_istringstream(const basic_string<C,T,SA>&, const A&)
  {
    alloc_type a2(2);
    const std::string s2 = cstr;
    istringstream ss(s2, a2);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a2 );
    VERIFY( ss.str().get_allocator() == a2 );
  }

  // basic_istringstream(const basic_string<C,T,SA>&, openmode, const A&)
  {
    alloc_type a2(2);
    istringstream ss(s1, mode, a2);
    VERIFY( ss.str() == s1 );
    VERIFY( ss.rdbuf()->get_allocator() == a2 );
    VERIFY( ss.str().get_allocator() == a2 );
  }

  // basic_istringstream(const basic_string<C,T,SA>&, openmode, const A&)
  {
    alloc_type a2(2);
    const std::string s2 = cstr;
    istringstream ss(s2, mode, a2);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a2 );
    VERIFY( ss.str().get_allocator() == a2 );
  }

  // basic_istringstream(const basic_string<C,T,SA>&, openmode = in)
  {
    alloc_type a0;
    const std::string s2 = cstr;
    istringstream ss(s2, mode);
    VERIFY( ss.str() == cstr );
    VERIFY( ss.rdbuf()->get_allocator() == a0 );
    VERIFY( ss.str().get_allocator() == a0 );
  }
}

// A minimal allocator with no default constructor
template<typename T>
  struct NoDefaultCons : __gnu_test::SimpleAllocator<T>
  {
    using __gnu_test::SimpleAllocator<T>::SimpleAllocator;

    NoDefaultCons() = delete;

    NoDefaultCons(int) { }
  };

void
test04()
{
  using sstream = std::basic_istringstream<char, std::char_traits<char>,
					   NoDefaultCons<char>>;

  NoDefaultCons<char> a(1);
  const std::string str(cstr);

  sstream ss1(str, a);
  VERIFY( ss1.str() == cstr );
  VERIFY( ss1.get() == cstr[0] );

  sstream ss2(str, std::ios::out, a);
  VERIFY( ss2.str() == cstr );
  VERIFY( ss2.get() == cstr[0] );

  sstream ss3(std::string(str), std::ios::out, a);
  VERIFY( ss3.str() == cstr );
  VERIFY( ss3.get() == cstr[0] );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
