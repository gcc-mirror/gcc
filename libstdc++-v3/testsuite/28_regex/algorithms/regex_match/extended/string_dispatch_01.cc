// { dg-options "-std=gnu++11" }

//
// 2013-07-29  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.11.2 regex_match
// Tests Extended automatic matcher dispatching against a std::string target.

#include <regex>
#include <testsuite_hooks.h>

using namespace std;

template<typename _Bi_iter, typename _Alloc,
         typename _Ch_type, typename _Rx_traits>
  void
  fake_match(_Bi_iter                                 __s,
             _Bi_iter                                 __e,
             match_results<_Bi_iter, _Alloc>&         __m,
             const basic_regex<_Ch_type, _Rx_traits>& __re,
             regex_constants::match_flag_type         __flags
                            = regex_constants::match_default)
  {
    VERIFY( (dynamic_cast
             <__detail::_DFSExecutor<_Bi_iter, _Alloc, _Ch_type, _Rx_traits>*>
             (&*__detail::__get_executor(__s, __e, __m, __re, __flags))
             != nullptr) );
  }

void
test01()
{
  bool test __attribute__((unused)) = true;

  regex  re("()(one(.*))abc\\1"); // backref cause DFS
  const string target("onetwoabc");
  smatch m;
  fake_match(target.begin(), target.end(), m, re);

  regex_match(target, m, re);
  VERIFY( m[2].matched );
  VERIFY( m[3].matched );
  VERIFY( std::string(m[2].first, m[2].second) == "onetwo" );
  VERIFY( std::string(m[3].first, m[3].second) == "two" );
}

int
main()
{
  test01();
  return 0;
}
