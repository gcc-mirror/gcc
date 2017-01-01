// { dg-do run { target c++11 } }
// 2007-06-05 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2007-2017 Free Software Foundation, Inc.
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
#include <system_error>
#include <testsuite_hooks.h>

int main()
{
  const std::string s("too late: boulangerie out of pain au raisin");
  const std::error_code
    e(std::make_error_code(std::errc::operation_not_supported));

  // 1
  {
    std::system_error err1(e, s);
    VERIFY( err1.code() == e ); 
    VERIFY( std::string(err1.what()).find(s) != std::string::npos );
  }

  // 2
  {
    std::system_error err2(95, std::system_category(), s);
    VERIFY( err2.code() == std::error_code(95, std::system_category()) ); 
    VERIFY( std::string((err2.what(), s)).find(s) != std::string::npos );
  }

  return 0;
}
