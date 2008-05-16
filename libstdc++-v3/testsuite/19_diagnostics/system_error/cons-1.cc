// { dg-options "-std=gnu++0x" }
// 2007-06-05 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <cstring>
#include <system_error>
#include <testsuite_hooks.h>

int main()
{
  bool test __attribute__((unused)) = true;
  const std::string s("too late: boulangerie out of pain au raisin");
  const std::error_code e(std::posix_error::operation_not_supported);

  // 1
  {
    std::system_error err1(e, s);
    VERIFY( err1.code() == e ); 
    VERIFY( std::strcmp(err1.runtime_error::what(), s.c_str()) == 0 );
  }

  // 2
  {
    std::system_error err2(95, std::system_category, s);
    VERIFY( err2.code() == std::error_code(95, std::system_category) ); 
    VERIFY( std::strcmp(err2.runtime_error::what(), s.c_str()) == 0 );
  }

  return 0;
}
