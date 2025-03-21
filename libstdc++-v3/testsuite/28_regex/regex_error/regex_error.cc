// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }
//
// 2009-06-17  Stephen M. Webb  <stephen.webb@xandros.com>
//
// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

// 28.6 [re.badexp]

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  std::regex_error error(std::regex_constants::error_collate);
  VERIFY(error.code() == std::regex_constants::error_collate);

  try
    {
      throw error;
    }
  catch (std::runtime_error& ex)
    {
    }
}

int main()
{
  test01();
  return 0;
}
