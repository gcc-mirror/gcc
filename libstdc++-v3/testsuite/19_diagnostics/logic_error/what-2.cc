// 2001-02-26 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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

// 19.1 Exception classes

#include <string>
#include <stdexcept>
#include <cstring>
#include <testsuite_hooks.h>

// libstdc++/2089
class fuzzy_logic : public std::logic_error
{
public:
  fuzzy_logic() : std::logic_error("whoa") { }
};

void test03()
{
  try
    { throw fuzzy_logic(); }
  catch(const fuzzy_logic& obj)
    { VERIFY( std::strcmp("whoa", obj.what()) == 0 ); }
  catch(...)
    { VERIFY( false ); }
}

int main(void)
{
  test03();
  return 0;
}
