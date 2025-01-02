// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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


// 27.6.2.3 class basic_ostream::sentry

#include <ostream>
#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/9562
void
test01()
{
  std::wstringbuf strbuf01;
  std::wostream strm1(&strbuf01);
  const std::wostream::sentry sentry1(strm1);

  VERIFY( bool(sentry1) == true );
}

int main() 
{
  test01();
  return 0;
}
