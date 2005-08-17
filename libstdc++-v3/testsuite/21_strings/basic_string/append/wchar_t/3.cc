// 2004-25-10  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation, Inc.
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

// 21.3.5 string modifiers

#include <string>
#include <testsuite_hooks.h>

// Upon reallocation (basic_string::reserve) we were copying from
// deallocated memory.
void
test03()
{
  bool test __attribute__((unused)) = true;

  using namespace std;
 
  const wchar_t * source = L"Kesto";

  for (unsigned i = 0; i < 10; ++i)
    {
      wstring one(source);
      wstring two(source);
      for (unsigned j = 0; j < 18; ++j)
	{
	  VERIFY( one == two );
	  one.append(one);
	  one += L'x';
	  two.append(two.c_str(), two.size());
	  two += L'x';
	}
    }
}

int main()
{ 
  test03();
  return 0;
}
