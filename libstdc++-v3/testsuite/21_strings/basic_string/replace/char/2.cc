// 1999-06-10 bkoz

// Copyright (C) 1994, 1999, 2001, 2002, 2003 Free Software Foundation, Inc.
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

// 21.3.5.6 basic_string::replace

#include <string>
#include <testsuite_hooks.h>

void
test02()
{
  bool test __attribute__((unused)) = true;
  const char* strlit = "../the long pier/Hanalei Bay/Kauai/Hawaii";
  std::string aux = strlit;
  aux.replace(aux.begin()+5, aux.begin()+20,
	      aux.begin()+10, aux.begin()+15);
  VERIFY(aux == "../thg piealei Bay/Kauai/Hawaii");
  
  aux = strlit;
  aux.replace(aux.begin() + 10, aux.begin() + 15,
	      aux.begin() + 5, aux.begin() + 20);
  VERIFY(aux == "../the lone long pier/Hanr/Hanalei Bay/Kauai/Hawaii");
}

int main()
{ 
  test02();
  return 0;
}
