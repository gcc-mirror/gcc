// 1999-08-24 bkoz

// Copyright (C) 2000, 1999 Free Software Foundation
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 22.2.1 the ctype category

// 1: Test that the locale headers are picking up the correct declaration
// of the internal type `ctype_base::mask'.
int mask ();

#include <locale>

// 2: Should be able to instantiate this for other types besides char, wchar_t
class gnu_ctype: public std::ctype<unsigned char> { };
gnu_ctype facet01;


int main() { }
