// { dg-do compile }

// 2005-2-17  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 6.3.3 class template hash

#include <string>
#include <tr1/functional>

int main()
{
  using namespace std::tr1;

  // Verify that we can instantiate hash for every required type.

  hash<bool> hb;
  hash<char> hc;
  hash<signed char> hsc;
  hash<unsigned char> huc;
  hash<short> hs;
  hash<int> hi;
  hash<long> hl;
  hash<unsigned short> hus;
  hash<unsigned int> hui;
  hash<unsigned long> hul;
  hash<float> hf;
  hash<double> hd;
  hash<long double> hld;
  hash<void*> hp;
  hash<std::string> hstr;

#ifdef _GLIBCXX_USE_WCHAR_T
  hash<wchar_t> hw;
  hash<std::wstring> hwstr;
#endif
}
