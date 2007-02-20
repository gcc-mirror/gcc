// { dg-do compile }

// 2005-02-17  Matt Austern  <austern@apple.com>
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 6.3.3 class template hash

#include <string>
#include <tr1/functional>

using namespace std::tr1;

// Verify that we can instantiate hash for every required type.
template class hash<bool>;
template class hash<char>;
template class hash<signed char>;
template class hash<unsigned char>;
template class hash<short>;
template class hash<int>;
template class hash<long>;
template class hash<unsigned short>;
template class hash<unsigned int>;
template class hash<unsigned long>;
template class hash<float>;
template class hash<double>;
template class hash<long double>;
template class hash<void*>;
template class hash<std::string>;

#ifdef _GLIBCXX_USE_WCHAR_T
template class hash<wchar_t>;
template class hash<std::wstring>;
#endif

