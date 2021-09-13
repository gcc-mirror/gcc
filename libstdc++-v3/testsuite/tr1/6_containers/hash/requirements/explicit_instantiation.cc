// { dg-do compile }

// 2005-02-17  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 6.3.3 class template hash

#include <string>
#include <tr1/functional>

// Verify that we can instantiate hash for every required type.
template class std::tr1::hash<bool>;
template class std::tr1::hash<char>;
template class std::tr1::hash<signed char>;
template class std::tr1::hash<unsigned char>;
template class std::tr1::hash<short>;
template class std::tr1::hash<int>;
template class std::tr1::hash<long>;
template class std::tr1::hash<unsigned short>;
template class std::tr1::hash<unsigned int>;
template class std::tr1::hash<unsigned long>;
template class std::tr1::hash<float>;
template class std::tr1::hash<double>;
template class std::tr1::hash<long double>;
template class std::tr1::hash<void*>;
template class std::tr1::hash<std::string>;

#ifdef _GLIBCXX_USE_WCHAR_T
template class std::tr1::hash<wchar_t>;
template class std::tr1::hash<std::wstring>;
#endif

