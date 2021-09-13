// Explicit instantiation file.

// Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

//
// ISO C++ 14882:
//

#define _GLIBCXX_USE_CXX11_ABI 0
#include <fstream>

#if ! _GLIBCXX_USE_DUAL_ABI
# error This file should not be compiled for this configuration.
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template basic_filebuf<char>*
    basic_filebuf<char>::open(const std::string&, ios_base::openmode);
  template
    basic_ifstream<char>::
    basic_ifstream(const std::string&, ios_base::openmode);
  template void
    basic_ifstream<char>::open(const std::string&, ios_base::openmode);
  template
    basic_ofstream<char>::
    basic_ofstream(const std::string&, ios_base::openmode);
  template void
    basic_ofstream<char>::open(const std::string&, ios_base::openmode);
  template
    basic_fstream<char>::basic_fstream(const std::string&, ios_base::openmode);
  template void
    basic_fstream<char>::open(const std::string&, ios_base::openmode);

#ifdef _GLIBCXX_USE_WCHAR_T
  template basic_filebuf<wchar_t>*
    basic_filebuf<wchar_t>::open(const std::string&, ios_base::openmode);
  template
    basic_ifstream<wchar_t>::
    basic_ifstream(const std::string&, ios_base::openmode);
  template void
    basic_ifstream<wchar_t>::open(const std::string&, ios_base::openmode);
  template
    basic_ofstream<wchar_t>::
    basic_ofstream(const std::string&, ios_base::openmode);
  template void
    basic_ofstream<wchar_t>::open(const std::string&, ios_base::openmode);
  template
    basic_fstream<wchar_t>::
    basic_fstream(const std::string&, ios_base::openmode);
  template void
    basic_fstream<wchar_t>::open(const std::string&, ios_base::openmode);
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
