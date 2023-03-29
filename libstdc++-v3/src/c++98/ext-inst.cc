// Explicit instantiation file.

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

#include <ext/rope>

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  namespace
  {
    const int min_len = __detail::_S_max_rope_depth + 1;
  }

  template
    const unsigned long
    rope<char, std::allocator<char> >::_S_min_len[min_len];

  template
    char
    rope<char, std::allocator<char> >::
    _S_fetch(_Rope_RopeRep<char, std::allocator<char> >*, size_type);

#ifdef _GLIBCXX_USE_WCHAR_T
  template
    const unsigned long
    rope<wchar_t, std::allocator<wchar_t> >::_S_min_len[min_len];

  template
    wchar_t
    rope<wchar_t, std::allocator<wchar_t> >::
    _S_fetch(_Rope_RopeRep<wchar_t, std::allocator<wchar_t> >*, size_type);
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
