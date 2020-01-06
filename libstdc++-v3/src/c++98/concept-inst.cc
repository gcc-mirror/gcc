// Concept checking instantiations -*- C++ -*-

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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

// The implementation of some of the more complex checks uses the simple
// checks (good reuse of code), thereby requiring that the simple checks
// be instantiated somewhere.  The simple checks use other simple checks,
// and so on, until a couple hundred symbols all need instantiations.  We
// explicitly instantiate the initial set of symbols; compiling this file
// with -fimplicit-templates will take care of the rest for us.

#define _GLIBCXX_USE_CXX11_ABI 0
#include <bits/concept_check.h>

#ifdef _GLIBCXX_CONCEPT_CHECKS

#include <memory>
#include <iterator>
#include <ostream>

#define _Instantiate(...) template void __function_requires< __VA_ARGS__ > ()

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template void __aux_require_boolean_expr<bool>(bool const&);

  _Instantiate(_ConvertibleConcept<unsigned, unsigned> );

  _Instantiate(_InputIteratorConcept<char*> );

  _Instantiate(_InputIteratorConcept<char const*> );

#ifdef _GLIBCXX_USE_WCHAR_T
  _Instantiate(_InputIteratorConcept<wchar_t*> );

  _Instantiate(_InputIteratorConcept<wchar_t const*> );

  _Instantiate(_LessThanComparableConcept<wchar_t*> );
#endif

  _Instantiate(_LessThanComparableConcept<char*> );

  _Instantiate(_LessThanComparableConcept<int> );

  _Instantiate(_LessThanComparableConcept<long> );

  _Instantiate(_LessThanComparableConcept<long long> );

  _Instantiate(_LessThanComparableConcept<unsigned> );

  _Instantiate(_OutputIteratorConcept<std::ostreambuf_iterator<
    char, std::char_traits<char> >, char> );

#ifdef _GLIBCXX_USE_WCHAR_T
  _Instantiate(_OutputIteratorConcept<std::ostreambuf_iterator<
    wchar_t, std::char_traits<wchar_t> >, wchar_t> );
#endif

  _Instantiate(_RandomAccessIteratorConcept<char*> );

  _Instantiate(_RandomAccessIteratorConcept<char const*> );

  _Instantiate(_RandomAccessIteratorConcept<
    __normal_iterator<char const*, std::string> > );

  _Instantiate(_RandomAccessIteratorConcept<
    __normal_iterator<char*, std::string> > );

#ifdef _GLIBCXX_USE_WCHAR_T
  _Instantiate(_RandomAccessIteratorConcept<
    __normal_iterator<wchar_t const*,
    std::basic_string<wchar_t, std::char_traits<wchar_t>,
                               std::allocator<wchar_t> > > > );

  _Instantiate(_RandomAccessIteratorConcept<
    __normal_iterator<wchar_t*,
    std::basic_string<wchar_t, std::char_traits<wchar_t>,
                               std::allocator<wchar_t> > > > );

  _Instantiate(_RandomAccessIteratorConcept<wchar_t*> );

  _Instantiate(_RandomAccessIteratorConcept<wchar_t const*> );
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#undef _Instantiate

#endif
