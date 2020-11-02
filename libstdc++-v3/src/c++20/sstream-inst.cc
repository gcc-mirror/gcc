// Explicit instantiation file.

// Copyright (C) 2020 Free Software Foundation, Inc.
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

// Instantiations in this file are only for the new SSO std::string ABI
#include <sstream>

#if _GLIBCXX_USE_CXX11_ABI
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

template basic_stringbuf<char>::basic_stringbuf(const allocator_type&);
template basic_stringbuf<char>::basic_stringbuf(ios_base::openmode,
						const allocator_type&);
template basic_stringbuf<char>::basic_stringbuf(__string_type&&,
						ios_base::openmode);
template basic_stringbuf<char>::basic_stringbuf(basic_stringbuf&&,
						const allocator_type&);
template basic_stringbuf<char>::allocator_type
basic_stringbuf<char>::get_allocator() const noexcept;
template basic_stringbuf<char>::__sv_type
basic_stringbuf<char>::view() const noexcept;

template basic_istringstream<char>::basic_istringstream(ios_base::openmode,
							const allocator_type&);
template basic_istringstream<char>::basic_istringstream(__string_type&&,
							ios_base::openmode);
template string_view
basic_istringstream<char>::view() const noexcept;

template basic_ostringstream<char>::basic_ostringstream(ios_base::openmode,
							const allocator_type&);
template basic_ostringstream<char>::basic_ostringstream(__string_type&&,
							ios_base::openmode);
template string_view
basic_ostringstream<char>::view() const noexcept;

template basic_stringstream<char>::basic_stringstream(ios_base::openmode,
						      const allocator_type&);
template basic_stringstream<char>::basic_stringstream(__string_type&&,
						      ios_base::openmode);
template string_view
basic_stringstream<char>::view() const noexcept;

#ifdef _GLIBCXX_USE_WCHAR_T
using wsv_type = basic_string_view<wchar_t>;

template basic_stringbuf<wchar_t>::basic_stringbuf(const allocator_type&);
template basic_stringbuf<wchar_t>::basic_stringbuf(ios_base::openmode,
						   const allocator_type&);
template basic_stringbuf<wchar_t>::basic_stringbuf(__string_type&&,
						   ios_base::openmode);
template basic_stringbuf<wchar_t>::basic_stringbuf(basic_stringbuf&&,
						   const allocator_type&);
template basic_stringbuf<wchar_t>::allocator_type
basic_stringbuf<wchar_t>::get_allocator() const noexcept;

template basic_istringstream<wchar_t>::__stringbuf_type::__sv_type
basic_stringbuf<wchar_t>::view() const noexcept;

template basic_istringstream<wchar_t>::basic_istringstream(ios_base::openmode,
							   const allocator_type&);
template basic_istringstream<wchar_t>::basic_istringstream(__string_type&&,
							   ios_base::openmode);
template wstring_view
basic_istringstream<wchar_t>::view() const noexcept;

template basic_ostringstream<wchar_t>::basic_ostringstream(ios_base::openmode,
							   const allocator_type&);
template basic_ostringstream<wchar_t>::basic_ostringstream(__string_type&&,
							   ios_base::openmode);
template wstring_view
basic_ostringstream<wchar_t>::view() const noexcept;

template basic_stringstream<wchar_t>::basic_stringstream(ios_base::openmode,
							 const allocator_type&);
template basic_stringstream<wchar_t>::basic_stringstream(__string_type&&,
							 ios_base::openmode);
template wstring_view
basic_stringstream<wchar_t>::view() const noexcept;
#endif

_GLIBCXX_END_NAMESPACE_VERSION
}

#endif //_GLIBCXX_USE_CXX11_ABI
