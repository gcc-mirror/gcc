// std::codecvt implementation details, GNU version -*- C++ -*-

// Copyright (C) 2002-2024 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.2.1.5 - Template class codecvt
//

// Written by Benjamin Kosnik <bkoz@redhat.com>

#include <locale>
#include <cstdlib>  // For MB_CUR_MAX
#include <climits>  // For MB_LEN_MAX
#include <bits/c++locale_internal.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Specializations.
#ifdef _GLIBCXX_USE_WCHAR_T
  codecvt_base::result
  codecvt<wchar_t, char, mbstate_t>::
  do_out(state_type& __state, const intern_type* __from,
	 const intern_type* __from_end, const intern_type*& __from_next,
	 extern_type* __to, extern_type* __to_end,
	 extern_type*& __to_next) const
  {
    result __ret = ok;
    state_type __tmp_state(__state);

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __c_locale __old = __uselocale(_M_c_locale_codecvt);
#endif

    // wcsnrtombs is *very* fast but stops if encounters NUL characters:
    // in case we fall back to wcrtomb and then continue, in a loop.
    // NB: wcsnrtombs is a GNU extension
    for (__from_next = __from, __to_next = __to;
	 __from_next < __from_end && __to_next < __to_end
	 && __ret == ok;)
      {
	const intern_type* __from_chunk_end = wmemchr(__from_next, L'\0',
						      __from_end - __from_next);
	if (!__from_chunk_end)
	  __from_chunk_end = __from_end;

	__from = __from_next;
	const size_t __conv = wcsnrtombs(__to_next, &__from_next,
					 __from_chunk_end - __from_next,
					 __to_end - __to_next, &__state);
	if (__conv == static_cast<size_t>(-1))
	  {
	    // In case of error, in order to stop at the exact place we
	    // have to start again from the beginning with a series of
	    // wcrtomb.
	    for (; __from < __from_next; ++__from)
	      __to_next += wcrtomb(__to_next, *__from, &__tmp_state);
	    __state = __tmp_state;
	    __ret = error;
	  }
	else if (__from_next && __from_next < __from_chunk_end)
	  {
	    __to_next += __conv;
	    __ret = partial;
	  }
	else
	  {
	    __from_next = __from_chunk_end;
	    __to_next += __conv;
	  }

	if (__from_next < __from_end && __ret == ok)
	  {
	    extern_type __buf[MB_LEN_MAX];
	    __tmp_state = __state;
	    const size_t __conv2 = wcrtomb(__buf, *__from_next, &__tmp_state);
	    if (__conv2 > static_cast<size_t>(__to_end - __to_next))
	      __ret = partial;
	    else
	      {
		memcpy(__to_next, __buf, __conv2);
		__state = __tmp_state;
		__to_next += __conv2;
		++__from_next;
	      }
	  }
      }

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __uselocale(__old);
#endif

    return __ret;
  }

  codecvt_base::result
  codecvt<wchar_t, char, mbstate_t>::
  do_in(state_type& __state, const extern_type* __from,
	const extern_type* __from_end, const extern_type*& __from_next,
	intern_type* __to, intern_type* __to_end,
	intern_type*& __to_next) const
  {
    result __ret = ok;
    state_type __tmp_state(__state);

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __c_locale __old = __uselocale(_M_c_locale_codecvt);
#endif

    // mbsnrtowcs is *very* fast but stops if encounters NUL characters:
    // in case we store a L'\0' and then continue, in a loop.
    // NB: mbsnrtowcs is a GNU extension
    for (__from_next = __from, __to_next = __to;
	 __from_next < __from_end && __to_next < __to_end
	 && __ret == ok;)
      {
	const extern_type* __from_chunk_end;
	__from_chunk_end = static_cast<const extern_type*>(memchr(__from_next, '\0',
								  __from_end
								  - __from_next));
	if (!__from_chunk_end)
	  __from_chunk_end = __from_end;

	__from = __from_next;
	size_t __conv = mbsnrtowcs(__to_next, &__from_next,
				   __from_chunk_end - __from_next,
				   __to_end - __to_next, &__state);
	if (__conv == static_cast<size_t>(-1))
	  {
	    // In case of error, in order to stop at the exact place we
	    // have to start again from the beginning with a series of
	    // mbrtowc.
	    for (;; ++__to_next, __from += __conv)
	      {
		__conv = mbrtowc(__to_next, __from, __from_end - __from,
				 &__tmp_state);
		if (__conv == static_cast<size_t>(-1)
		    || __conv == static_cast<size_t>(-2))
		  break;
	      }
	    __from_next = __from;
	    __state = __tmp_state;
	    __ret = error;
	  }
	else if (__from_next && __from_next < __from_chunk_end)
	  {
	    // It is unclear what to return in this case (see DR 382).
	    __to_next += __conv;
	    __ret = partial;
	  }
	else
	  {
	    __from_next = __from_chunk_end;
	    __to_next += __conv;
	  }

	if (__from_next < __from_end && __ret == ok)
	  {
	    if (__to_next < __to_end)
	      {
		// XXX Probably wrong for stateful encodings
		__tmp_state = __state;
		++__from_next;
		*__to_next++ = L'\0';
	      }
	    else
	      __ret = partial;
	  }
      }

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __uselocale(__old);
#endif

    return __ret;
  }

  int
  codecvt<wchar_t, char, mbstate_t>::
  do_encoding() const throw()
  {
    // XXX This implementation assumes that the encoding is
    // stateless and is either single-byte or variable-width.
    int __ret = 0;
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __c_locale __old = __uselocale(_M_c_locale_codecvt);
#endif
    if (MB_CUR_MAX == 1)
      __ret = 1;
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __uselocale(__old);
#endif
    return __ret;
  }

  int
  codecvt<wchar_t, char, mbstate_t>::
  do_max_length() const throw()
  {
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __c_locale __old = __uselocale(_M_c_locale_codecvt);
#endif
    // XXX Probably wrong for stateful encodings.
    int __ret = MB_CUR_MAX;
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __uselocale(__old);
#endif
    return __ret;
  }

  int
  codecvt<wchar_t, char, mbstate_t>::
  do_length(state_type& __state, const extern_type* __from,
	    const extern_type* __end, size_t __max) const
  {
    int __ret = 0;
    state_type __tmp_state(__state);

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __c_locale __old = __uselocale(_M_c_locale_codecvt);
#endif

    // mbsnrtowcs is *very* fast but stops if encounters NUL characters:
    // in case we advance past it and then continue, in a loop.
    // NB: mbsnrtowcs is a GNU extension

    // A dummy internal buffer is needed in order for mbsnrtocws to consider
    // its fourth parameter (it wouldn't with NULL as first parameter).
    wchar_t* __to = static_cast<wchar_t*>(__builtin_alloca(sizeof(wchar_t)
							   * __max));
    while (__from < __end && __max)
      {
	const extern_type* __from_chunk_end;
	__from_chunk_end = static_cast<const extern_type*>(memchr(__from, '\0',
								  __end
								  - __from));
	if (!__from_chunk_end)
	  __from_chunk_end = __end;

	const extern_type* __tmp_from = __from;
	size_t __conv = mbsnrtowcs(__to, &__from,
				   __from_chunk_end - __from,
				   __max, &__state);
	if (__conv == static_cast<size_t>(-1))
	  {
	    // In case of error, in order to stop at the exact place we
	    // have to start again from the beginning with a series of
	    // mbrtowc.
	    for (__from = __tmp_from;; __from += __conv)
	      {
		__conv = mbrtowc(0, __from, __end - __from,
				 &__tmp_state);
		if (__conv == static_cast<size_t>(-1)
		    || __conv == static_cast<size_t>(-2))
		  break;
	      }
	    __state = __tmp_state;
	    __ret += __from - __tmp_from;
	    break;
	  }
	if (!__from)
	  __from = __from_chunk_end;

	__ret += __from - __tmp_from;
	__max -= __conv;

	if (__from < __end && __max)
	  {
	    // XXX Probably wrong for stateful encodings
	    __tmp_state = __state;
	    ++__from;
	    ++__ret;
	    --__max;
	  }
      }

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
    __uselocale(__old);
#endif

    return __ret;
  }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
