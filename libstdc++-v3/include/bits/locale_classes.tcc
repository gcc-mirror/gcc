// Locale support -*- C++ -*-

// Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

/** @file bits/locale_classes.tcc
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{locale}
 */

//
// ISO C++ 14882: 22.1  Locales
//

#ifndef _LOCALE_CLASSES_TCC
#define _LOCALE_CLASSES_TCC 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <cerrno>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++11-extensions" // extern template
#pragma GCC diagnostic ignored "-Wvariadic-macros"

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _Facet>
    locale::
    locale(const locale& __other, _Facet* __f)
    {
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 2295. Locale name when the provided Facet is a nullptr
      if (__builtin_expect(!__f, 0))
	{
	  _M_impl = __other._M_impl;
	  _M_impl->_M_add_reference();
	  return;
	}

      _M_impl = new _Impl(*__other._M_impl, 1);

      __try
	{ _M_impl->_M_install_facet(&_Facet::id, __f); }
      __catch(...)
	{
	  _M_impl->_M_remove_reference();
	  __throw_exception_again;
	}
      delete [] _M_impl->_M_names[0];
      _M_impl->_M_names[0] = 0;   // Unnamed.
    }

  template<typename _Facet>
    locale
    locale::
    combine(const locale& __other) const
    {
#if __cpp_lib_type_trait_variable_templates // C++ >= 17
      static_assert(__is_facet<_Facet>, "Template argument must be a facet");
#endif

      _Impl* __tmp = new _Impl(*_M_impl, 1);
      __try
	{
	  __tmp->_M_replace_facet(__other._M_impl, &_Facet::id);
	}
      __catch(...)
	{
	  __tmp->_M_remove_reference();
	  __throw_exception_again;
	}
      delete[] __tmp->_M_names[0];
      __tmp->_M_names[0] = 0;   // Unnamed.
      return locale(__tmp);
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    bool
    locale::
    operator()(const basic_string<_CharT, _Traits, _Alloc>& __s1,
	       const basic_string<_CharT, _Traits, _Alloc>& __s2) const
    {
      typedef std::collate<_CharT> __collate_type;
      const __collate_type& __collate = use_facet<__collate_type>(*this);
      return (__collate.compare(__s1.data(), __s1.data() + __s1.length(),
				__s2.data(), __s2.data() + __s2.length()) < 0);
    }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions"
  template<typename _Facet>
    inline const _Facet*
    __try_use_facet(const locale& __loc) _GLIBCXX_NOTHROW
    {
      const size_t __i = _Facet::id._M_id();
      const locale::facet** __facets = __loc._M_impl->_M_facets;

      // We know these standard facets are always installed in every locale
      // so dynamic_cast always succeeds, just use static_cast instead.
#define _GLIBCXX_STD_FACET(...) \
      if _GLIBCXX_CONSTEXPR (__is_same(const _Facet, const __VA_ARGS__)) \
	return static_cast<const _Facet*>(__facets[__i])

      _GLIBCXX_STD_FACET(ctype<char>);
      _GLIBCXX_STD_FACET(num_get<char>);
      _GLIBCXX_STD_FACET(num_put<char>);
      _GLIBCXX_STD_FACET(codecvt<char, char, mbstate_t>);
      _GLIBCXX_STD_FACET(collate<char>);
      _GLIBCXX_STD_FACET(moneypunct<char>);
      _GLIBCXX_STD_FACET(moneypunct<char, true>);
      _GLIBCXX_STD_FACET(money_get<char>);
      _GLIBCXX_STD_FACET(money_put<char>);
      _GLIBCXX_STD_FACET(numpunct<char>);
      _GLIBCXX_STD_FACET(time_get<char>);
      _GLIBCXX_STD_FACET(time_put<char>);
      _GLIBCXX_STD_FACET(messages<char>);

#ifdef _GLIBCXX_USE_WCHAR_T
      _GLIBCXX_STD_FACET(ctype<wchar_t>);
      _GLIBCXX_STD_FACET(num_get<wchar_t>);
      _GLIBCXX_STD_FACET(num_put<wchar_t>);
      _GLIBCXX_STD_FACET(codecvt<wchar_t, char, mbstate_t>);
      _GLIBCXX_STD_FACET(collate<wchar_t>);
      _GLIBCXX_STD_FACET(moneypunct<wchar_t>);
      _GLIBCXX_STD_FACET(moneypunct<wchar_t, true>);
      _GLIBCXX_STD_FACET(money_get<wchar_t>);
      _GLIBCXX_STD_FACET(money_put<wchar_t>);
      _GLIBCXX_STD_FACET(numpunct<wchar_t>);
      _GLIBCXX_STD_FACET(time_get<wchar_t>);
      _GLIBCXX_STD_FACET(time_put<wchar_t>);
      _GLIBCXX_STD_FACET(messages<wchar_t>);
#endif
#if __cplusplus >= 201103L
      _GLIBCXX_STD_FACET(codecvt<char16_t, char, mbstate_t>);
      _GLIBCXX_STD_FACET(codecvt<char32_t, char, mbstate_t>);
#endif

#undef _GLIBCXX_STD_FACET

      if (__i >= __loc._M_impl->_M_facets_size || !__facets[__i])
	return 0;

#if __cpp_rtti
      return dynamic_cast<const _Facet*>(__facets[__i]);
#else
      return static_cast<const _Facet*>(__facets[__i]);
#endif
    }
#pragma GCC diagnostic pop

  /**
   *  @brief  Test for the presence of a facet.
   *  @ingroup locales
   *
   *  has_facet tests the locale argument for the presence of the facet type
   *  provided as the template parameter.  Facets derived from the facet
   *  parameter will also return true.
   *
   *  @tparam  _Facet  The facet type to test the presence of.
   *  @param  __loc  The locale to test.
   *  @return  true if @p __loc contains a facet of type _Facet, else false.
  */
  template<typename _Facet>
    _GLIBCXX_NODISCARD
    inline bool
    has_facet(const locale& __loc) _GLIBCXX_USE_NOEXCEPT
    {
#if __cplusplus >= 201103L
      static_assert(__is_base_of(locale::facet, _Facet),
		    "template argument must be derived from locale::facet");
#else
      (void) static_cast<const _Facet*>(static_cast<const locale::facet*>(0));
#endif
      return std::__try_use_facet<_Facet>(__loc) != 0;
    }

  /**
   *  @brief  Return a facet.
   *  @ingroup locales
   *
   *  use_facet looks for and returns a reference to a facet of type Facet
   *  where Facet is the template parameter.  If has_facet(locale) is true,
   *  there is a suitable facet to return.  It throws std::bad_cast if the
   *  locale doesn't contain a facet of type Facet.
   *
   *  @tparam  _Facet  The facet type to access.
   *  @param  __loc  The locale to use.
   *  @return  Reference to facet of type Facet.
   *  @throw  std::bad_cast if @p __loc doesn't contain a facet of type _Facet.
  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdangling-reference"
  template<typename _Facet>
    _GLIBCXX_NODISCARD
    inline const _Facet&
    use_facet(const locale& __loc)
    {
#if __cplusplus >= 201103L
      static_assert(__is_base_of(locale::facet, _Facet),
		    "template argument must be derived from locale::facet");
#else
      (void) static_cast<const _Facet*>(static_cast<const locale::facet*>(0));
#endif
      if (const _Facet* __f = std::__try_use_facet<_Facet>(__loc))
	return *__f;
      __throw_bad_cast();
    }
#pragma GCC diagnostic pop


  // Generic version does nothing.
  template<typename _CharT>
    int
    collate<_CharT>::_M_compare(const _CharT*, const _CharT*) const throw ()
    { return 0; }

  // Generic version does nothing.
  template<typename _CharT>
    size_t
    collate<_CharT>::_M_transform(_CharT*, const _CharT*, size_t) const throw ()
    { return 0; }

  template<typename _CharT>
    int
    collate<_CharT>::
    do_compare(const _CharT* __lo1, const _CharT* __hi1,
	       const _CharT* __lo2, const _CharT* __hi2) const
    {
      // strcoll assumes zero-terminated strings so we make a copy
      // and then put a zero at the end.
      const string_type __one(__lo1, __hi1);
      const string_type __two(__lo2, __hi2);

      const _CharT* __p = __one.c_str();
      const _CharT* __pend = __one.data() + __one.length();
      const _CharT* __q = __two.c_str();
      const _CharT* __qend = __two.data() + __two.length();

      // strcoll stops when it sees a nul character so we break
      // the strings into zero-terminated substrings and pass those
      // to strcoll.
      for (;;)
	{
	  const int __res = _M_compare(__p, __q);
	  if (__res)
	    return __res;

	  __p += char_traits<_CharT>::length(__p);
	  __q += char_traits<_CharT>::length(__q);
	  if (__p == __pend && __q == __qend)
	    return 0;
	  else if (__p == __pend)
	    return -1;
	  else if (__q == __qend)
	    return 1;

	  __p++;
	  __q++;
	}
    }

  template<typename _CharT>
    typename collate<_CharT>::string_type
    collate<_CharT>::
    do_transform(const _CharT* __lo, const _CharT* __hi) const
    {
      string_type __ret;

      // strxfrm assumes zero-terminated strings so we make a copy
      const string_type __str(__lo, __hi);

      const _CharT* __p = __str.c_str();
      const _CharT* __pend = __str.data() + __str.length();

      size_t __len = (__hi - __lo) * 2;

      struct _Buf
      {
	_Buf(size_t __n, void* __buf, int __e)
	: _M_c(__buf ? (_CharT*)__buf : new _CharT[__n]),
	  _M_stackbuf(__buf),
	  _M_errno(__e)
	{ }

	~_Buf()
	{
	  if (_M_c != _M_stackbuf)
	    delete[] _M_c;
	  if (errno == 0)
	    errno = _M_errno;
	}

	void _M_realloc(size_t __len)
	{
	  _CharT* __p = new _CharT[__len];
	  if (_M_c != _M_stackbuf)
	    delete[] _M_c;
	  _M_c = __p;
	}

	_CharT* _M_c;
	void* const _M_stackbuf;
	int _M_errno;
      };

      const size_t __bytes = __len * sizeof(_CharT);
      _Buf __buf(__len, __bytes <= 256 ? __builtin_alloca(__bytes) : 0, errno);
      errno = 0;

      // strxfrm stops when it sees a nul character so we break
      // the string into zero-terminated substrings and pass those
      // to strxfrm.
      for (;;)
	{
	  // First try a buffer perhaps big enough.
	  size_t __res = _M_transform(__buf._M_c, __p, __len);
	  // If the buffer was not large enough, try again with the
	  // correct size.
	  if (__res >= __len)
	    {
	      if (__builtin_expect(errno, 0))
		{
#if __cpp_exceptions
		  __throw_system_error(errno);
#else
		  // std::regex can call this function internally with
		  // char values that always fail, so we don't want to
		  // use _GLIBCXX_THROW_OR_ABORT here.
		  __ret.clear();
		  break;
#endif
		}

	      __len = __res + 1;
	      __buf._M_realloc(__len);
	      __res = _M_transform(__buf._M_c, __p, __len);
	    }

	  __ret.append(__buf._M_c, __res);
	  __p += char_traits<_CharT>::length(__p);
	  if (__p == __pend)
	    break;

	  __p++;
	  __ret.push_back(_CharT());
	}

      return __ret;
    }

  template<typename _CharT>
    long
    collate<_CharT>::
    do_hash(const _CharT* __lo, const _CharT* __hi) const
    {
      unsigned long __val = 0;
      for (; __lo < __hi; ++__lo)
	__val =
	  *__lo + ((__val << 7)
		   | (__val >> (__gnu_cxx::__numeric_traits<unsigned long>::
				__digits - 7)));
      return static_cast<long>(__val);
    }

  // Inhibit implicit instantiations for required instantiations,
  // which are defined via explicit instantiations elsewhere.
#if _GLIBCXX_EXTERN_TEMPLATE
  extern template class collate<char>;
  extern template class collate_byname<char>;

  extern template
    const collate<char>*
    __try_use_facet<collate<char> >(const locale&) _GLIBCXX_NOTHROW;

  extern template
    const collate<char>&
    use_facet<collate<char> >(const locale&);

  extern template
    bool
    has_facet<collate<char> >(const locale&);

#ifdef _GLIBCXX_USE_WCHAR_T
  extern template class collate<wchar_t>;
  extern template class collate_byname<wchar_t>;

  extern template
    const collate<wchar_t>*
    __try_use_facet<collate<wchar_t> >(const locale&) _GLIBCXX_NOTHROW;

  extern template
    const collate<wchar_t>&
    use_facet<collate<wchar_t> >(const locale&);

  extern template
    bool
    has_facet<collate<wchar_t> >(const locale&);
#endif
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#pragma GCC diagnostic pop
#endif
