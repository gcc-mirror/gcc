// Locale support -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// Warning: this file is not meant for user inclusion.  Use <locale>.

#ifndef _CPP_BITS_LOCFACETS_TCC
#define _CPP_BITS_LOCFACETS_TCC 1

#include <bits/std_cerrno.h>
#include <bits/std_clocale.h>   // For localeconv
#include <bits/std_cstdlib.h>   // For strof, strtold
#include <bits/std_limits.h>    // For numeric_limits
#include <bits/std_memory.h>    // For auto_ptr
#include <bits/streambuf_iterator.h>     // For streambuf_iterators
#include <bits/std_cctype.h>    // For isspace
#include <typeinfo> 		// For bad_cast
#include <bits/std_vector.h>	

namespace std
{
  template<typename _Facet>
    locale
    locale::combine(const locale& __other)
    {
      _Impl* __tmp = new _Impl(*_M_impl, 1);
      __tmp->_M_replace_facet(__other._M_impl, &_Facet::id);
      return locale(__tmp);
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    bool
    locale::operator()(const basic_string<_CharT, _Traits, _Alloc>& __s1,
                       const basic_string<_CharT, _Traits, _Alloc>& __s2) const
    {
      typedef std::collate<_CharT> __collate_type;
      const __collate_type* __fcoll = &use_facet<__collate_type>(*this);
      return (__fcoll->compare(__s1.data(), __s1.data() + __s1.length(),
                               __s2.data(), __s2.data() + __s2.length()) < 0);
    }

  template<typename _Facet>
    const _Facet&
    use_facet(const locale& __loc)
    {
      typedef locale::_Impl::__vec_facet        __vec_facet;
      size_t __i = _Facet::id._M_index;
      __vec_facet* __facet = __loc._M_impl->_M_facets;
      const locale::facet* __fp = (*__facet)[__i]; 
      if (__fp == 0 || __i >= __facet->size())
        __throw_bad_cast();
      return static_cast<const _Facet&>(*__fp);
    }

  template<typename _Facet>
    bool
    has_facet(const locale& __loc) throw()
    {
      typedef locale::_Impl::__vec_facet        __vec_facet;
      size_t __i = _Facet::id._M_index;
      __vec_facet* __facet = __loc._M_impl->_M_facets;
      return (__i < __facet->size() && (*__facet)[__i] != 0);
    }


  // This member function takes an (w)istreambuf_iterator object and
  // parses it into a generic char array suitable for parsing with
  // strto[l,ll,f,d]. The thought was to encapsulate the conversion
  // into this one function, and thus the num_get::do_get member
  // functions can just adjust for the type of the overloaded
  // argument and process the char array returned from _M_extract.
  // Other things were also considered, including a fused
  // multiply-add loop that would obviate the need for any call to
  // strto... at all: however, it would b e a bit of a pain, because
  // you'd have to be able to return either floating or integral
  // types, etc etc. The current approach seems to be smack dab in
  // the middle between an unoptimized approach using sscanf, and
  // some kind of hyper-optimized approach alluded to above.

  // XXX
  // Need to do partial specialization to account for differences
  // between character sets. For char, this is pretty
  // straightforward, but for wchar_t, the conversion to a plain-jane
  // char type is a bit more involved.
  template<typename _CharT, typename _InIter>
    void
    num_get<_CharT, _InIter>::
    _M_extract(_InIter /*__beg*/, _InIter /*__end*/, ios_base& /*__io*/,
               ios_base::iostate& /*__err*/, char* /*__xtrc*/,
               int& /*__base*/, bool /*__fp*/) const
    {
      // XXX Not currently done: need to expand upon char version below.
    }

  template<>
    void
    num_get<char, istreambuf_iterator<char> >::
    _M_extract(istreambuf_iterator<char> __beg, 
	       istreambuf_iterator<char> __end, ios_base& __io, 
	       ios_base::iostate& __err, char* __xtrc, int& __base, 
	       bool __fp) const;

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
  // NB: This is an unresolved library defect #17
  //17.  Bad bool parsing
  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, bool& __v) const
    {
      // Parse bool values as long
      if (!(__io.flags() & ios_base::boolalpha))
        {
          // NB: We can't just call do_get(long) here, as it might
          // refer to a derived class.

          // Stage 1: extract and determine the conversion specifier.
          // Assuming leading zeros eliminated, thus the size of 32 for
          // integral types.
          char __xtrc[32] = {'\0'};
          int __base;
          _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

          // Stage 2: convert and store results.
          char* __sanity;
          errno = 0;
          long __l = strtol(__xtrc, &__sanity, __base);
          if (!(__err & ios_base::failbit)
              && __l <= 1
              && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
            __v = __l;
          else
            __err |= ios_base::failbit;
        }

      // Parse bool values as alphanumeric
      else
        {
          typedef _Format_cache<char_type> __fcache_type;
          __fcache_type* __fmt = __fcache_type::_S_get(__io);
          const char_type* __true = __fmt->_M_truename.c_str();
          const char_type* __false = __fmt->_M_falsename.c_str();
          const size_t __truelen =  __traits_type::length(__true) - 1;
          const size_t __falselen =  __traits_type::length(__false) - 1;

          for (size_t __pos = 0; __beg != __end; ++__pos)
            {
              char_type __c = *__beg++;
              bool __testf = __c == __false[__pos];
              bool __testt = __c == __true[__pos];
              if (!(__testf || __testt))
                {
                  __err |= ios_base::failbit;
                  break;
                }
              else if (__testf && __pos == __falselen)
                {
                  __v = 0;
                  break;
                }
              else if (__testt && __pos == __truelen)
                {
                  __v = 1;
                  break;
                }
            }
          if (__beg == __end)
            __err |= ios_base::eofbit;
        }

      return __beg;
    }
#endif

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
  //XXX.  Which number?  Presumably same as in locale_facets.h...
  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, short& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      long __l = strtol(__xtrc, &__sanity, __base);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0
          && __l >= SHRT_MIN && __l <= SHRT_MAX)
        __v = static_cast<short>(__l);
      else
        __err |= ios_base::failbit;

      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, int& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32] = {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      long __l = strtol(__xtrc, &__sanity, __base);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0
          && __l >= INT_MIN && __l <= INT_MAX)
        __v = static_cast<int>(__l);
      else
        __err |= ios_base::failbit;

      return __beg;
    }
#endif

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, long& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      long __l = strtol(__xtrc, &__sanity, __base);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
        __v = __l;
      else
        __err |= ios_base::failbit;

      return __beg;
    }

#ifdef _GLIBCPP_USE_LONG_LONG
  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, long long& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      long long __ll = strtoll(__xtrc, &__sanity, __base);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
        __v = __ll;
      else
        __err |= ios_base::failbit;

      return __beg;
    }
#endif

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, unsigned short& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      unsigned long __ul = strtoul(__xtrc, &__sanity, __base);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0
          && __ul <= USHRT_MAX)
        __v = static_cast<unsigned short>(__ul);
      else
        __err |= ios_base::failbit;

      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, unsigned int& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      unsigned long __ul = strtoul(__xtrc, &__sanity, __base);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0
          && __ul <= UINT_MAX)
        __v = static_cast<unsigned int>(__ul);
      else
        __err |= ios_base::failbit;

      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, unsigned long& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32] = {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      unsigned long __ul = strtoul(__xtrc, &__sanity, __base);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
        __v = __ul;
      else
        __err |= ios_base::failbit;

      return __beg;
    }

#ifdef _GLIBCPP_USE_LONG_LONG
  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, unsigned long long& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      unsigned long long __ull = strtoull(__xtrc, &__sanity, __base);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
        __v = __ull;
      else
        __err |= ios_base::failbit;

      return __beg;
    }
#endif

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, float& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 256 for
      // floating-point types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, true);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
#ifdef _GLIBCPP_USE_C99
      float __f = strtof(__xtrc, &__sanity);
#else
      float __f = static_cast<float>(strtod(__xtrc, &__sanity));
#endif
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
        __v = __f;
      else
        __err |= ios_base::failbit;

      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, double& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 256 for
      // floating-point types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, true);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      double __d = strtod(__xtrc, &__sanity);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
        __v = __d;
      else
        __err |= ios_base::failbit;

      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, long double& __v) const
    {
      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 256 for
      // floating-point types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, true);

#if defined(_GLIBCPP_USE_C99) && !defined(__hpux)
      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      long double __ld = strtold(__xtrc, &__sanity);
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
        __v = __ld;
#else
      // Stage 2: determine a conversion specifier.
      ios_base::fmtflags __basefield = __io.flags() & ios_base::basefield;
      const char* __conv;
      if (__basefield == ios_base::oct)
        __conv = "%Lo";
      else if (__basefield == ios_base::hex)
        __conv = "%LX";
      else if (__basefield == 0)
        __conv = "%Li";
      else
        __conv = "%Lf";

      // Stage 3: store results.
      typedef typename __traits_type::int_type int_type;
      long double __ld;
      int __p = sscanf(__xtrc, __conv, &__ld);
      if (!(__err & ios_base::failbit) && __p 
	  && static_cast<int_type>(__p) != __traits_type::eof())
        __v = __ld;
#endif
      else
        __err |= ios_base::failbit;

      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    num_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, ios_base& __io,
           ios_base::iostate& __err, void*& __v) const
    {
      // Prepare for hex formatted input
      typedef ios_base::fmtflags        fmtflags;
      fmtflags __fmt = __io.flags();
      fmtflags __fmtmask = ~(ios_base::showpos | ios_base::basefield
                             | ios_base::uppercase | ios_base::internal);
      __io.flags(__fmt & __fmtmask | (ios_base::hex | ios_base::showbase));

      // Stage 1: extract and determine the conversion specifier.
      // Assuming leading zeros eliminated, thus the size of 32 for
      // integral types.
      char __xtrc[32]= {'\0'};
      int __base;
      _M_extract(__beg, __end, __io, __err, __xtrc, __base, false);

      // Stage 2: convert and store results.
      char* __sanity;
      errno = 0;
      void* __vp = reinterpret_cast<void*>(strtoul(__xtrc, &__sanity, __base));
      if (!(__err & ios_base::failbit)
          && __sanity != __xtrc && *__sanity == '\0' && errno == 0)
        __v = __vp;
      else
        __err |= ios_base::failbit;

      // Reset from hex formatted input
      __io.flags(__fmt);
      return __beg;
    }

  // __pad is specialized for ostreambuf_iterator, random access iterator.
  template<typename _CharT, typename _OutIter>
    inline _OutIter
    __pad(_OutIter __s, _CharT __fill, int __padding);

  template<typename _CharT, typename _RaIter>
    _RaIter
    __pad(_RaIter __s, _CharT __fill, int __padding, 
	  random_access_iterator_tag)
    {
      fill_n(__s, __fill);
      return __s + __padding;
    }

  template<typename _CharT, typename _OutIter, typename _Tag>
    _OutIter
    __pad(_OutIter __s, _CharT __fill, int __padding, _Tag)
    {
      while (--__padding >= 0) { *__s = __fill; ++__s; }
      return __s;
    }

  template<typename _CharT, typename _OutIter>
    inline _OutIter
    __pad(_OutIter __s, _CharT __fill, int __padding)
    {
      return __pad(__s, __fill, __padding, 
		   typename iterator_traits<_OutIter>::iterator_category());
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    __pad_numeric(_OutIter __s, ios_base::fmtflags /*__flags*/,
		  _CharT /*__fill*/, int /*__width*/, 
		  _CharT const* /*__first*/, _CharT const* /*__middle*/, 
		  _CharT const* /*__last*/)
  {
      // XXX Not currently done: non streambuf_iterator
      return __s;
    }

  // Partial specialization for ostreambuf_iterator.
  template<typename _CharT>   
    ostreambuf_iterator<_CharT>
    __pad_numeric(ostreambuf_iterator<_CharT> __s, ios_base::fmtflags __flags,
		  _CharT __fill, int __width, _CharT const* __first,
		  _CharT const* __middle, _CharT const* __last)
    {
      typedef ostreambuf_iterator<_CharT> 	__out_iter;
      int __padding = __width - (__last - __first);
      if (__padding < 0)
        __padding = 0;
      ios_base::fmtflags __aflags = __flags & ios_base::adjustfield;
      bool __testfield = __padding == 0 || __aflags == ios_base::left
                         || __aflags == ios_base::internal;

      // This was needlessly complicated.
      if (__first != __middle)
        {
          if (!__testfield)
            {
              __pad(__s, __fill, __padding);
              __padding = 0;
            }
          copy(__first, __middle, __s);
        }
      __out_iter __s2 = __s;

      if (__padding && __aflags != ios_base::left)
        {
          __pad(__s2, __fill, __padding);
          __padding = 0;
        }
      __out_iter __s3 = copy(__middle, __last, __s2);
      if (__padding)
        __pad(__s3, __fill, __padding);
      return __s3;
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    num_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __io, char_type __fill, bool __v) const
    {
      const _Format_cache<_CharT>* __fmt = _Format_cache<_CharT>::_S_get(__io);
      ios_base::fmtflags __flags = __io.flags();

      if ((__flags & ios_base::boolalpha) == 0)
        {
          unsigned long __uv = __v;
          return __output_integer(__s, __io, __fill, false, __uv);
        }
      else
        {
          const char_type* __first;
          const char_type* __last;
          if (__v)
            {
              __first = __fmt->_M_truename.data();
              __last = __first + __fmt->_M_truename.size();
            }
          else
            {
              __first = __fmt->_M_falsename.data();
              __last = __first + __fmt->_M_falsename.size();
            }
          copy(__first, __last, __s);
        }
      return __s;
    }

  template<typename _CharT, typename _OutIter, typename _ValueT>
    _OutIter
    __output_integer(_OutIter __s, ios_base& __io, _CharT __fill, bool __neg,
		     _ValueT __v)
    {
      // Leave room for "+/-," "0x," and commas. This size is
      // arbitrary, but should work.
      const int __n = numeric_limits<_ValueT>::digits10 * 2 + 4;
      _CharT* __digits = static_cast<_CharT*>(__builtin_alloca(sizeof(_CharT) * __n));
      _CharT* __digits_end = __digits + __n;
      ios_base::fmtflags __flags = __io.flags();
      const _Format_cache<_CharT>* __fmt = _Format_cache<_CharT>::_S_get(__io);
      char const* __table = __fmt->_S_literals + __fmt->_S_digits;

      ios_base::fmtflags __basefield = (__flags & __io.basefield);
      _CharT* __sign_end = __digits_end;
      if (__basefield == ios_base::hex)
        {
          if (__flags & ios_base::uppercase)
            __table += 16;  // use ABCDEF
          do
            *--__digits_end = __table[__v & 15];
          while ((__v >>= 4) != 0);
          __sign_end = __digits_end;
          if (__flags & ios_base::showbase)
            {
              *--__digits_end = __fmt->_S_literals[__fmt->_S_x +
                       ((__flags & ios_base::uppercase) ? 1 : 0)];
              *--__digits_end = __table[0];
            }
        }
      else if (__basefield == ios_base::oct)
        {
          do
            *--__digits_end = __table[__v & 7];
          while ((__v >>= 3) != 0);
          if (__flags & ios_base::showbase
              && static_cast<char>(*__digits_end) != __table[0])
            *--__digits_end = __table[0];
          __sign_end = __digits_end;
        }
      else
        {
          // NB: This is _lots_ faster than using ldiv.
          do
            *--__digits_end = __table[__v % 10];
          while ((__v /= 10) != 0);
          __sign_end = __digits_end;
          // NB: ios_base:hex || ios_base::oct assumed to be unsigned.
          if (__neg || (__flags & ios_base::showpos))
            *--__digits_end = __fmt->_S_literals[__fmt->_S_plus - __neg];
        }

      // XXX should specialize!
      if (!__fmt->_M_use_grouping && !__io.width())
        return copy(__digits_end, __digits + __n, __s);

      if (!__fmt->_M_use_grouping)
        return __pad_numeric(__s, __flags, __fill, __io.width(0),
			     __digits_end, __sign_end, __digits + __n);

      _CharT* __p = __digits;
      while (__digits_end < __sign_end)
        *__p++ = *__digits_end++;
      const char* __gbeg = __fmt->_M_grouping.data();
      const char* __gend = __gbeg + __fmt->_M_grouping.size();
      __digits_end = __group_digits(__p, __fmt->_M_thousands_sep, 
				    __gbeg, __gend,
				    __sign_end, __digits + __n);
      return __pad_numeric(__s, __flags, __fill, __io.width(0),
			   __digits, __p, __digits_end);
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    num_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __io, char_type __fill, long __v) const
    {
      unsigned long __uv = __v;
      bool __neg = false;
      if (__v < 0)
        {
          __neg = true;
          __uv = -__uv;
        }
      return __output_integer(__s, __io, __fill, __neg, __uv);
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    num_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __io, char_type __fill,
           unsigned long __v) const
    { return __output_integer(__s, __io, __fill, false, __v); }

#ifdef _GLIBCPP_USE_LONG_LONG
  template<typename _CharT, typename _OutIter>
    _OutIter
    num_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __b, char_type __fill, long long __v) const
    {
      unsigned long long __uv = __v;
      bool __neg = false;
      if (__v < 0)
        {
          __neg = true;
          __uv = -__uv;
        }
      return __output_integer(__s, __b, __fill, __neg, __uv);
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    num_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __io, char_type __fill,
           unsigned long long __v) const
    { return __output_integer(__s, __io, __fill, false, __v); }
#endif

  // Generic helper function
  template<typename _CharT, typename _Traits, typename _OutIter>
    _OutIter
    __output_float(_OutIter __s, ios_base& __io, _CharT __fill,
                    const char* __sptr, size_t __slen)
    {
      // XXX Not currently done: non-streambuf_iterator
      return __s;
    }

  // Partial specialization for ostreambuf_iterator.
  template<typename _CharT, typename _Traits>
    ostreambuf_iterator<_CharT, _Traits>
    __output_float(ostreambuf_iterator<_CharT, _Traits> __s, ios_base& __io, 
		   _CharT __fill, const char* __sptr, size_t __slen)
    {
      size_t __padding = __io.width() > streamsize(__slen) ?
                         __io.width() -__slen : 0;
      locale __loc = __io.getloc();
      ctype<_CharT> const& __ctype = use_facet<ctype<_CharT> >(__loc);
      ios_base::fmtflags __adjfield = __io.flags() & ios_base::adjustfield;
      const char* const __eptr = __sptr + __slen;
      // [22.2.2.2.2.19] Table 61
      if (__adjfield == ios_base::internal)
       {
         // [22.2.2.2.2.14]; widen()
         if (__sptr < __eptr && (*__sptr == '+' || *__sptr == '-'))
           {
             __s = __ctype.widen(*__sptr);
             ++__s;
             ++__sptr;
           }
         __s = __pad(__s, __fill, __padding);
         __padding = 0;
       }
      else if (__adjfield != ios_base::left)
        {
          __s = __pad(__s, __fill, __padding);
          __padding = 0;
        }
      // the "C" locale decimal character
      char __decimal_point = *(localeconv()->decimal_point);
      const _Format_cache<_CharT>* __fmt = _Format_cache<_CharT>::_S_get(__io);
      for (; __sptr != __eptr; ++__s, ++__sptr)
       {
         // [22.2.2.2.2.17]; decimal point conversion
         if (*__sptr == __decimal_point)
           __s = __fmt->_M_decimal_point;
         // [22.2.2.2.2.14]; widen()
         else
           __s = __ctype.widen(*__sptr);
       }
      // [22.2.2.2.2.19] Table 61
      if (__padding)
        __pad(__s, __fill, __padding);
      __io.width(0);
      return __s;
    }

  bool
  __build_float_format(ios_base& __io, char* __fptr, char __modifier,
		       streamsize __prec);

  template<typename _CharT, typename _OutIter>
    _OutIter
    num_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __io, char_type __fill, double __v) const
    {
      const streamsize __max_prec = numeric_limits<double>::digits10 + 3;
      streamsize __prec = __io.precision();
      // Protect against sprintf() buffer overflows.
      if (__prec > __max_prec)
        __prec = __max_prec;
      // The *2 provides for signs, exp, 'E', and pad.
      char __sbuf[__max_prec * 2];
      size_t __slen;
      // Long enough for the max format spec.
      char __fbuf[16];
      if (__build_float_format(__io, __fbuf, 0, __prec))
        __slen = sprintf(__sbuf, __fbuf, __prec, __v);
      else
        __slen = sprintf(__sbuf, __fbuf, __v);
      // [22.2.2.2.2] Stages 2-4.
      return __output_float(__s, __io, __fill, __sbuf, __slen);
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    num_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __io, char_type __fill,
           long double __v) const
    {
      const streamsize __max_prec = numeric_limits<long double>::digits10 + 3;
      streamsize __prec = __io.precision();
      // Protect against sprintf() buffer overflows.
      if (__prec > __max_prec)
        __prec = __max_prec;
      // The *2 provides for signs, exp, 'E', and pad.
      char __sbuf[__max_prec * 2];
      size_t __slen;
      // Long enough for the max format spec.
      char __fbuf[16];
      // 'L' as per [22.2.2.2.2] Table 59
      if (__build_float_format(__io, __fbuf, 'L', __prec))
        __slen = sprintf(__sbuf, __fbuf, __prec, __v);
      else
        __slen = sprintf(__sbuf, __fbuf, __v);
      // [22.2.2.2.2] Stages 2-4
      return __output_float(__s, __io, __fill, __sbuf, __slen);
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    num_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __io, char_type __fill,
           const void* __v) const
    {
      typedef ios_base::fmtflags        fmtflags;
      fmtflags __fmt = __io.flags();
      fmtflags __fmtmask = ~(ios_base::showpos | ios_base::basefield
                             | ios_base::uppercase | ios_base::internal);
      __io.flags(__fmt & __fmtmask | (ios_base::hex | ios_base::showbase));
      try {
        _OutIter __s2 = __output_integer(__s, __io, __fill, false,
                                  reinterpret_cast<unsigned long>(__v));
        __io.flags(__fmt);
        return __s2;
      }
      catch (...) {
        __io.flags(__fmt);
        __throw_exception_again;
      }
    }


  template<typename _CharT, typename _InIter>
    _InIter
    money_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, bool __intl, ios_base& __io, 
	   ios_base::iostate& __err, long double& __units) const
    { 
      string_type __str;
      this->do_get(__beg, __end, __intl, __io, __err, __str); 

      const int __n = numeric_limits<long double>::digits10;
      char* __cs = static_cast<char*>(__builtin_alloca(sizeof(char) * __n));
      const locale __loc = __io.getloc();
      const ctype<_CharT>& __ctype = use_facet<ctype<_CharT> >(__loc); 
      const _CharT* __wcs = __str.c_str();
      __ctype.narrow(__wcs, __wcs + __str.size() + 1, char(), __cs);      

#if defined(_GLIBCPP_USE_C99) && !defined(__hpux)
      char* __sanity;
      errno = 0;
      long double __ld = strtold(__cs, &__sanity);
      if (!(__err & ios_base::failbit)
          && __sanity != __cs && *__sanity == '\0' && errno == 0)
        __units = __ld;
#else
      typedef typename char_traits<_CharT>::int_type int_type;
      long double __ld;
      int __p = sscanf(__cs, "%Lf", &__ld);
      if (!(__err & ios_base::failbit)
	  && __p && static_cast<int_type>(__p) != char_traits<_CharT>::eof())
        __units = __ld;
#endif
      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    money_get<_CharT, _InIter>::
    do_get(iter_type __beg, iter_type __end, bool __intl, ios_base& __io, 
	   ios_base::iostate& __err, string_type& __units) const
    { 
      // These contortions are quite unfortunate.
      typedef moneypunct<_CharT, true> 		__money_true;
      typedef moneypunct<_CharT, false> 	__money_false;
      typedef money_base::part 			part;
      typedef typename string_type::size_type 	size_type;

      const locale __loc = __io.getloc();
      const __money_true& __mpt = use_facet<__money_true>(__loc); 
      const __money_false& __mpf = use_facet<__money_false>(__loc); 
      const ctype<_CharT>& __ctype = use_facet<ctype<_CharT> >(__loc); 

      const money_base::pattern __p = __intl ? __mpt.neg_format() 
					     : __mpf.neg_format();

      const string_type __pos_sign =__intl ? __mpt.positive_sign() 
					   : __mpf.positive_sign();
      const string_type __neg_sign =__intl ? __mpt.negative_sign() 
					   : __mpf.negative_sign();
      const char_type __d = __intl ? __mpt.decimal_point() 
  	    	       		   : __mpf.decimal_point();
      const char_type __sep = __intl ? __mpt.thousands_sep() 
		    		     : __mpf.thousands_sep();

      const string __grouping = __intl ? __mpt.grouping() : __mpf.grouping();

      // Set to deduced positive or negative sign, depending.
      string_type __sign;
      // String of grouping info from thousands_sep plucked from __units.
      string __grouping_tmp; 
      // Marker for thousands_sep position.
      int __sep_pos = 0;
      // If input iterator is in a valid state.
      bool __testvalid = true;
      // Flag marking when a decimal point is found.
      bool __testdecfound = false; 

      char_type __c = *__beg;
      char_type __eof = static_cast<char_type>(char_traits<char_type>::eof());
      for (int __i = 0; __beg != __end && __i < 4 && __testvalid; ++__i)
	{
	  part __which = static_cast<part>(__p.field[__i]);
	  switch (__which)
		{
		case money_base::symbol:
		  if (__io.flags() & ios_base::showbase)
		    {
		      // Symbol is required.
		      const string_type __symbol = __intl ? __mpt.curr_symbol()
						    	 : __mpf.curr_symbol();
		      size_type __len = __symbol.size();
		      size_type __i = 0;
		      while (__beg != __end 
			     && __i < __len && __symbol[__i] == __c)
			{
			  __c = *(++__beg);
			  ++__i;
			}
		      if (__i != __len)
			__testvalid = false;
		    }
		  break;
		case money_base::sign:		    
		  // Sign might not exist, or be more than one character long. 
		  if (__pos_sign.size() && __neg_sign.size())
		  {
		    // Sign is mandatory.
		    if (__c == __pos_sign[0])
		      {
			__sign = __pos_sign;
			__c = *(++__beg);
		      }
		    else if (__c == __neg_sign[0])
		      {
			__sign = __neg_sign;
			__c = *(++__beg);
		      }
		    else
		      __testvalid = false;
		  }
		  else if (__pos_sign.size() && __c == __pos_sign[0])
		    {
		      __sign = __pos_sign;
		      __c = *(++__beg);
		    }
		  else if (__neg_sign.size() && __c == __neg_sign[0])
		    {
		      __sign = __neg_sign;
		      __c = *(++__beg);
		    }
		  break;
		case money_base::value:
		  // Extract digits, remove and stash away the
		  // grouping of found thousands separators.
		  while (__beg != __end 
			 && (__ctype.is(ctype_base::digit, __c) 
			     || (__c == __d && !__testdecfound)
			     || __c == __sep))
		    {
		      if (__c == __d)
			{
			  __grouping_tmp += static_cast<char>(__sep_pos);
			  __sep_pos = 0;
			  __testdecfound = true;
			}
		      else if (__c == __sep)
			{
			  if (__grouping.size())
			    {
			      // Mark position for later analysis.
			      __grouping_tmp += static_cast<char>(__sep_pos);
			      __sep_pos = 0;
			    }
			  else
			    {
			      __testvalid = false;
			      break;
			    }
			}
		      else
			{
			  __units += __c;
			  ++__sep_pos;
			}
		      __c = *(++__beg);
		    }
		  break;
		case money_base::space:
		case money_base::none:
		  // Only if not at the end of the pattern.
		  if (__i != 3)
		    while (__beg != __end 
			   && __ctype.is(ctype_base::space, __c))
		      __c = *(++__beg);
		  break;
		}
	}

      // Need to get the rest of the sign characters, if they exist.
      if (__sign.size() > 1)
	{
	  size_type __len = __sign.size();
	  size_type __i = 1;
	  for (; __c != __eof && __i < __len; ++__i)
	    while (__beg != __end && __c != __sign[__i])
	      __c = *(++__beg);
	  
	  if (__i != __len)
	    __testvalid = false;
	}

      // Strip leading zeros.
      while (__units[0] == __ctype.widen('0'))
	__units.erase(__units.begin());

      if (__sign == __neg_sign)
	__units.insert(__units.begin(), __ctype.widen('-'));

      // Test for grouping fidelity.
      if (__grouping.size() && __grouping_tmp.size())
	{
	  if (!__verify_grouping(__grouping, __grouping_tmp))
	    __testvalid = false;
	}

      // Iff no more characters are available.      
      if (__c == __eof)
	__err |= ios_base::eofbit;

      // Iff valid sequence is not recognized.
      if (!__testvalid || !__units.size())
	__err |= ios_base::failbit;

      return __beg; 
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    money_put<_CharT, _OutIter>::
    do_put(iter_type __s, bool __intl, ios_base& __io, char_type __fill,
	   long double __units) const
    { 
      const locale __loc = __io.getloc();
      const ctype<_CharT>& __ctype = use_facet<ctype<_CharT> >(__loc); 
      const int __n = numeric_limits<long double>::digits10;
      char* __cs = static_cast<char*>(__builtin_alloca(sizeof(char) * __n));
      _CharT* __ws = static_cast<_CharT*>(__builtin_alloca(sizeof(_CharT) * __n));
      int __len = sprintf(__cs, "%.01Lf", __units);
      __ctype.widen(__cs, __cs + __len, __ws);
      string_type __digits(__ws);
      return this->do_put(__s, __intl, __io, __fill, __digits); 
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    money_put<_CharT, _OutIter>::
    do_put(iter_type __s, bool __intl, ios_base& __io, char_type __fill,
	   const string_type& __digits) const
    { 
      typedef typename string_type::size_type 	size_type;
      typedef money_base::part 			part;

      const locale __loc = __io.getloc();
      const size_type __width = static_cast<size_type>(__io.width());

      // These contortions are quite unfortunate.
      typedef moneypunct<_CharT, true> __money_true;
      typedef moneypunct<_CharT, false> __money_false;
      const __money_true& __mpt = use_facet<__money_true>(__loc); 
      const __money_false& __mpf = use_facet<__money_false>(__loc); 
      const ctype<_CharT>& __ctype = use_facet<ctype<_CharT> >(__loc); 

      // Determine if negative or positive formats are to be used, and
      // discard leading negative_sign if it is present.
      const char_type* __beg = __digits.data();
      const char_type* __end = __beg + __digits.size();
      money_base::pattern __p;
      string_type __sign;
      if (*__beg != __ctype.widen('-'))
	{
	  __p = __intl ? __mpt.pos_format() : __mpf.pos_format();
	  __sign =__intl ? __mpt.positive_sign() : __mpf.positive_sign();
	}
      else
	{
	  __p = __intl ? __mpt.neg_format() : __mpf.neg_format();
	  __sign =__intl ? __mpt.negative_sign() : __mpf.negative_sign();
	  ++__beg;
	}
      
      // Look for valid numbers in the current ctype facet within input digits.
      __end = __ctype.scan_not(ctype_base::digit, __beg, __end);
      if (__beg != __end)
	{
	  // Assume valid input, and attempt to format.
	  // Break down input numbers into base components, as follows:
	  //   final_value = grouped units + (decimal point) + (digits)
	  string_type __res;
	  string_type __value;
	  const string_type __symbol = __intl ? __mpt.curr_symbol() 
	    				      : __mpf.curr_symbol();

	  // Deal with decimal point, decimal digits.
	  const int __frac = __intl ? __mpt.frac_digits() 
	    			    : __mpf.frac_digits();
	  if (__frac > 0)
	    {
	      const char_type __d = __intl ? __mpt.decimal_point() 
					   : __mpf.decimal_point();
	      if (__end - __beg >= __frac)
		{
		  __value = string_type(__end - __frac, __end);
		  __value.insert(__value.begin(), __d);
		  __end -= __frac;
		}
	      else
		{
		  // Have to pad zeros in the decimal position.
		  __value = string_type(__beg, __end);
		  int __paddec = __frac - (__end - __beg);
		  char_type __zero = __ctype.widen('0');
		  __value.insert(__value.begin(), __paddec, __zero);
		  __value.insert(__value.begin(), __d);
		  __beg = __end;
		}
	    }

	  // Add thousands separators to non-decimal digits, per
	  // grouping rules.
	  if (__beg != __end)
	    {
	      const string __grouping = __intl ? __mpt.grouping() 
					       : __mpf.grouping();
	      if (__grouping.size())
		{
		  const char_type __sep = __intl ? __mpt.thousands_sep() 
		    			         : __mpf.thousands_sep();
		  const char* __gbeg = __grouping.data();
		  const char* __gend = __gbeg + __grouping.size();
		  const int __n = numeric_limits<long double>::digits10 * 2;
		  _CharT* __s = static_cast<_CharT*>(__builtin_alloca(sizeof(_CharT) * __n));
		  _CharT* __s_end = __group_digits(__s, __sep, __gbeg, 
						   __gend, __beg, __end);
		  __value.insert(0, __s, __s_end - __s);
		}
	      else
		__value.insert(0, string_type(__beg, __end));
	    }

	  // Calculate length of resulting string.
	  ios_base::fmtflags __f = __io.flags() & ios_base::adjustfield;
	  size_type __len = __value.size() + __sign.size();
	  __len += (__io.flags() & ios_base::showbase) ? __symbol.size() : 0;
	  bool __testipad = __f == ios_base::internal && __len < __width;

	  // Fit formatted digits into the required pattern.
	  for (int __i = 0; __i < 4; ++__i)
	    {
	      part __which = static_cast<part>(__p.field[__i]);
	      switch (__which)
		{
		case money_base::symbol:
		  if (__io.flags() & ios_base::showbase)
		    __res += __symbol;
		  break;
		case money_base::sign:		    
		  // Sign might not exist, or be more than one
		  // charater long. In that case, add in the rest
		  // below.
		  if (__sign.size())
		    __res += __sign[0];
		  break;
		case money_base::value:
		  __res += __value;
		  break;
		case money_base::space:
		  // At least one space is required, but if internal
		  // formatting is required, an arbitrary number of
		  // fill spaces will be necessary.
		  if (__testipad)
		    __res += string_type(__width - __len, __fill);
		  else
		    __res += __ctype.widen(' ');
		  break;
		case money_base::none:
		  if (__testipad)
		    __res += string_type(__width - __len, __fill);
		  break;
		}
	    }

	  // Special case of multi-part sign parts.
	  if (__sign.size() > 1)
	    __res += string_type(__sign.begin() + 1, __sign.end());

	  // Pad, if still necessary.
	  __len = __res.size();
	  if (__width > __len)
	    {
	      if (__f == ios_base::left)
		// After.
		__res.append(__width - __len, __fill);
	      else
		// Before.
		__res.insert(0, string_type(__width - __len, __fill));
	      __len = __width;
	    }

	  // Write resulting, fully-formatted string to output iterator.
	  for (size_type __j = 0; __j < __len; ++__j)
	    __s = __res[__j];
	}
      __io.width(0);
      return __s; 
    }


  // NB: Not especially useful. Without an ios_base object or some
  // kind of locale reference, we are left clawing at the air where
  // the side of the mountain used to be...
  template<typename _CharT, typename _InIter>
    time_base::dateorder
    time_get<_CharT, _InIter>::do_date_order() const
    { return time_base::no_order; }

  template<typename _CharT, typename _InIter>
    void
    time_get<_CharT, _InIter>::
    _M_extract_via_format(iter_type& __beg, iter_type& __end, ios_base& __io,
			  ios_base::iostate& __err, tm* __tm, 
			  const _CharT* __format) const
    {  
      locale __loc = __io.getloc();
      __timepunct<_CharT> const& __tp = use_facet<__timepunct<_CharT> >(__loc);
      const ctype<_CharT>& __ctype = use_facet<ctype<_CharT> >(__loc); 
      size_t __len = char_traits<_CharT>::length(__format);

      for (size_t __i = 0; __beg != __end && __i < __len && !__err; ++__i)
	{
	  char __c = __format[__i];
	  if (__c == '%')
	    {
	      // Verify valid formatting code, attempt to extract.
	      __c = __format[++__i];
	      char __mod = 0;
	      int __mem = 0; 
	      if (__c == 'E' || __c == 'O')
		{
		  __mod = __c;
		  __c = __format[++__i];
		}
	      switch (__c)
		{
		  const char* __cs;
		  _CharT __wcs[10];
		case 'a':
		  // Abbreviated weekday name [tm_wday]
		  const char_type*  __days1[7];
		  __tp._M_days_abbreviated(__days1);
		  _M_extract_name(__beg, __end, __tm->tm_wday, __days1, 7, 
				  __err);
		  break;
		case 'A':
		  // Weekday name [tm_wday].
		  const char_type*  __days2[7];
		  __tp._M_days(__days2);
		  _M_extract_name(__beg, __end, __tm->tm_wday, __days2, 7, 
				  __err);
		  break;
		case 'h':
		case 'b':
		  // Abbreviated month name [tm_mon]
		  const char_type*  __months1[12];
		  __tp._M_months_abbreviated(__months1);
		  _M_extract_name(__beg, __end, __tm->tm_mon, __months1, 12, 
				  __err);
		  break;
		case 'B':
		  // Month name [tm_mon].
		  const char_type*  __months2[12];
		  __tp._M_months(__months2);
		  _M_extract_name(__beg, __end, __tm->tm_mon, __months2, 12, 
				  __err);
		  break;
		case 'c':
		  // Default time and date representation.
		  const char_type*  __dt[2];
		  __tp._M_date_time_formats(__dt);
		  _M_extract_via_format(__beg, __end, __io, __err, __tm, 
					__dt[0]);
		  break;
		case 'd':
		  // Day [01, 31]. [tm_mday]
		  _M_extract_num(__beg, __end, __tm->tm_mday, 1, 31, 2, 
				 __ctype, __err);
		  break;
		case 'D':
		  // Equivalent to %m/%d/%y.[tm_mon, tm_mday, tm_year]
		  __cs = "%m/%d/%y";
		  __ctype.widen(__cs, __cs + 9, __wcs);
		  _M_extract_via_format(__beg, __end, __io, __err, __tm, 
					__wcs);
		  break;
		case 'H':
		  // Hour [00, 23]. [tm_hour]
		  _M_extract_num(__beg, __end, __tm->tm_hour, 0, 23, 2,
				 __ctype, __err);
		  break;
		case 'I':
		  // Hour [01, 12]. [tm_hour]
		  _M_extract_num(__beg, __end, __tm->tm_hour, 1, 12, 2,
				 __ctype, __err);
		  break;
		case 'm':
		  // Month [01, 12]. [tm_mon]
		  _M_extract_num(__beg, __end, __mem, 1, 12, 2,
				 __ctype, __err);
		  if (!__err)
		    __tm->tm_mon = __mem - 1;
		  break;
		case 'M':
		  // Minute [00, 59]. [tm_min]
		  _M_extract_num(__beg, __end, __tm->tm_min, 0, 59, 2,
				 __ctype, __err);
		  break;
		case 'n':
		  if (__ctype.narrow(*__beg, 0) == '\n')
		    ++__beg;
		  else
		    __err |= ios_base::failbit;
		  break;
		case 'R':
		  // Equivalent to (%H:%M).
		  __cs = "%H:%M";
		  __ctype.widen(__cs, __cs + 6, __wcs);
		  _M_extract_via_format(__beg, __end, __io, __err, __tm, 
					__wcs);
		  break;
		case 'S':
		  // Seconds.
		  _M_extract_num(__beg, __end, __tm->tm_sec, 0, 59, 2,
				 __ctype, __err);
		  break;
		case 't':
		  if (__ctype.narrow(*__beg, 0) == '\t')
		    ++__beg;
		  else
		__err |= ios_base::failbit;
		  break;
		case 'T':
		  // Equivalent to (%H:%M:%S).
		  __cs = "%H:%M:%S";
		  __ctype.widen(__cs, __cs + 9, __wcs);
		  _M_extract_via_format(__beg, __end, __io, __err, __tm, 
					__wcs);
		  break;
		case 'x':
		  // Locale's date.
		  const char_type*  __dates[2];
		  __tp._M_date_formats(__dates);
		  _M_extract_via_format(__beg, __end, __io, __err, __tm, 
					__dates[0]);
		  break;
		case 'X':
		  // Locale's time.
		  const char_type*  __times[2];
		  __tp._M_time_formats(__times);
		  _M_extract_via_format(__beg, __end, __io, __err, __tm, 
					__times[0]);
		  break;
		case 'y':
		  // Two digit year. [tm_year]
		  _M_extract_num(__beg, __end, __tm->tm_year, 0, 99, 2, 
				 __ctype, __err);
		  break;
		case 'Y':
		  // Year [1900). [tm_year]
		  _M_extract_num(__beg, __end, __mem, 0, 
				 numeric_limits<int>::max(), 4, 
				 __ctype, __err);
		  if (!__err)
		    __tm->tm_year = __mem - 1900;
		  break;
		case 'Z':
		  // Timezone info.
		  if (__ctype.is(ctype_base::upper, *__beg))
		    {
		      int __tmp;
		      _M_extract_name(__beg, __end, __tmp, 
				      __timepunct<_CharT>::_S_timezones, 
				      14, __err);
		      
		      // GMT requires special effort.
		      char_type __c = *__beg;
		      if (!__err && __tmp == 0 
			  && (__c == __ctype.widen('-') 
			      || __c == __ctype.widen('+')))
			{
			  _M_extract_num(__beg, __end, __tmp, 0, 23, 2,
					  __ctype, __err);
			  _M_extract_num(__beg, __end, __tmp, 0, 59, 2,
					  __ctype, __err);
			}	    
			  }
		      else
			__err |= ios_base::failbit;
		      break;
		    default:
		      // Not recognized.
		      __err |= ios_base::failbit;
		    }
		}
	      else
		{
		  // Verify format and input match, extract and discard.
		  if (__c == __ctype.narrow(*__beg, 0))
		    ++__beg;
		  else
		    __err |= ios_base::failbit;
		}
	}
    }

  template<typename _CharT, typename _InIter>
    void
    time_get<_CharT, _InIter>::
    _M_extract_num(iter_type& __beg, iter_type& __end, int& __member,
		   int __min, int __max, size_t __len, 
		   const ctype<_CharT>& __ctype, 
		   ios_base::iostate& __err) const
    {
      size_t __i = 0;
      string __digits;
      bool __testvalid = true;
      char_type __c = *__beg;
      while (__beg != __end && __i < __len 
	     && __ctype.is(ctype_base::digit, __c)) 
	{
	  __digits += __ctype.narrow(__c, 0);
	  __c = *(++__beg);
	  ++__i;
	}
      if (__i == __len)
	{
	  int __value = atoi(__digits.c_str());
	  if (__min <= __value && __value <= __max)
	    __member = __value;
	  else
	    __testvalid = false;
	}
      else
	__testvalid = false;
      if (!__testvalid)
	__err |= ios_base::failbit;
    }

  // Assumptions:
  // All elements in __names are unique.
  template<typename _CharT, typename _InIter>
    void
    time_get<_CharT, _InIter>::
    _M_extract_name(iter_type& __beg, iter_type& __end, int& __member,
		    const _CharT** __names, size_t __indexlen, 
		    ios_base::iostate& __err) const
    {
      typedef char_traits<char_type> __traits_type;
      int __matches[__indexlen];
      size_t __nmatches = 0;
      size_t __pos = 0;
      bool __testvalid = true;
      const char_type* __name;

      char_type __c = *__beg;
      // Look for initial matches.
      for (size_t __i1 = 0; __i1 < __indexlen; ++__i1)
	if (__c == __names[__i1][0])
	  __matches[__nmatches++] = __i1;
      
      while(__nmatches > 1)
	{
	  // Find smallest matching string.
	  size_t __minlen = 10;
	  for (size_t __i2 = 0; __i2 < __nmatches; ++__i2)
	    __minlen = min(__minlen, 
			   __traits_type::length(__names[__matches[__i2]]));
	  
	  if (__pos < __minlen && __beg != __end)
	    {
	      ++__pos;
	      __c = *(++__beg);
	      for (size_t __i3 = 0; __i3 < __nmatches; ++__i3)
		{
		  __name = __names[__matches[__i3]];
		  if (__name[__pos] != __c)
		    __matches[__i3] = __matches[--__nmatches];
		}
	    }
	  else
	    break;
	}

      if (__nmatches == 1)
	{
	  // Make sure found name is completely extracted.
	  __name = __names[__matches[0]];
	  const size_t __len = __traits_type::length(__name);
	  while (__pos < __len && __beg != __end && __name[__pos] == *__beg)
	    ++__beg, ++__pos;

	  if (__len == __pos)
	    __member = __matches[0];
	  else
	    __testvalid = false;
	}
      else
	__testvalid = false;
      if (!__testvalid)
	__err |= ios_base::failbit;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    time_get<_CharT, _InIter>::
    do_get_time(iter_type __beg, iter_type __end, ios_base& __io,
		ios_base::iostate& __err, tm* __tm) const
    {
      _CharT __wcs[3];
      const char* __cs = "%X";
      locale __loc = __io.getloc();
      ctype<_CharT> const& __ctype = use_facet<ctype<_CharT> >(__loc);
      __ctype.widen(__cs, __cs + 3, __wcs);
      _M_extract_via_format(__beg, __end, __io, __err, __tm, __wcs);
      if (__beg == __end)
	__err |= ios_base::eofbit;
      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    time_get<_CharT, _InIter>::
    do_get_date(iter_type __beg, iter_type __end, ios_base& __io,
		ios_base::iostate& __err, tm* __tm) const
    {
      _CharT __wcs[3];
      const char* __cs = "%x";
      locale __loc = __io.getloc();
      ctype<_CharT> const& __ctype = use_facet<ctype<_CharT> >(__loc);
      __ctype.widen(__cs, __cs + 3, __wcs);
      _M_extract_via_format(__beg, __end, __io, __err, __tm, __wcs);
      if (__beg == __end)
	__err |= ios_base::eofbit;
      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    time_get<_CharT, _InIter>::
    do_get_weekday(iter_type __beg, iter_type __end, ios_base& __io, 
		   ios_base::iostate& __err, tm* __tm) const
    {
      typedef char_traits<char_type> __traits_type;
      locale __loc = __io.getloc();
      __timepunct<_CharT> const& __tp = use_facet<__timepunct<_CharT> >(__loc);
      const char_type*  __days[7];
      __tp._M_days_abbreviated(__days);
      int __tmpwday;
      _M_extract_name(__beg, __end, __tmpwday, __days, 7, __err);

      // Check to see if non-abbreviated name exists, and extract.
      // NB: Assumes both _M_days and _M_days_abbreviated organized in
      // exact same order, first to last, such that the resulting
      // __days array with the same index points to a day, and that
      // day's abbreviated form.
      // NB: Also assumes that an abbreviated name is a subset of the name. 
      if (!__err)
	{
	  size_t __pos = __traits_type::length(__days[__tmpwday]);
	  __tp._M_days(__days);
	  const char_type* __name = __days[__tmpwday];
	  if (__name[__pos] == *__beg)
	    {
	      // Extract the rest of it.
	      const size_t __len = __traits_type::length(__name);
	      while (__pos < __len && __beg != __end 
		     && __name[__pos] == *__beg)
		++__beg, ++__pos;
	      if (__len != __pos)
		__err |= ios_base::failbit;
	    }
	  if (!__err)
	    __tm->tm_wday = __tmpwday;
	}
      if (__beg == __end)
	__err |= ios_base::eofbit;
      return __beg;
     }

  template<typename _CharT, typename _InIter>
    _InIter
    time_get<_CharT, _InIter>::
    do_get_monthname(iter_type __beg, iter_type __end,
                     ios_base& __io, ios_base::iostate& __err, tm* __tm) const
    {
      typedef char_traits<char_type> __traits_type;
      locale __loc = __io.getloc();
      __timepunct<_CharT> const& __tp = use_facet<__timepunct<_CharT> >(__loc);
      const char_type*  __months[12];
      __tp._M_months_abbreviated(__months);
      int __tmpmon;
      _M_extract_name(__beg, __end, __tmpmon, __months, 12, __err);

      // Check to see if non-abbreviated name exists, and extract.
      // NB: Assumes both _M_months and _M_months_abbreviated organized in
      // exact same order, first to last, such that the resulting
      // __months array with the same index points to a month, and that
      // month's abbreviated form.
      // NB: Also assumes that an abbreviated name is a subset of the name. 
      if (!__err)
	{
	  size_t __pos = __traits_type::length(__months[__tmpmon]);
	  __tp._M_months(__months);
	  const char_type* __name = __months[__tmpmon];
	  if (__name[__pos] == *__beg)
	    {
	      // Extract the rest of it.
	      const size_t __len = __traits_type::length(__name);
	      while (__pos < __len && __beg != __end 
		     && __name[__pos] == *__beg)
		++__beg, ++__pos;
	      if (__len != __pos)
		__err |= ios_base::failbit;
	    }
	  if (!__err)
	    __tm->tm_mon = __tmpmon;
	}
 
      if (__beg == __end)
	__err |= ios_base::eofbit;
      return __beg;
    }

  template<typename _CharT, typename _InIter>
    _InIter
    time_get<_CharT, _InIter>::
    do_get_year(iter_type __beg, iter_type __end, ios_base& __io, 
		ios_base::iostate& __err, tm* __tm) const
    {
      locale __loc = __io.getloc();
      const ctype<_CharT>& __ctype = use_facet<ctype<_CharT> >(__loc); 

      char_type __c = *__beg;
      size_t __i = 0;
      string __digits;
      while (__i < 4 && __beg != __end && __ctype.is(ctype_base::digit, __c))
	{
	  __digits += __ctype.narrow(__c, 0);
	  __c = *(++__beg);
	  ++__i;
	}
      if (__i == 2 || __i == 4)
	{
	  int __year = atoi(__digits.c_str());
	  __year = __i == 2 ? __year : __year - 1900; 
	  __tm->tm_year = __year;
	}
      else
	__err |= ios_base::failbit;
      if (__beg == __end)
	__err |= ios_base::eofbit;
      return __beg;
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    time_put<_CharT, _OutIter>::
    put(iter_type __s, ios_base& __io, char_type, const tm* __tm, 
	const _CharT* __beg, const _CharT* __end) const
    {
      locale __loc = __io.getloc();
      ctype<_CharT> const& __ctype = use_facet<ctype<_CharT> >(__loc);
      while (__beg != __end)
	{
	  char __c = __ctype.narrow(*__beg, 0);
	  ++__beg;
	  if (__c == '%')
	    {
	      char __format;
	      char __mod = 0;
	      size_t __len = 1; 
	      __c = __ctype.narrow(*__beg, 0);
	      ++__beg;
	      if (__c == 'E' || __c == 'O')
		{
		  __mod = __c;
		  __format = __ctype.narrow(*__beg, 0);
		  ++__beg;
		}
	      else
		__format = __c;
	      this->do_put(__s, __io, char_type(), __tm, __format, __mod);
	    }
	  else
	    __s = __c;
	}
      return __s;
    }

  template<typename _CharT, typename _OutIter>
    _OutIter
    time_put<_CharT, _OutIter>::
    do_put(iter_type __s, ios_base& __io, char_type, const tm* __tm, 
	   char __format, char __mod) const
    { 
      locale __loc = __io.getloc();
      ctype<_CharT> const& __ctype = use_facet<ctype<_CharT> >(__loc);
      __timepunct<_CharT> const& __tp = use_facet<__timepunct<_CharT> >(__loc);

      // NB: This size is arbitrary. Should this be a data member,
      // initialized at construction?
      const size_t __maxlen = 64;
      char_type* __res = static_cast<char_type*>(__builtin_alloca(__maxlen));

      // NB: In IEE 1003.1-200x, and perhaps other locale models, it
      // is possible that the format character will be longer than one
      // character. Possibilities include 'E' or 'O' followed by a
      // format charcter: if __mod is not the default argument, assume
      // it's a valid modifier.
      char_type __fmt[4];
      __fmt[0] = __ctype.widen('%'); 
      if (!__mod)
	{
	  __fmt[1] = __format;
	  __fmt[2] = char_type();
	}
      else
	{
	  __fmt[1] = __mod;
	  __fmt[2] = __format;
	  __fmt[3] = char_type();
	}

      __tp._M_put_helper(__res, __maxlen, __fmt, __tm);

      // Write resulting, fully-formatted string to output iterator.
      size_t __len = char_traits<char_type>::length(__res);
      for (size_t __i = 0; __i < __len; ++__i)
	__s = __res[__i];
      return __s;
    }


  // Generic version does nothing.
  template<typename _CharT>
    int
    collate<_CharT>::_M_compare_helper(const _CharT*, const _CharT*) const
    { return 0; }

  // Generic version does nothing.
  template<typename _CharT>
    size_t
    collate<_CharT>::_M_transform_helper(_CharT*, const _CharT*, size_t) const
    { return 0; }

  template<typename _CharT>
    int
    collate<_CharT>::
    do_compare(const _CharT* __lo1, const _CharT* __hi1, 
	       const _CharT* __lo2, const _CharT* __hi2) const
    { 
      const string_type __one(__lo1, __hi1);
      const string_type __two(__lo2, __hi2);
      return _M_compare_helper(__one.c_str(), __two.c_str());
    }

 template<typename _CharT>
    collate<_CharT>::string_type
    collate<_CharT>::
    do_transform(const _CharT* __lo, const _CharT* __hi) const
    {
      string_type __orig(__lo, __hi);
      string_type __trans(__orig.size(), char_type());
      size_t __res = _M_transform_helper(__trans.begin().base(), 
					 __orig.c_str(), __trans.size());
      while (__res >= __trans.size())
	{
	  // Increment size of translated string.
	  typename string_type::size_type __newsize = __trans.size() * 2;
	  __trans.resize(__newsize);
	  __res = _M_transform_helper(__trans.begin().base(), __orig.c_str(), 
				      __trans.size());
	}
      return __trans;
    }

 template<typename _CharT>
    long
    collate<_CharT>::
    do_hash(const _CharT* __lo, const _CharT* __hi) const
    { 
      unsigned long __val = 0;
      for (; __lo < __hi; ++__lo)
	__val = *__lo + ((__val << 7) | 
		       (__val >> (numeric_limits<unsigned long>::digits - 1)));
      return static_cast<long>(__val);
    }

  // Used by both numeric and monetary facets.
  // Check to make sure that the __grouping_tmp string constructed in
  // money_get or num_get matches the canonical grouping for a given
  // locale.
  // __grouping_tmp is parsed L to R
  // 1,222,444 == __grouping_tmp of "/1/3/3"
  // __grouping is parsed R to L
  // 1,222,444 == __grouping of "/3" == "/3/3/3"
  template<typename _CharT>
    bool
    __verify_grouping(const basic_string<_CharT>& __grouping, 
		      basic_string<_CharT>& __grouping_tmp)
    {         
      int __i = 0;
      int __j = 0;
      const int __len = __grouping.size();
      const int __n = __grouping_tmp.size();
      bool __test = true;
      
      // Parsed number groupings have to match the
      // numpunct::grouping string exactly, starting at the
      // right-most point of the parsed sequence of elements ...
      while (__test && __i < __n - 1)
	for (__j = 0; __test && __j < __len && __i < __n - 1; ++__j,++__i)
	  __test &= __grouping[__j] == __grouping_tmp[__n - __i - 1];
      // ... but the last parsed grouping can be <= numpunct
      // grouping.
      __j == __len ? __j = 0 : __j;
      __test &= __grouping[__j] >= __grouping_tmp[__n - __i - 1];
      return __test;
    }

  // Used by both numeric and monetary facets.
  // Inserts "group separator" characters into an array of characters.
  // It's recursive, one iteration per group.  It moves the characters
  // in the buffer this way: "xxxx12345" -> "12,345xxx".  Call this
  // only with __gbeg != __gend.
  template<typename _CharT>
    _CharT*
    __group_digits(_CharT* __s, _CharT __sep,  
		   const char* __gbeg, const char* __gend, 
		   const _CharT* __first, const _CharT* __last)
    {
      if (__last - __first > *__gbeg)
        {
          __s = __group_digits(__s,  __sep,
			       (__gbeg + 1 == __gend ? __gbeg : __gbeg + 1),
			       __gend, __first, __last - *__gbeg);
          __first = __last - *__gbeg;
          *__s++ = __sep;
        }
      do
        {
          *__s++ = *__first++;
        }
      while (__first != __last);
      return __s;
    }

  // XXX This stuff needs to be re-examined, heavily modified, or ditched.
  template<typename _CharT>
    _Format_cache<_CharT>::_Format_cache()
    : _M_valid(true), _M_use_grouping(false)
    { }

  template<>
    _Format_cache<char>::_Format_cache();

  template<>
    _Format_cache<wchar_t>::_Format_cache();

  template<typename _CharT>
    void
    _Format_cache<_CharT>::_M_populate(ios_base& __io)
    {
      locale __loc = __io.getloc();
      numpunct<_CharT> const& __np = use_facet<numpunct<_CharT> >(__loc);
      _M_truename = __np.truename();
      _M_falsename = __np.falsename();
      _M_thousands_sep = __np.thousands_sep();
      _M_decimal_point = __np.decimal_point();
      _M_grouping = __np.grouping();
      _M_use_grouping = _M_grouping.size() != 0 && _M_grouping.data()[0] != 0;
      _M_valid = true;
    }

  // This function is always called via a pointer installed in
  // an ios_base by ios_base::register_callback.
  template<typename _CharT>
    void
    _Format_cache<_CharT>::
    _S_callback(ios_base::event __ev, ios_base& __ios, int __ix) throw()
    {
      void*& __p = __ios.pword(__ix);
      switch (__ev)
        {
        case ios_base::erase_event:
          delete static_cast<_Format_cache<_CharT>*>(__p);
	  __p = 0;
          break;
        case ios_base::copyfmt_event:
          // If just stored zero, the callback would get registered again.
          try 
	    { __p = new _Format_cache<_CharT>; }
          catch(...) 
	    { }
          break;
        case ios_base::imbue_event:
          static_cast<_Format_cache<_CharT>*>(__p)->_M_valid = false;
          break;
        }
    }

  template<typename _CharT>
    _Format_cache<_CharT>*
    _Format_cache<_CharT>::_S_get(ios_base& __ios)
    {
      if (!_S_pword_ix)
        _S_pword_ix = ios_base::xalloc();  // XXX MT
      void*& __p = __ios.pword(_S_pword_ix);

      // XXX What if pword fails? must check failbit, throw.
      if (__p == 0)  // XXX MT?  maybe sentry takes care of it
        {
          auto_ptr<_Format_cache<_CharT> > __ap(new _Format_cache<_CharT>);
          __ios.register_callback(&_Format_cache<_CharT>::_S_callback,
                                  _S_pword_ix);
          __p = __ap.release();
        }
      _Format_cache<_CharT>* __ncp = static_cast<_Format_cache<_CharT>*>(__p);
      if (!__ncp->_M_valid)
        __ncp->_M_populate(__ios);

      return __ncp;
    }
} // namespace std

#endif
