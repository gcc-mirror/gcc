// Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

#include <bits/std_clocale.h>
#include <bits/std_cstring.h>
#include <bits/std_cassert.h>
#include <bits/std_cctype.h>
#include <bits/std_limits.h>
#include <exception>
#include <bits/std_stdexcept.h>
#include <bits/std_locale.h>
#include <bits/std_istream.h>
#include <bits/std_ostream.h>
#include <bits/std_vector.h>
#include <bits/std_memory.h>      // for auto_ptr
#ifdef _GLIBCPP_USE_WCHAR_T  
# include <bits/std_cwctype.h>     // for towupper, etc.
#endif

namespace std {

  // Definitions for static const data members of locale.
  const locale::category 	locale::none;
  const locale::category 	locale::collate;
  const locale::category 	locale::ctype;
  const locale::category 	locale::monetary;
  const locale::category 	locale::numeric;
  const locale::category 	locale::time;
  const locale::category 	locale::messages;
  const locale::category 	locale::all;

  locale::_Impl* 		locale::_S_classic;
  locale::_Impl* 		locale::_S_global; 
  const int 			locale::_S_categories_num;
  const int 			locale::_S_facets_num;

  // Definitions for static const data members of locale::_Impl
  const locale::id* const
  locale::_Impl::_S_id_collate[] =
  {
    &std::collate<char>::id,
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::collate<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const
  locale::_Impl::_S_id_ctype[] =
  {
    &std::ctype<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::ctype<wchar_t>::id,
#endif
    &codecvt<char, char, mbstate_t>::id,
#ifdef _GLIBCPP_USE_WCHAR_T
    &codecvt<wchar_t, char, mbstate_t>::id,
#endif
    0
  };

  const locale::id* const
  locale::_Impl::_S_id_monetary[] =
  {
    &moneypunct<char, false>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &moneypunct<wchar_t, false>::id,
#endif
    &std::moneypunct<char,true >::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &moneypunct<wchar_t,true >::id,
#endif
    &money_get<char>::id,        
#ifdef _GLIBCPP_USE_WCHAR_T
    &money_get<wchar_t>::id,
#endif
    &money_put<char>::id,        
#ifdef _GLIBCPP_USE_WCHAR_T
    &money_put<wchar_t>::id,
#endif
    0
  };

  const locale::id* const
  locale::_Impl::_S_id_numeric[] =
  {
    &numpunct<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &numpunct<wchar_t>::id,
#endif
    &num_get<char>::id,  
 #ifdef _GLIBCPP_USE_WCHAR_T
    &num_get<wchar_t>::id,
#endif
    &num_put<char>::id,  
#ifdef _GLIBCPP_USE_WCHAR_T
    &num_put<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const
  locale::_Impl::_S_id_time[] =
  {
    &time_get<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &time_get<wchar_t>::id,
#endif
    &time_put<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &time_put<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const
  locale::_Impl::_S_id_messages[] =
  {
    &time_get<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &time_get<wchar_t>::id,
#endif
    &time_put<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &time_put<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const* const
  locale::_Impl::_S_facet_categories[] =
  {
    // Order must match the decl order in class locale.
    locale::_Impl::_S_id_collate,
    locale::_Impl::_S_id_ctype,
    locale::_Impl::_S_id_monetary,
    locale::_Impl::_S_id_numeric,
    locale::_Impl::_S_id_time,
    locale::_Impl::_S_id_messages,
    0
  };

  // Definitions for static const data members of locale::id
  size_t locale::id::_S_highwater;  // init'd to 0 by linker

  // Definitions for static const data members of money_base
  const money_base::pattern 
  money_base::_S_default_pattern =  {{symbol, sign, none, value}};;

  template<>
    _Format_cache<char>::_Format_cache()
    : _M_valid(true),
    _M_decimal_point('.'), _M_thousands_sep(','),
    _M_truename("true"), _M_falsename("false"), _M_use_grouping(false)
    { }

#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    _Format_cache<wchar_t>::_Format_cache()
    : _M_valid(true),
    _M_decimal_point(L'.'), _M_thousands_sep(L','),
    _M_truename(L"true"), _M_falsename(L"false"), _M_use_grouping(false)
    { }
#endif

  template<>
    const ctype<char>&
    use_facet<ctype<char> >(const locale& __loc)
    {
      size_t __i = ctype<char>::id._M_index;
      const locale::_Impl* __tmp = __loc._M_impl;
      return static_cast<const ctype<char>&>(* (*(__tmp->_M_facets))[__i]);
    }

#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    const ctype<wchar_t>&
    use_facet<ctype<wchar_t> >(const locale& __loc)
    {
      size_t __i = ctype<wchar_t>::id._M_index;
      const locale::_Impl* __tmp = __loc._M_impl;
      return static_cast<const ctype<wchar_t>&>(* (*(__tmp->_M_facets))[__i]);
    }
#endif

  template<>
    void
    num_get<char, istreambuf_iterator<char> >::
    _M_extract(istreambuf_iterator<char> __beg, 
	       istreambuf_iterator<char> __end, ios_base& __io, 
	       ios_base::iostate& __err, char* __xtrc, int& __base, 
	       bool __fp) const
    {
      typedef _Format_cache<char> __cache_type;	

      // Prepare for possible failure
      __xtrc[0] = '\0';

      // Stage 1: determine a conversion specifier.
      ios_base::fmtflags __basefield = __io.flags() & ios_base::basefield;
      if (__basefield == ios_base::dec)
        __base = 10;
      else if (__basefield == ios_base::oct)
        __base = 8;
      else if (__basefield == ios_base::hex)
        __base = 16;
      else
        __base = 0;
      // As far as I can tell, bases other than 10 are not available for
      // floating point types
      if (__fp)
        __base = 10;

      // Stage 2: extract characters.
      __cache_type const* __fmt = __cache_type::_S_get(__io);
      bool __valid = __beg != __end;
      // Fail quickly if !__valid
      if (!__valid)
        {
          __err |= (ios_base::eofbit | ios_base::failbit);
          return;
        }

      // Acceptable formats for numbers here are based on 22.2.3.1
      string __grp;
      int __sep_pos = 0;
      int __pos = 0;
      const char* __lits = __fmt->_S_literals;
      char __c = *__beg;

      // Check first for sign
      bool __testsign = false;
      if ((__c == __lits[__cache_type::_S_minus])
          || (__c == __lits[__cache_type::_S_plus]))
        {
          __xtrc[__pos++] = __c;
          ++__beg;
          __testsign = true;
          // whitespace may follow a sign
          while ((__beg != __end) && (isspace(*__beg)))
            ++__beg;

          // There had better be more to come...
          if (__beg == __end)
            {
              __xtrc[__pos] = '\0';
              __err |= (ios_base::eofbit | ios_base::failbit);
              return;
            }
        }

      bool __testzero = false;    // Has there been a leading zero?

      // Now check if first character is a zero
      __c = *__beg;
      if (__c == __lits[__cache_type::_S_digits])
        {
           __testzero = true;
           ++__beg;

           // We have to check for __beg == __end here. If so,
           // a plain '0' (possibly with a sign) can be got rid of now
           if (__beg == __end)
             {
               __xtrc[__pos++] = __c;
               __xtrc[__pos] = '\0';
               __err |= ios_base::eofbit;
               return;
             }

          // Figure out base for integer types only
          // Based on Table 55 of 22.2.2.1.2
          if (!__fp && __base != 10 && __base != 8)
            {
              // Here, __base == 0 or 16
              __c = *__beg;
              if ((__c == __lits[__cache_type::_S_x])
                 || (__c == __lits[__cache_type::_S_X]))
                {
                  ++__beg;
                  __base = 16;
                  __testzero = false; // "0x" is not a leading zero
                }
              else if (__base == 0)
                __base = 8;
            }

          // Remove any more leading zeros
          while (__beg != __end)
            {
              if (*__beg == __lits[__cache_type::_S_digits])
                {
                  ++__beg;
                  __testzero = true;
                }
              else
                break;
            }
        }
      else if (__base == 0) // 1st character is not zero
        __base = 10;

      // We now seek "units", i.e. digits and thousands separators.
      // We may need to know if anything is found here. A leading zero
      // (removed by now) would count.
      bool __testunits = __testzero;
      while (__valid && __beg != __end)
        {
          __valid = false;
          __c = *__beg;
          const char* __p = strchr(__fmt->_S_literals, __c);

          // NB: strchr returns true for __c == 0x0
          if (__p && __c)
            {
              // Try first for acceptable digit; record it if found
              if ((__p >= &__lits[__cache_type::_S_digits]
                    && __p < &__lits[__cache_type::_S_digits + __base])
                   || (__p >= &__lits[__cache_type::_S_udigits]
                       && __p < &__lits[__cache_type::_S_udigits + __base]))
                {
                  __xtrc[__pos++] = __c;
                  ++__sep_pos;
                  __valid = true;
                  __testunits = true;
                }
            }
          else if (__c == __fmt->_M_thousands_sep
                   && __fmt->_M_use_grouping)
            {
              // NB: Thousands separator at the beginning of a string
              // is a no-no, as is two consecutive thousands
              // separators
              if (__sep_pos)
                {
                  __grp += static_cast<char>(__sep_pos);
                  __sep_pos = 0;
                  __valid = true;
                }
              else
                __err |= ios_base::failbit;
            }
          if (__valid)
            ++__beg;
        }

      // Digit grouping is checked. If _M_groupings() doesn't
      // match, then get very very upset, and set failbit.
      if (__fmt->_M_use_grouping && !__grp.empty())
        {
          // Add the ending grouping
          __grp += static_cast<char>(__sep_pos);

          // __grp is parsed L to R
          // 1,222,444 == __grp of "/1/3/3"
          // __fmt->_M_grouping is parsed R to L
          // 1,222,444 == __fmt->_M_grouping of "/3" == "/3/3/3"
          int __i = 0;
          int __j = 0;
          const int __len = __fmt->_M_grouping.size();
          int __n = __grp.size();
          bool __test = true;

          // Parsed number groupings have to match the
          // numpunct::grouping string exactly, starting at the
          // right-most point of the parsed sequence of elements ...
          while (__test && __i < __n - 1)
            for (__j = 0; __test && __j < __len && __i < __n - 1; ++__j,++__i)
              __test &= __fmt->_M_grouping[__j] == __grp[__n - __i - 1];
          // ... but the last parsed grouping can be <= numpunct
          // grouping.
          __j == __len ? __j = 0 : __j;
          __test &= __fmt->_M_grouping[__j] >= __grp[__n - __i - 1];

          if (!__test)
            {
              __err |= ios_base::failbit;
              __xtrc[__pos] = '\0';
              if (__beg == __end)
                __err |= ios_base::eofbit;
              return;
            }
        }

      // If there was nothing but zeros, put one in the output string
      if (__testzero && (__pos == 0 || (__pos == 1 && __testsign)))
        __xtrc[__pos++] = __lits[__cache_type::_S_digits];

      // That's it for integer types. Remaining code is for floating point
      if (__fp && __beg != __end)
        {
          __c = *__beg;
          // Check first for decimal point. There MUST be one if
          // __testunits is false.
          bool __testdec = false;    // Is there a decimal point
                                     // with digits following it?
          if (__c == __fmt->_M_decimal_point)
            {
              __xtrc[__pos++] = '.';
              ++__beg;
              // Now we get any digits after the decimal point
              // There MUST be some if __testunits is false.
              while (__beg != __end)
                {
                  __c = *__beg;
                  const char* __p = strchr(__fmt->_S_literals, __c);
                  if ((__p >= &__lits[__cache_type::_S_digits]
                        && __p < &__lits[__cache_type::_S_digits + __base])
                       || (__p >= &__lits[__cache_type::_S_udigits]
                           && __p < &__lits[__cache_type::_S_udigits + __base]))
                    {
                      __xtrc[__pos++] = __c;
                      ++__beg;
                      __testdec = true;
                    }
                  else
                    break;
                }
            }
          if (!__testunits && !__testdec) // Ill formed
            {
              __err |= ios_base::failbit;
              __xtrc[__pos] = '\0';
              if (__beg == __end)
                __err |= ios_base::eofbit;
              return;
            }

          // Now we may find an exponent
          if (__beg != __end)
            {
              __c = *__beg;
              if ((__c == __lits[__cache_type::_S_ee])
                   || (__c == __lits[__cache_type::_S_Ee]))
                {
                  __xtrc[__pos++] = __c;
                  ++__beg;
                  // Now there may be a sign
                  if (__beg != __end)
                    {
                      __c = *__beg;
                      if ((__c == __lits[__cache_type::_S_minus])
                          || (__c == __lits[__cache_type::_S_plus]))
                        {
                          __xtrc[__pos++] = __c;
                          ++__beg;
                          // whitespace may follow a sign
                          while ((__beg != __end) && (isspace(*__beg)))
                            ++__beg;

                        }
                    }
                  // And now there must be some digits
                  if (__beg == __end)
                    {
                      __xtrc[__pos] = '\0';
                      __err |= (ios_base::eofbit | ios_base::failbit);
                      return;
                    }
                  while (__beg != __end)
                    {
                      __c = *__beg;
                      const char* __p = strchr(__fmt->_S_literals, __c);
                      if ((__p >= &__lits[__cache_type::_S_digits]
                            && __p < &__lits[__cache_type::_S_digits + __base])
                           || (__p >= &__lits[__cache_type::_S_udigits]
                               && __p < &__lits[__cache_type::_S_udigits + __base]))
                        {
                          __xtrc[__pos++] = __c;
                          ++__beg;
                        }
                      else
                        break;
                    }
                }
            }
          // Finally, that's it for floating point
        }

      // Finish up
      __xtrc[__pos] = '\0';
      if (__beg == __end)
        __err |= ios_base::eofbit;
    }

  // The following code uses sprintf() to convert floating point
  // values for insertion into a stream. The current implementation
  // replicates the code in _S_pad_numeric() (in _S_output_float()) in
  // order to prevent having to create a "wide" buffer in addition to
  // the "narrow" buffer passed to sprintf(). An optimization would be
  // to replace sprintf() with code that works directly on a wide
  // buffer and then use _S_pad_numeric() to do the padding. It would
  // be good to replace sprintf() anyway to avoid accidental buffer
  // overruns and to gain back the efficiency that C++ provides by
  // knowing up front the type of the values to insert. This
  // implementation follows the C++ standard fairly directly as
  // outlined in 22.2.2.2 [lib.locale.num.put]
  bool
  _S_build_float_format(ios_base& __io, char* __fptr, char __modifier,
                        streamsize __prec)
  {
    bool __incl_prec = false;
    ios_base::fmtflags __flags = __io.flags();
    *__fptr++ = '%';
    // [22.2.2.2.2] Table 60
    if (__flags & ios_base::showpos)
      *__fptr++ = '+';
    if (__flags & ios_base::showpoint)
      *__fptr++ = '#';
    // As per [22.2.2.2.2.11]
    if (__flags & ios_base::fixed || __prec > 0)
      {
        *__fptr++ = '.';
        *__fptr++ = '*';
        __incl_prec = true;
      }
    if (__modifier)
      *__fptr++ = __modifier;
    ios_base::fmtflags __fltfield = __flags & ios_base::floatfield;
    // [22.2.2.2.2] Table 58
    if (__fltfield == ios_base::fixed)
      *__fptr++ = 'f';
    else if (__fltfield == ios_base::scientific)
      *__fptr++ = (__flags & ios_base::uppercase) ? 'E' : 'e';
    else
      *__fptr++ = (__flags & ios_base::uppercase) ? 'G' : 'g';
    *__fptr = '\0';
    return __incl_prec;
  }


  locale::locale(_Impl* __ip) throw()
  : _M_impl(__ip)
  { __ip->_M_add_reference(); }


  locale::locale(const char* __name)
  {
    if (__name)
      {
	if (strcmp(__name, "C") == 0 || strcmp(__name, "POSIX") == 0)
	  (_M_impl = _S_classic)->_M_add_reference();
	// Might throw:
	else
	  // XXX Named locale support not finished.
	  // _M_impl = new _Impl(_S_facets_num, 1, true, __name);
	  _M_impl = new _Impl(*_S_classic, __name, all, 1);
      }
    else
      throw runtime_error("attempt to create named locale from NULL name");
  }

  locale::locale(const locale& __other, const char* __name, category __cat)
  { 
    if (__name)
      {
	if (__other.name() == __name)
	  (_M_impl = __other._M_impl)->_M_add_reference();
	// Might throw:
	else
	  _M_impl = new _Impl(*__other._M_impl, __name, __cat, 1);
      }
    else
      throw runtime_error("attempt to create locale from NULL named locale");
  }

  locale::locale(const locale& __other, const locale& __one, category __cat)
  {
    __cat = _S_normalize_category(__cat);    // might throw
    _M_impl = new _Impl(*__other._M_impl, 1);  // might throw

    try { 
      _M_impl->_M_replace_categories(__one._M_impl, __cat); 
    }
    catch (...) { 
      _M_impl->_M_remove_reference(); 
      throw; 
    }

    // XXX
    //    _M_impl->_M_cached_name_ok = false;
    if (!__other._M_impl->_M_has_name)
      _M_impl->_M_has_name = false;
  }

  bool
  locale::operator==(const locale& __rhs) const throw()
  {
    return((this->name() != "*" && this->name() == __rhs.name())
	   || _M_impl == __rhs._M_impl);
  }

  const locale&
  locale::operator=(const locale& __other) throw()
  {
    __other._M_impl->_M_add_reference();
    _M_impl->_M_remove_reference();
    _M_impl = __other._M_impl;
    return *this;
  }

  locale
  locale::global(const locale& __other)
  {
    // XXX MT
    _S_initialize();
    locale __keep(_S_global);
    __other._M_impl->_M_add_reference();
    _S_global->_M_remove_reference();
    _S_global = __other._M_impl; 
    if (_S_global->_M_has_name)
      setlocale(LC_ALL, __other.name().c_str());
    return __keep;
  }

  string
  locale::name() const
  { return _M_impl->_M_name; }

  locale const&
  locale::classic()
  {
    static locale* __classic_locale;
    // XXX MT
    if (!_S_classic)
      {
	try {
	  // 26 Standard facets, 2 references.
	  // One reference for _M_classic, one for _M_global
	  _S_classic = new _Impl(_S_facets_num, 2, true, "C");
	  _S_global = _S_classic; 

	  _S_classic->_M_facet_init(new std::collate<char>);
	  _S_classic->_M_facet_init(new std::ctype<char>);
	  _S_classic->_M_facet_init(new codecvt<char, char, mbstate_t>);
	  _S_classic->_M_facet_init(new moneypunct<char, false>);
	  _S_classic->_M_facet_init(new moneypunct<char,true >);
	  _S_classic->_M_facet_init(new money_get<char>);
	  _S_classic->_M_facet_init(new money_put<char>);
	  _S_classic->_M_facet_init(new numpunct<char>);
	  _S_classic->_M_facet_init(new num_get<char>);
	  _S_classic->_M_facet_init(new num_put<char>);
	  _S_classic->_M_facet_init(new time_get<char>);
	  _S_classic->_M_facet_init(new time_put<char>);
	  _S_classic->_M_facet_init(new std::messages<char>);

#ifdef  _GLIBCPP_USE_WCHAR_T
	  _S_classic->_M_facet_init(new std::collate<wchar_t>);
	  _S_classic->_M_facet_init(new std::ctype<wchar_t>);
	  _S_classic->_M_facet_init(new codecvt<wchar_t, char, mbstate_t>);
	  _S_classic->_M_facet_init(new moneypunct<wchar_t, false>);
	  _S_classic->_M_facet_init(new moneypunct<wchar_t,true >);
	  _S_classic->_M_facet_init(new money_get<wchar_t>);
	  _S_classic->_M_facet_init(new money_put<wchar_t>);
	  _S_classic->_M_facet_init(new numpunct<wchar_t>);
	  _S_classic->_M_facet_init(new num_get<wchar_t>);
	  _S_classic->_M_facet_init(new num_put<wchar_t>);
	  _S_classic->_M_facet_init(new time_get<wchar_t>);
	  _S_classic->_M_facet_init(new time_put<wchar_t>);
	  _S_classic->_M_facet_init(new std::messages<wchar_t>);
#endif	  

	  // Finesse static init order hassles
	  __classic_locale = new locale(_S_classic);
	}
	catch(...) {
	  delete __classic_locale;
	  if (_S_classic)
	    {
	      _S_classic->_M_remove_reference();
	      _S_global->_M_remove_reference();
	    }
	  _S_classic = _S_global = 0;
	  // XXX MT
	  throw;
	}
      }
    return *__classic_locale;
  }

  int
  locale::_S_normalize_category(int __cat) 
  {
    int __ret;
    if ((__cat & all) && !(__cat & ~all))
      __ret = __cat;
    else
      {
	// NB: May be a C-style "LC_ALL" category; convert.
	switch (__cat)
	  {
	  case LC_COLLATE:  
	    __ret = collate; 
	    break;
	  case LC_CTYPE:    
	    __ret = ctype;
	    break;
	  case LC_MONETARY: 
	    __ret = monetary;
	    break;
	  case LC_NUMERIC:  
	    __ret = numeric;
	    break;
	  case LC_TIME:     
	    __ret = time; 
	    break;
#ifdef _GLIBCPP_HAVE_LC_MESSAGES
	  case LC_MESSAGES: 
	    __ret = messages;
	    break;
#endif	
	  case LC_ALL:      
	    __ret = all;
	    break;
	  default:
	    throw runtime_error("bad locale category");
	  }
      }
    return __ret;
  }

  locale::facet::
  facet(size_t __refs) throw()
  : _M_references(__refs - 1) 
  { }

  void  
  locale::facet::
  _M_add_reference() throw()
  { 
    if (this) 
      ++_M_references; 
  }                     // XXX MT

  void  
  locale::facet::
  _M_remove_reference() throw()
  {
    if (this && _M_references-- == 0)
      {
        try { 
	  delete this; 
	}  // XXX MT
	catch (...) { 
	}
      }
  }

  char const* 
  _Bad_use_facet::
  what() const throw()
  { return "_Bad_use_facet thrown from use_facet"; }

  _Bad_use_facet::
  ~_Bad_use_facet() throw() { }
  
  // Platform-specific initialization code for ctype tables.
  #include <bits/ctype_noninline.h>

  locale::id ctype<char>::id;

  const size_t ctype<char>::table_size;

  ctype<char>::~ctype()
  { if (_M_del) delete[] this->table(); }

  char
  ctype<char>::do_widen(char __c) const
  { return __c; }
  
  const char* 
  ctype<char>::do_widen(const char* __low, const char* __high, 
			char* __dest) const
  {
    memcpy(__dest, __low, __high - __low);
    return __high;
  }
  
  char
  ctype<char>::do_narrow(char __c, char /*__dfault*/) const
  { return __c; }
  
  const char* 
  ctype<char>::do_narrow(const char* __low, const char* __high, 
			 char /*__dfault*/, char* __dest) const
  {
    memcpy(__dest, __low, __high - __low);
    return __high;
  }

  ctype_byname<char>::ctype_byname(const char* /*__s*/, size_t __refs)
  : ctype<char>(new mask[table_size], true, __refs)
  { }

  locale::id collate<char>::id;

  collate<char>::collate(size_t __refs)
  : _Collate<char>(__refs) { }
  
  collate<char>::~collate() { }
  
  int 
  collate<char>::do_compare(const char* __lo1, const char* __hi1, 
			    const char* __lo2, const char* __hi2) const
  {
    for (; __lo1 < __hi1 && __lo2 < __hi2; ++__lo1, ++__lo2) 
      if (*__lo1 != *__lo2) 
	return (*__lo1 < *__lo2) ? -1 : 1;
    if (__lo1 < __hi1) 
      return 1;
    else if (__lo2 < __hi2) 
      return -1;
    else 
      return 0;
  }
  
  string
  collate<char>::
  do_transform(const char* __lo, const char* __hi) const
  { return string(__lo, __hi - __lo); }
  
  long
  collate<char>::
  do_hash(const char* __lo, const char* __hi) const
  {
    unsigned long __val = 0xdeadbeef;
    for (; __lo < __hi; ++__lo)
      __val = *__lo ^ ((__val << 7) & 
		   (__val >> (numeric_limits<unsigned long>::digits - 1)));
    return __val;
  }
  
  collate_byname<char>::collate_byname(const char* /*__s*/, size_t __refs)
  : collate<char>(__refs) { }

  numpunct_byname<char>::numpunct_byname(const char* /*__s*/, size_t __refs)
  : numpunct<char>(__refs) { }

  moneypunct_byname<char, false>::moneypunct_byname(const char* /*__s*/, 
						    size_t __refs)
  : moneypunct<char, false>(__refs) { }
  
  moneypunct_byname<char, true>::moneypunct_byname(const char* /*__s*/, 
						   size_t __refs)
  : moneypunct<char, true>(__refs) { }
  
  messages_byname<char>::
  messages_byname(const char* /*__s*/, size_t __refs)
  : messages<char>(__refs) { }

#ifdef _GLIBCPP_USE_WCHAR_T  
  locale::id ctype<wchar_t>::id;

  ctype<wchar_t>::
  ~ctype() { }

  // NB: These ctype<wchar_t> methods are not configuration-specific,
  // unlike the ctype<char> bits.
  ctype<wchar_t>::ctype(size_t __refs) : _Ctype<wchar_t>(__refs) { }

  wchar_t
  ctype<wchar_t>::do_toupper(wchar_t __c) const
  { return towupper(__c); }

  const wchar_t*
  ctype<wchar_t>::do_toupper(wchar_t* __low, const wchar_t* __high) const
  {
    while (__low < __high)
      {
        *__low = towupper(*__low);
        ++__low;
      }
    return __high;
  }
  
  wchar_t
  ctype<wchar_t>::do_tolower(wchar_t __c) const
  { return towlower(__c); }
  
  const wchar_t*
  ctype<wchar_t>::do_tolower(wchar_t* __low, const wchar_t* __high) const
  {
    while (__low < __high)
      {
        *__low = towlower(*__low);
        ++__low;
      }
    return __high;
  }

  bool
  ctype<wchar_t>::
  do_is(mask __m, char_type __c) const
  { return static_cast<bool>(iswctype(__c, _M_convert_to_wmask(__m))); }
  
  const wchar_t* 
  ctype<wchar_t>::
  do_is(const wchar_t* __low, const wchar_t* __high, mask* __m) const
  {
    while (__low < __high && !this->is(*__m, *__low))
      ++__low;
    return __low;
  }
  
  const wchar_t* 
  ctype<wchar_t>::
  do_scan_is(mask __m, const wchar_t* __low, const wchar_t* __high) const
  {
    while (__low < __high && !this->is(__m, *__low))
      ++__low;
    return __low;
  }

  const wchar_t*
  ctype<wchar_t>::
  do_scan_not(mask __m, const char_type* __low, const char_type* __high) const
  {
    while (__low < __high && this->is(__m, *__low) != 0)
      ++__low;
    return __low;
  }

  wchar_t
  ctype<wchar_t>::
  do_widen(char __c) const
  { return btowc(__c); }
  
  const char* 
  ctype<wchar_t>::
  do_widen(const char* __low, const char* __high, wchar_t* __dest) const
  {
    mbstate_t __state;
    memset(static_cast<void*>(&__state), 0, sizeof(mbstate_t));
    mbsrtowcs(__dest, &__low, __high - __low, &__state);
    return __high;
  }

  char
  ctype<wchar_t>::
  do_narrow(wchar_t __wc, char __dfault) const
  { 
    int __c = wctob(__wc);
    return (__c == EOF ? __dfault : static_cast<char>(__c)); 
  }

  const wchar_t*
  ctype<wchar_t>::
  do_narrow(const wchar_t* __low, const wchar_t* __high, char __dfault, 
	    char* __dest) const
  {
    mbstate_t __state;
    memset(static_cast<void*>(&__state), 0, sizeof(mbstate_t));
    size_t __len = __high - __low;
    size_t __conv = wcsrtombs(__dest, &__low, __len, &__state);
    if (__conv == __len)
      *__dest = __dfault;
    return __high;
  }

  ctype_byname<wchar_t>::
  ctype_byname(const char* /*__s*/, size_t __refs)
  : ctype<wchar_t>(__refs) { }

  locale::id collate<wchar_t>::id;

  collate<wchar_t>::
  collate(size_t __refs)
  : _Collate<wchar_t> (__refs) { }
  
  collate<wchar_t>::
  ~collate() { }

  int 
  collate<wchar_t>::
  do_compare(const wchar_t* /*__lo1*/, const wchar_t* /*__hi1*/,
	     const wchar_t* /*__lo2*/, const wchar_t* /*__hi2*/) const
  {
    return 0; // XXX not done
  }
  
  wstring collate<wchar_t>::
  do_transform(const wchar_t* /*__lo*/, const wchar_t* /*__hi*/) const
  {
    return wstring(); // XXX not done
  }
  
  long collate<wchar_t>::
  do_hash(const wchar_t* /*__lo*/, const wchar_t* /*__hi*/) const
  {
    return 0; // XXX not done
  }

  numpunct_byname<wchar_t>::
  numpunct_byname(const char* /*__s*/, size_t __refs)
  : numpunct<wchar_t> (__refs) { }

  collate_byname<wchar_t>::
  collate_byname(const char* /*__s*/, size_t __refs)
  : collate<wchar_t> (__refs) { }
  
  moneypunct_byname<wchar_t, false>::
  moneypunct_byname(const char* /*__s*/, size_t __refs)
  : moneypunct<wchar_t, false> (__refs) { }
  
  moneypunct_byname<wchar_t, true>::
  moneypunct_byname(const char* /*__s*/, size_t __refs)
  : moneypunct<wchar_t, true> (__refs) { }
    
  messages_byname<wchar_t>::
  messages_byname(const char* /*__s*/, size_t __refs)
  : messages<wchar_t> (__refs) { }
#endif //  _GLIBCPP_USE_WCHAR_T

} // namespace std






