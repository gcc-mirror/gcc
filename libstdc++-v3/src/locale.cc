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

namespace std 
{
  // Defined in globals.cc.
  extern locale::_Impl locale_impl_c;
  extern locale locale_c;

  // Definitions for static const data members of locale.
  const locale::category 	locale::none;
  const locale::category 	locale::ctype;
  const locale::category 	locale::numeric;
  const locale::category 	locale::collate;
  const locale::category 	locale::time;
  const locale::category 	locale::monetary;
  const locale::category 	locale::messages;
  const locale::category 	locale::all;

  locale::_Impl* 		locale::_S_classic;
  locale::_Impl* 		locale::_S_global; 
  const size_t 			locale::_S_num_categories;
  const size_t 			locale::_S_num_facets;

  // Definitions for locale::id of standard facets. 
  locale::id ctype<char>::id;
  locale::id codecvt<char, char, mbstate_t>::id;

#ifdef _GLIBCPP_USE_WCHAR_T  
  locale::id ctype<wchar_t>::id;
  locale::id codecvt<wchar_t, char, mbstate_t>::id;
#endif

  // Definitions for static const data members of locale::id
  size_t locale::id::_S_highwater;  // init'd to 0 by linker

  // Definitions for static const data members of locale::_Impl
  const locale::id* const
  locale::_Impl::_S_id_ctype[] =
  {
    &std::ctype<char>::id, 
    &codecvt<char, char, mbstate_t>::id,
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::ctype<wchar_t>::id,
    &codecvt<wchar_t, char, mbstate_t>::id,
#endif
    0
  };

  const locale::id* const
  locale::_Impl::_S_id_numeric[] =
  {
    &num_get<char>::id,  
    &num_put<char>::id,  
    &numpunct<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &num_get<wchar_t>::id,
    &num_put<wchar_t>::id,
    &numpunct<wchar_t>::id,
#endif
    0
  };
  
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
  locale::_Impl::_S_id_time[] =
  {
    &time_get<char>::id, 
    &time_put<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &time_get<wchar_t>::id,
    &time_put<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const
  locale::_Impl::_S_id_monetary[] =
  {
    &money_get<char>::id,        
    &money_put<char>::id,        
    &moneypunct<char, false>::id, 
    &moneypunct<char, true >::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &money_get<wchar_t>::id,
    &money_put<wchar_t>::id,
    &moneypunct<wchar_t, false>::id,
    &moneypunct<wchar_t, true >::id,
#endif
    0
  };

  const locale::id* const
  locale::_Impl::_S_id_messages[] =
  {
    &std::messages<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::messages<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const* const
  locale::_Impl::_S_facet_categories[] =
  {
    // Order must match the decl order in class locale.
    locale::_Impl::_S_id_ctype,
    locale::_Impl::_S_id_numeric,
    locale::_Impl::_S_id_collate,
    locale::_Impl::_S_id_time,
    locale::_Impl::_S_id_monetary,
    locale::_Impl::_S_id_messages,
    0
  };

  locale::~locale() throw()
  { _M_impl->_M_remove_reference(); }

  void
  locale::_M_coalesce(const locale& __base, const locale& __add, 
		      category __cat)
  {
    __cat = _S_normalize_category(__cat);  
    _M_impl = new _Impl(*__base._M_impl, 1);  

    try 
      { _M_impl->_M_replace_categories(__add._M_impl, __cat); }
    catch (...) 
      { 
	_M_impl->_M_remove_reference(); 
	__throw_exception_again;
      }
  }

  locale::locale() throw()
  { 
    _S_initialize(); 
    (_M_impl = _S_global)->_M_add_reference(); 
  } // XXX MT

  locale::locale(const locale& __other) throw()
  { (_M_impl = __other._M_impl)->_M_add_reference(); }

  // This is used to initialize global and classic locales, and
  // assumes that the _Impl objects are constructed correctly.
  locale::locale(_Impl* __ip) throw() : _M_impl(__ip)
  { }

  locale::locale(const char* __s)
  {
    if (__s)
      {
	_S_initialize(); 
	if (strcmp(__s, "C") == 0 || strcmp(__s, "POSIX") == 0)
	  (_M_impl = _S_classic)->_M_add_reference();
	else
	  _M_impl = new _Impl(__s, 1);
      }
    else
      __throw_runtime_error("attempt to create locale from NULL name");
  }

  locale::locale(const locale& __base, const char* __s, category __cat)
  { 
    // NB: There are complicated, yet more efficient ways to do
    // this. Building up locales on a per-category way is tedious, so
    // let's do it this way until people complain.
    locale __add(__s);
    _M_coalesce(__base, __add, __cat);
  }

  locale::locale(const locale& __base, const locale& __add, category __cat)
  { _M_coalesce(__base, __add, __cat); }

  bool
  locale::operator==(const locale& __rhs) const throw()
  {
    string __name = this->name();
    return (_M_impl == __rhs._M_impl 
	    || (__name != "*" && __name == __rhs.name()));
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
    _Impl* __old = _S_global;
    __other._M_impl->_M_add_reference();
    _S_global = __other._M_impl; 
    if (_S_global->_M_check_same_name() && _S_global->_M_names[0] != "*")
      setlocale(LC_ALL, __other.name().c_str());

    // Reference count sanity check: one reference removed for the
    // subsition of __other locale, one added by return-by-value. Net
    // difference: zero. When the returned locale object's destrutor
    // is called, then the reference count is decremented and possibly
    // destroyed.
    return locale(__old);
  }

  string
  locale::name() const
  {
    string __ret;
    // Need some kind of separator character. This one was pretty much
    // arbitrarily chosen as to not conflict with glibc locales: the
    // exact formatting is not set in stone.
    const char __separator = '|';

    if (_M_impl->_M_check_same_name())
      __ret = _M_impl->_M_names[0];
    else
      {
	for (size_t i = 0; i < _S_num_categories; ++i)
	  __ret += __separator + _M_impl->_M_names[i];
      }
    return __ret;
  }

  locale const&
  locale::classic()
  {
    // XXX MT
    if (!_S_classic)
      {
	try 
	  {
	    // 26 Standard facets, 2 references.
	    // One reference for _M_classic, one for _M_global
	    _S_classic = new (&locale_impl_c) _Impl("C", 2);
	    _S_global = _S_classic; 	    
	    new (&locale_c) locale(_S_classic);
	  }
	catch(...) 
	  {
	    // Just call destructor, so that locale_impl_c's memory is
	    // not deallocated via a call to delete.
	    if (_S_classic)
	      _S_classic->~_Impl();
	    _S_classic = _S_global = 0;
	    __throw_exception_again;
	  }
      }
    return locale_c;
  }

  locale::category
  locale::_S_normalize_category(category __cat) 
  {
    int __ret = 0;
    if (__cat == none || (__cat & all) && !(__cat & ~all))
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
	    __throw_runtime_error("bad locale category");
	  }
      }
    return __ret;
  }

  locale::facet::
  facet(size_t __refs) throw() : _M_references(__refs) 
  { }

  void  
  locale::facet::
  _M_add_reference() throw()
  { ++_M_references; }                     // XXX MT

  void  
  locale::facet::
  _M_remove_reference() throw()
  {
    if (_M_references-- == 0)
      {
        try 
	  { delete this; }  
	catch (...) 
	  { }
      }
  }
  
  // Definitions for static const data members of ctype_base.
  const ctype_base::mask ctype_base::space;
  const ctype_base::mask ctype_base::print;
  const ctype_base::mask ctype_base::cntrl;
  const ctype_base::mask ctype_base::upper;
  const ctype_base::mask ctype_base::lower;
  const ctype_base::mask ctype_base::alpha;
  const ctype_base::mask ctype_base::digit;
  const ctype_base::mask ctype_base::punct;
  const ctype_base::mask ctype_base::xdigit;
  const ctype_base::mask ctype_base::alnum;
  const ctype_base::mask ctype_base::graph;

  // Platform-specific initialization code for ctype tables.
  #include <bits/ctype_noninline.h>

  const size_t ctype<char>::table_size;

  ctype<char>::~ctype()
  { if (_M_del) delete[] this->table(); }

  // These are dummy placeholders as these virtual functions are never called.
  bool 
  ctype<char>::do_is(mask, char_type) const 
  { return false; }
  
  const char*
  ctype<char>::do_is(const char_type* __c, const char_type*, mask*) const 
  { return __c; }
  
  const char*
  ctype<char>::do_scan_is(mask, const char_type* __c, const char_type*) const 
  { return __c; }

  const char* 
  ctype<char>::do_scan_not(mask, const char_type* __c, const char_type*) const
  { return __c; }

  char
  ctype<char>::do_widen(char __c) const
  { return __c; }
  
  const char* 
  ctype<char>::do_widen(const char* __lo, const char* __hi, char* __dest) const
  {
    memcpy(__dest, __lo, __hi - __lo);
    return __hi;
  }
  
  char
  ctype<char>::do_narrow(char __c, char /*__dfault*/) const
  { return __c; }
  
  const char* 
  ctype<char>::do_narrow(const char* __lo, const char* __hi, 
			 char /*__dfault*/, char* __dest) const
  {
    memcpy(__dest, __lo, __hi - __lo);
    return __hi;
  }

  template<>
  ctype_byname<char>::ctype_byname(const char* /*__s*/, size_t __refs)
  : ctype<char>(new mask[table_size], true, __refs)
  { }

  // Definitions for static const data members of money_base
  const money_base::pattern 
  money_base::_S_default_pattern =  {{symbol, sign, none, value}};

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

      // Fail quickly if !__valid
      if (__beg == __end)
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
          __testsign = true;
          __xtrc[__pos++] = __c;
          ++__beg;
	  __c = * __beg;

          // Whitespace may follow a sign
          while ((__beg != __end) && (isspace(__c)))
	    {
	      ++__beg;
	      __c = *__beg;
	    }

          // There had better be more to come...
          if (__beg == __end)
            {
              __xtrc[__pos] = '\0';
              __err |= (ios_base::eofbit | ios_base::failbit);
              return;
            }
        }

      // Now check if first character is a zero.
      bool __testzero = false;    
      if (__c == __lits[__cache_type::_S_digits])
        {
           __testzero = true;
           ++__beg;
	   __c = *__beg;

           // We have to check for __beg == __end here. If so,
           // a plain '0' (possibly with a sign) can be got rid of now
           if (__beg == __end)
             {
               __xtrc[__pos++] = __lits[__cache_type::_S_digits];
               __xtrc[__pos] = '\0';
               __err |= ios_base::eofbit;
               return;
             }

          // Figure out base for integer types only
          // Based on Table 55 of 22.2.2.1.2
          if (!__fp && __base != 10 && __base != 8)
            {
              // Here, __base == 0 or 16
              if ((__c == __lits[__cache_type::_S_x])
                 || (__c == __lits[__cache_type::_S_X]))
                {
                  ++__beg;
		  __c = *__beg;
                  __base = 16;
                  __testzero = false; // "0x" is not a leading zero
                }
              else if (__base == 0)
                __base = 8;
            }

          // Remove any more leading zeros
          while (__beg != __end)
            {
              if (__c == __lits[__cache_type::_S_digits])
                {
                  ++__beg;
		  __c = *__beg;
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
      while (__beg != __end)
        {
          const char* __p = strchr(__lits, __c);

          // NB: strchr returns true for __c == 0x0
          if (__p && __c
	      &&((__p >= &__lits[__cache_type::_S_digits]
		  && __p < &__lits[__cache_type::_S_digits + __base])
		 || (__p >= &__lits[__cache_type::_S_udigits]
		     && __p < &__lits[__cache_type::_S_udigits + __base])))
	    {
	      // Try first for acceptable digit; record it if found.
	      __xtrc[__pos++] = __c;
	      ++__sep_pos;
	      __testunits = true;
	      ++__beg;
	      __c = *__beg;
	    }
          else if (__c == __fmt->_M_thousands_sep && __fmt->_M_use_grouping)
	    {
              // NB: Thousands separator at the beginning of a string
              // is a no-no, as is two consecutive thousands
              // separators.
              if (__sep_pos)
                {
                  __grp += static_cast<char>(__sep_pos);
                  __sep_pos = 0;
		  ++__beg;
		  __c = *__beg;
                }
              else
		{
		  __err |= ios_base::failbit;
		  break;
		}
            }
	  else
	    // Not a valid input item.
	    break;
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
          // Check first for decimal point. There MUST be one if
          // __testunits is false.
          bool __testdec = false;    // Is there a decimal point
                                     // with digits following it?
          if (__c == __fmt->_M_decimal_point)
            {
              __xtrc[__pos++] = '.';
              ++__beg;
	      __c = *__beg;

              // Now we get any digits after the decimal point
              // There MUST be some if __testunits is false.
              while (__beg != __end)
                {
                  const char* __p = strchr(__lits, __c);
                  if ((__p >= &__lits[__cache_type::_S_digits]
                        && __p < &__lits[__cache_type::_S_digits + __base])
                       || (__p >= &__lits[__cache_type::_S_udigits]
                           && __p < &__lits[__cache_type::_S_udigits + __base]))
                    {
                      __xtrc[__pos++] = __c;
                      ++__beg;
		      __c = *__beg;
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
              if ((__c == __lits[__cache_type::_S_ee])
                   || (__c == __lits[__cache_type::_S_Ee]))
                {
                  __xtrc[__pos++] = __c;
                  ++__beg;
		  __c = *__beg;

                  // Now there may be a sign
                  if (__beg != __end)
                    {
                      if ((__c == __lits[__cache_type::_S_minus])
                          || (__c == __lits[__cache_type::_S_plus]))
                        {
                          __xtrc[__pos++] = __c;
                          ++__beg;
			  __c = *__beg;
                          // whitespace may follow a sign
                          while ((__beg != __end) && (isspace(__c)))
			    {
			      ++__beg;
			      __c = *__beg;
			    }
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
                      const char* __p = strchr(__lits, __c);
                      if ((__p >= &__lits[__cache_type::_S_digits]
                            && __p < &__lits[__cache_type::_S_digits + __base])
                           || (__p >= &__lits[__cache_type::_S_udigits]
                               && __p < &__lits[__cache_type::_S_udigits + __base]))
                        {
                          __xtrc[__pos++] = __c;
                          ++__beg;
			  __c = *__beg;
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
  __build_float_format(ios_base& __io, char* __fptr, char __modifier,
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

  template<>
  moneypunct_byname<char, false>::moneypunct_byname(const char* /*__s*/, 
						    size_t __refs)
  : moneypunct<char, false>(__refs) { }
  
  template<>
  moneypunct_byname<char, true>::moneypunct_byname(const char* /*__s*/, 
						   size_t __refs)
  : moneypunct<char, true>(__refs) { }
  
#ifdef _GLIBCPP_USE_WCHAR_T  
  ctype<wchar_t>::__wmask_type
  ctype<wchar_t>::_M_convert_to_wmask(const mask __m) const
  {
    __wmask_type __ret;
    switch (__m)
      {
      case space:
	__ret = wctype("space");
	break;
      case print:
	__ret = wctype("print");
	break;
      case cntrl:
	__ret = wctype("cntrl");
	break;
      case upper:
	__ret = wctype("upper");
	break;
      case lower:
	__ret = wctype("lower");
	break;
      case alpha:
	__ret = wctype("alpha");
	break;
      case digit:
	__ret = wctype("digit");
	break;
      case punct:
	__ret = wctype("punct");
	break;
      case xdigit:
	__ret = wctype("xdigit");
	break;
      case alnum:
	__ret = wctype("alnum");
	break;
      case graph:
	__ret = wctype("graph");
	break;
      default:
	__ret = 0;
      }
    return __ret;
  };
  
  ctype<wchar_t>::~ctype() { }

  // NB: These ctype<wchar_t> methods are not configuration-specific,
  // unlike the ctype<char> bits.
  ctype<wchar_t>::ctype(size_t __refs) : __ctype_abstract_base<wchar_t>(__refs)
  { }

  wchar_t
  ctype<wchar_t>::do_toupper(wchar_t __c) const
  { return towupper(__c); }

  const wchar_t*
  ctype<wchar_t>::do_toupper(wchar_t* __lo, const wchar_t* __hi) const
  {
    while (__lo < __hi)
      {
        *__lo = towupper(*__lo);
        ++__lo;
      }
    return __hi;
  }
  
  wchar_t
  ctype<wchar_t>::do_tolower(wchar_t __c) const
  { return towlower(__c); }
  
  const wchar_t*
  ctype<wchar_t>::do_tolower(wchar_t* __lo, const wchar_t* __hi) const
  {
    while (__lo < __hi)
      {
        *__lo = towlower(*__lo);
        ++__lo;
      }
    return __hi;
  }

  bool
  ctype<wchar_t>::
  do_is(mask __m, char_type __c) const
  { return static_cast<bool>(iswctype(__c, _M_convert_to_wmask(__m))); }
  
  const wchar_t* 
  ctype<wchar_t>::
  do_is(const wchar_t* __lo, const wchar_t* __hi, mask* __m) const
  {
    while (__lo < __hi && !this->is(*__m, *__lo))
      ++__lo;
    return __lo;
  }
  
  const wchar_t* 
  ctype<wchar_t>::
  do_scan_is(mask __m, const wchar_t* __lo, const wchar_t* __hi) const
  {
    while (__lo < __hi && !this->is(__m, *__lo))
      ++__lo;
    return __lo;
  }

  const wchar_t*
  ctype<wchar_t>::
  do_scan_not(mask __m, const char_type* __lo, const char_type* __hi) const
  {
    while (__lo < __hi && this->is(__m, *__lo) != 0)
      ++__lo;
    return __lo;
  }

  wchar_t
  ctype<wchar_t>::
  do_widen(char __c) const
  { return btowc(__c); }
  
  const char* 
  ctype<wchar_t>::
  do_widen(const char* __lo, const char* __hi, wchar_t* __dest) const
  {
    mbstate_t __state;
    memset(static_cast<void*>(&__state), 0, sizeof(mbstate_t));
    mbsrtowcs(__dest, &__lo, __hi - __lo, &__state);
    return __hi;
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
  do_narrow(const wchar_t* __lo, const wchar_t* __hi, char __dfault, 
	    char* __dest) const
  {
    mbstate_t __state;
    memset(static_cast<void*>(&__state), 0, sizeof(mbstate_t));
    size_t __len = __hi - __lo;
    size_t __conv = wcsrtombs(__dest, &__lo, __len, &__state);
    if (__conv == __len)
      *__dest = __dfault;
    return __hi;
  }

  template<>
  ctype_byname<wchar_t>::
  ctype_byname(const char* /*__s*/, size_t __refs)
  : ctype<wchar_t>(__refs) { }
#endif //  _GLIBCPP_USE_WCHAR_T
} // namespace std
