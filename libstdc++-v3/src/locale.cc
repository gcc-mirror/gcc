// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
// Free Software Foundation, Inc.
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

#include <clocale>
#include <cstring>
#include <cassert>
#include <cctype>
#include <cwctype>     // For towupper, etc.
#include <limits>
#include <exception>
#include <locale>
#include <istream>
#include <ostream>
#include <bits/atomicity.h>

namespace std 
{
  // Defined in globals.cc.
  extern locale 		c_locale;
  extern locale::_Impl 		c_locale_impl;
  extern locale::facet**	facet_vec;

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

  // Definitions for locale::id of standard facets that are specialized.
  locale::id ctype<char>::id;
  locale::id codecvt<char, char, mbstate_t>::id;

#ifdef _GLIBCPP_USE_WCHAR_T  
  locale::id ctype<wchar_t>::id;
  locale::id codecvt<wchar_t, char, mbstate_t>::id;
#endif

  // Definitions for static const data members of locale::id
  _Atomic_word locale::id::_S_highwater;  // init'd to 0 by linker

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
    &__timepunct<char>::id, 
    &time_get<char>::id, 
    &time_put<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &__timepunct<wchar_t>::id, 
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
    locale::_Impl::_S_id_monetary,
    locale::_Impl::_S_id_time,
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
  }

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
	else if (strcmp(__s, "") == 0)
	  _M_impl = new _Impl(setlocale(LC_ALL, NULL), 1);
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
    if (_S_global->_M_check_same_name() 
	&& (strcmp(_S_global->_M_names[0], "*") != 0))
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
    // Need some kind of separator character. This one was pretty much
    // arbitrarily chosen as to not conflict with glibc locales: the
    // exact formatting is not set in stone.
    const char __separator = '|';

    string __ret;
    if (_M_impl->_M_check_same_name())
      __ret = _M_impl->_M_names[0];
    else
      {
	for (size_t i = 0; i < _S_num_categories; ++i)
	  {
	    __ret += __separator;
	    __ret += _M_impl->_M_names[i];
	  }
      }
    return __ret;
  }

  const locale&
  locale::classic()
  {
    static _STL_mutex_lock __lock __STL_MUTEX_INITIALIZER;
    _STL_auto_lock __auto(__lock);

    if (!_S_classic)
      {
	try 
	  {
	    // 26 Standard facets, 2 references.
	    // One reference for _M_classic, one for _M_global
	    facet** f = new(&facet_vec) facet*[_GLIBCPP_NUM_FACETS];
	    for (size_t __i = 0; __i < _GLIBCPP_NUM_FACETS; ++__i)
	      f[__i] = 0;

	    _S_classic = new (&c_locale_impl) _Impl(f, 2, true);
	    _S_global = _S_classic; 	    
	    new (&c_locale) locale(_S_classic);
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
    return c_locale;
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

  __c_locale
  locale::facet::_S_c_locale;
  
  locale::facet::
  ~facet() { }

  locale::facet::
  facet(size_t __refs) throw() : _M_references(__refs) 
  { 
    if (!_S_c_locale)
      _S_create_c_locale(_S_c_locale, "C");
  }

  void  
  locale::facet::
  _M_add_reference() throw()
  { __atomic_add(&_M_references, 1); }

  void  
  locale::facet::
  _M_remove_reference() throw()
  {
    if (__exchange_and_add(&_M_references, -1) == 0)
      {
        try 
	  { delete this; }  
	catch (...) 
	  { }
      }
  }
  
  locale::id::id() 
  { }

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
  { 
    if (_M_c_locale_ctype != _S_c_locale)
      _S_destroy_c_locale(_M_c_locale_ctype);
    if (_M_del) 
      delete[] this->table(); 
  }

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

#ifdef _GLIBCPP_USE_WCHAR_T
  ctype<wchar_t>::ctype(size_t __refs) 
  : __ctype_abstract_base<wchar_t>(__refs)
  { _M_c_locale_ctype = _S_c_locale; }

  ctype<wchar_t>::ctype(__c_locale __cloc, size_t __refs) 
  : __ctype_abstract_base<wchar_t>(__refs) 
  { _M_c_locale_ctype = _S_clone_c_locale(__cloc); }

  ctype<wchar_t>::~ctype() 
  { 
    if (_M_c_locale_ctype != _S_c_locale)
      _S_destroy_c_locale(_M_c_locale_ctype); 
  }

  template<>
    ctype_byname<wchar_t>::ctype_byname(const char* __s, size_t __refs)
    : ctype<wchar_t>(__refs) 
    { 	
      if (_M_c_locale_ctype != _S_c_locale)
	_S_destroy_c_locale(_M_c_locale_ctype);
      _S_create_c_locale(_M_c_locale_ctype, __s); 
    }
#endif

  // Definitions for static const data members of time_base
  template<> 
    const char*
    __timepunct<char>::_S_timezones[14] =
    { 
      "GMT", "HST", "AKST", "PST", "MST", "CST", "EST", "AST", "NST", "CET", 
      "IST", "EET", "CST", "JST"  
    };
 
#ifdef _GLIBCPP_USE_WCHAR_T
  template<> 
    const wchar_t*
    __timepunct<wchar_t>::_S_timezones[14] =
    { 
      L"GMT", L"HST", L"AKST", L"PST", L"MST", L"CST", L"EST", L"AST", 
      L"NST", L"CET", L"IST", L"EET", L"CST", L"JST"  
    };
#endif

  // Definitions for static const data members of money_base
  const money_base::pattern 
  money_base::_S_default_pattern =  { {symbol, sign, none, value} };

  template<>
    const ctype<char>&
    use_facet<ctype<char> >(const locale& __loc)
    {
      size_t __i = ctype<char>::id._M_id();
      const locale::_Impl* __tmp = __loc._M_impl;
      return static_cast<const ctype<char>&>(*(__tmp->_M_facets[__i]));
    }

#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    const ctype<wchar_t>&
    use_facet<ctype<wchar_t> >(const locale& __loc)
    {
      size_t __i = ctype<wchar_t>::id._M_id();
      const locale::_Impl* __tmp = __loc._M_impl;
      return static_cast<const ctype<wchar_t>&>(*(__tmp->_M_facets[__i]));
    }
#endif

  const char __num_base::_S_atoms[] = "0123456789eEabcdfABCDF";

  bool
  __num_base::_S_format_float(const ios_base& __io, char* __fptr, char __mod,
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
    if (__mod)
      *__fptr++ = __mod;
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
  
  void
  __num_base::_S_format_int(const ios_base& __io, char* __fptr, char __mod, 
			    char __modl)
  {
    ios_base::fmtflags __flags = __io.flags();
    *__fptr++ = '%';
    // [22.2.2.2.2] Table 60
    if (__flags & ios_base::showpos)
      *__fptr++ = '+';
    if (__flags & ios_base::showbase)
      *__fptr++ = '#';
    *__fptr++ = 'l';

    // For long long types.
    if (__modl)
      *__fptr++ = __modl;

    ios_base::fmtflags __bsefield = __flags & ios_base::basefield;
    if (__bsefield == ios_base::hex)
      *__fptr++ = (__flags & ios_base::uppercase) ? 'X' : 'x';
    else if (__bsefield == ios_base::oct)
      *__fptr++ = 'o';
    else
      *__fptr++ = __mod;
    *__fptr = '\0';
  }
} // namespace std

