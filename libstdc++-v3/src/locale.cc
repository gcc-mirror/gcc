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
#include <bits/std_exception.h>
#include <bits/std_stdexcept.h>
#include <bits/std_locale.h>
#include <bits/std_istream.h>
#include <bits/std_ostream.h>
#include <bits/std_vector.h>
#include <bits/std_memory.h>      // for auto_ptr

namespace std {

  // locale::_Impl
  locale::_Impl::~_Impl() throw ()
  {
    std::vector<facet*>::iterator it = _M_facets->begin();
    for (; it != _M_facets->end(); ++it)
      (*it)->_M_remove_reference();
    delete _M_facets;
    delete _M_category_names;
  }

  locale::_Impl::_Impl(size_t __numfacets, size_t __refs)
  : _M_num_references(__refs - 1)
  , _M_facets(0)
  , _M_category_names(0)
  , _M_has_name(false)
  , _M_cached_name_ok(false)
  , _M_cached_name(string ("*"))
  { 
    typedef vector<facet*, allocator<facet*> > __vec_facet;
    typedef vector<string, allocator<string> > __vec_string;

    auto_ptr<__vec_facet> __pvf(new __vec_facet(__numfacets, (facet*)0));
    auto_ptr<__vec_string> __pcn(new __vec_string(_S_num_categories,
						  string("*")));
    _M_facets = __pvf.release();
    _M_category_names = __pcn.release();
  }
  
  locale::_Impl::_Impl(const _Impl& __other, size_t __refs)
  : _M_num_references(__refs)
  , _M_facets(0)
  , _M_category_names(0)
  , _M_has_name(__other._M_has_name)
  , _M_cached_name_ok(__other._M_cached_name_ok)
  , _M_cached_name(__other._M_cached_name)
  {
    typedef vector<facet*, allocator<facet*> > __vec_facet;
    typedef vector<string, allocator<string> > __vec_string;

    auto_ptr<__vec_facet> __pvf(new __vec_facet(*(__other._M_facets)));
    auto_ptr<__vec_string> 
      __pcn(new __vec_string(*(__other._M_category_names)));

    std::vector<facet*>::iterator __it = __pvf->begin();
    for (; __it != __pvf->end(); ++__it)
      (*__it)->_M_add_reference();

    // these must be last since in the presence of an exception, the 
    // destructor for 'this' won't run until AFTER execution has passed  
    // the closing brace of the constructor
    _M_facets = __pvf.release();
    _M_category_names = __pcn.release();
  }
  
  void
  locale::_Impl::_M_replace_categories(const _Impl* __other, category __cats)
  {
    assert((__cats & locale::all) && !(__cats & ~locale::all));
    
    unsigned mask = (locale::all & -(unsigned)locale::all);
    for (unsigned ix = 0; (-mask & __cats) != 0; ++ix, (mask <<= 1))
      {
	if (mask & __cats)
	  {
	    _M_replace_category(__other, _S_facet_categories[ix]);
	    (*_M_category_names)[ix] = (*(__other->_M_category_names))[ix];
	  }
      }
  }

  void
  locale::_Impl::_M_replace_category(const _Impl* __other,
				     const locale::id* const* __idpp)
  {
    for (; *__idpp; ++__idpp)
      _M_replace_facet(__other, *__idpp);
  }
  
  void
  locale::_Impl::_M_replace_facet(const _Impl* __other, 
				  const locale::id* __idp)
  {
    size_t __index = __idp->_M_index;
    if (__index == 0 
	|| __other->_M_facets->size() <= __index 
	|| (*(__other->_M_facets))[__index] == 0)
      throw runtime_error("no locale facet");
	
    _M_install_facet(__idp, (*(__other->_M_facets))[__index]); 
  }

  void
  locale::_Impl::_M_install_facet(const locale::id* __idp, facet* __fp)
  {
    if (__fp == 0)
      return;

    size_t& __index = __idp->_M_index;
    if (!__index)
      __index = ++locale::id::_S_highwater;  // XXX MT

    if (__index >= _M_facets->size())
      _M_facets->resize(__index + 1, 0);  // might throw
    facet*& __fpr = (*_M_facets)[__index];
    // order matters, here:
    __fp->_M_add_reference();
    if (__fpr) 
      __fpr->_M_remove_reference();
    __fpr = __fp;
  }

  // locale facet category descriptions
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
    &std::codecvt<char, char, mbstate_t>::id,
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::codecvt<wchar_t, char, mbstate_t>::id,
#endif
    0
  };

  const locale::id* const
  locale::_Impl::_S_id_monetary[] =
  {
    &std::moneypunct<char, false>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::moneypunct<wchar_t, false>::id,
#endif
    &std::moneypunct<char,true >::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::moneypunct<wchar_t,true >::id,
#endif
    &std::money_get<char>::id,        
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::money_get<wchar_t>::id,
#endif
    &std::money_put<char>::id,        
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::money_put<wchar_t>::id,
#endif
    0
  };

  const locale::id* const
  locale::_Impl::_S_id_numeric[] =
  {
    &std::numpunct<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::numpunct<wchar_t>::id,
#endif
    &std::num_get<char>::id,  
 #ifdef _GLIBCPP_USE_WCHAR_T
    &std::num_get<wchar_t>::id,
#endif
    &std::num_put<char>::id,  
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::num_put<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const
  locale::_Impl::_S_id_time[] =
  {
    &std::time_get<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::time_get<wchar_t>::id,
#endif
    &std::time_put<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::time_put<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const
  locale::_Impl::_S_id_messages[] =
  {
    &std::time_get<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::time_get<wchar_t>::id,
#endif
    &std::time_put<char>::id, 
#ifdef _GLIBCPP_USE_WCHAR_T
    &std::time_put<wchar_t>::id,
#endif
    0
  };
  
  const locale::id* const* const
  locale::_Impl::_S_facet_categories[] =
  {
    //  order must match the decl order in class locale.
    locale::_Impl::_S_id_collate,
    locale::_Impl::_S_id_ctype,
    locale::_Impl::_S_id_monetary,
    locale::_Impl::_S_id_numeric,
    locale::_Impl::_S_id_time,
    locale::_Impl::_S_id_messages,
    0
  };
  
  locale::_Impl* locale::_S_global;  // init'd to 0 before static ctors run
  locale::_Impl* locale::_S_classic; // init'd to 0 before static ctors run

  locale::locale(_Impl* __ip) throw ()
  : _M_impl(__ip)
  { __ip->_M_add_reference(); }

  locale::locale(const locale& __other, const locale& __one, category __cats)
  {
    __cats = _S_normalize_category(__cats);    // might throw
    _M_impl = new _Impl(*__other._M_impl, 1);  // might throw

    try { 
      _M_impl->_M_replace_categories(__one._M_impl, __cats); 
    }
    catch (...) { 
      _M_impl->_M_remove_reference(); 
      throw; 
    }

    _M_impl->_M_cached_name_ok = false;
    if (!__other._M_impl->_M_has_name)
      _M_impl->_M_has_name = false;
  }

  const locale&
  locale::operator=(const locale& __other) throw ()
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
  {
    // XXX not done
    return "*";
  }

  locale const&
  locale::classic()
  {
    static locale* __classic_locale;
    // XXX MT
    if (!_S_classic)
      {
	try {
	  _S_classic = _S_global = new _Impl(26u, 2u);
	  // one reference for _M_classic, one for _M_global
	  // (constructor for (*the_classic_locale) adds a third)
	    
	  // collate category
	  _S_classic->_M_init_facet(new std::collate<char>);
	  
	  // ctype category
	  _S_classic->_M_init_facet(new std::ctype<char>);
	  _S_classic->_M_init_facet(new std::codecvt<char, char, mbstate_t>);

	  // monetary category
	  _S_classic->_M_init_facet(new std::moneypunct<char, false>);
	  _S_classic->_M_init_facet(new std::moneypunct<char,true >);
	  _S_classic->_M_init_facet(new std::money_get<char>);
	  _S_classic->_M_init_facet(new std::money_put<char>);
	  
	  // numeric category
	  _S_classic->_M_init_facet(new std::numpunct<char>);
	  _S_classic->_M_init_facet(new std::num_get<char>);
	  _S_classic->_M_init_facet(new std::num_put<char>);
	  
	  // time category
	  _S_classic->_M_init_facet(new std::time_get<char>);
	  _S_classic->_M_init_facet(new std::time_put<char>);
	  
	  // messages category
	  _S_classic->_M_init_facet(new std::messages<char>);

#ifdef  _GLIBCPP_USE_WCHAR_T
	  _S_classic->_M_init_facet(new std::collate<wchar_t>);
	  _S_classic->_M_init_facet(new std::ctype<wchar_t>);
	  _S_classic->_M_init_facet(new std::codecvt<wchar_t, char, mbstate_t>);
	  _S_classic->_M_init_facet(new std::moneypunct<wchar_t, false>);
	  _S_classic->_M_init_facet(new std::moneypunct<wchar_t,true >);
	  _S_classic->_M_init_facet(new std::money_get<wchar_t>);
	  _S_classic->_M_init_facet(new std::money_put<wchar_t>);
	  _S_classic->_M_init_facet(new std::numpunct<wchar_t>);
	  _S_classic->_M_init_facet(new std::num_get<wchar_t>);
	  _S_classic->_M_init_facet(new std::num_put<wchar_t>);
	  _S_classic->_M_init_facet(new std::time_get<wchar_t>);
	  _S_classic->_M_init_facet(new std::time_put<wchar_t>);
	  _S_classic->_M_init_facet(new std::messages<wchar_t>);
#endif	  

	  // finesse static init order hassles
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
  locale::_S_normalize_category(int __cats) 
  {
    if ((__cats & all) && !(__cats & ~all))
      return __cats;

    // NB: May be a C-style "LC_ALL" category; convert.
    switch (__cats)
      {
      case LC_COLLATE:  return collate; 
      case LC_CTYPE:    return ctype;
      case LC_MONETARY: return monetary;
      case LC_NUMERIC:  return numeric;
      case LC_TIME:     return time; 
#ifdef _GLIBCPP_HAVE_LC_MESSAGES
      case LC_MESSAGES: return messages;
#endif	
      case LC_ALL:      return all;
      }
    
    // XXX should throw derived class here
    throw runtime_error("bad locale category");
    /* NOTREACHED */
  }

  locale::facet::facet(size_t __refs) throw ()
  : _M_num_references(__refs - 1) 
  { }

  void  
  locale::facet::_M_add_reference() throw ()
  { 
    if (this) 
      ++_M_num_references; 
  }                     // XXX MT

  void  
  locale::facet::_M_remove_reference() throw ()
  {
    if (this && _M_num_references-- == 0)
      {
        try { 
	  delete this; 
	}  // XXX MT
	catch (...) { 
	}
      }
  }

  char const* 
  _Bad_use_facet::what() const throw()
  { return "bad_cast thrown from use_facet"; }

  _Bad_use_facet::~_Bad_use_facet() throw() {}
  
  size_t locale::id::_S_highwater;  // init'd to 0 by linker


  // Platform-specific initialization code for ctype tables.
  #include <ctype.cc>

  locale::id ctype<char>::id;

  ctype<char>::~ctype()
  { if (_M_del) delete[] table(); }

  char
  ctype<char>::do_widen(char __c) const
  { return __c; }
  
  const char* 
  ctype<char>::
  do_widen(const char* __low, const char* __high, char* __dest) const
  {
    memcpy(__dest, __low, __high - __low);
    return __high;
  }
  
  char
  ctype<char>::do_narrow(char __c, char /*__dfault*/) const
  { return __c; }
  
  const char* 
  ctype<char>::
  do_narrow(const char* __low, const char* __high, char /*__dfault*/,
	    char* __dest) const
  {
    memcpy(__dest, __low, __high - __low);
    return __high;
  }

  ctype_byname<char>::ctype_byname(const char* /*__s*/, size_t __refs)
  : ctype<char> (new mask[table_size], true, __refs)
  { }

  locale::id codecvt<char, char, mbstate_t>::id;

  codecvt<char, char, mbstate_t>::codecvt(size_t __refs)
  : _Codecvt<char, char, mbstate_t> (__refs)
  { }

  codecvt<char, char, mbstate_t>::~codecvt() { }
  
  codecvt_base::result
  codecvt<char, char, mbstate_t>::
  do_out(state_type& /*__state*/, const intern_type* __from, 
	 const intern_type* __from_end, const intern_type*& __from_next,
	 extern_type* __to, extern_type* __to_end, 
	 extern_type*& __to_next) const
  { 
    size_t __sizefrom = __from_end - __from;
    size_t __sizeto = __to_end - __to;
    size_t __length = __sizefrom <= __sizeto ? __sizefrom : __sizeto;
    memcpy(__to, __from, __length);
    __from_next = __from; 
    __to_next = __to;
    return noconv;  
  }
  
  codecvt_base::result
  codecvt<char, char, mbstate_t>::
  do_unshift(state_type& /*__state*/, extern_type* __to,
             extern_type* /*__to_limit*/,
	     extern_type*& __to_next) const
  { 
    __to_next = __to; 
    return noconv; 
  }
  
  codecvt_base::result
  codecvt<char, char, mbstate_t>::
  do_in(state_type& /*__state*/, const extern_type* __from, 
	const extern_type* __from_end, const extern_type*& __from_next,
	intern_type* __to, intern_type* __to_end, 
	intern_type*& __to_next) const
  { 
    size_t __sizefrom = __from_end - __from;
    size_t __sizeto = __to_end - __to;
    size_t __length = __sizefrom <= __sizeto ? __sizefrom : __sizeto;
    memcpy(__to, __from, __length);
    __from_next = __from; 
    __to_next = __to;
    return noconv;  
  }
  

  int 
  codecvt<char, char, mbstate_t>::do_encoding() const throw ()
  { return 1; }
  
  bool 
  codecvt<char, char, mbstate_t>::do_always_noconv() const throw ()
  { return true; }
  
  int 
  codecvt<char, char, mbstate_t>::
  do_length (const state_type& /*__state*/, const extern_type* __from,
	     const extern_type* __end, size_t __max) const
  { return (__max < size_t(__end - __from)) ? __max : __end - __from; }
  
  int 
  codecvt<char, char, mbstate_t>::do_max_length() const throw ()
  { return 1; }
  
  codecvt_byname<char, char, mbstate_t>::
  codecvt_byname(const char* /*__s*/, size_t __refs)
  : codecvt<char, char, mbstate_t>(__refs)
  { }

  codecvt_byname<char, char, mbstate_t>::~codecvt_byname() { }

  locale::id collate<char>::id;

  collate<char>::collate(size_t __refs)
  : _Collate<char> (__refs)
  { }
  
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
  collate<char>::do_transform(const char* __lo, const char* __hi) const
  { return string(__lo, __hi - __lo); }
  
  long
  collate<char>::do_hash(const char* __lo, const char* __hi) const
  {
    unsigned long __val = 0xdeadbeef;
    for (; __lo < __hi; ++__lo)
      __val = *__lo ^ ((__val << 7) & 
		   (__val >> (numeric_limits<unsigned long>::digits - 1)));
    return __val;
  }
  
  collate_byname<char>::collate_byname(const char* /*__s*/, size_t __refs)
  : collate<char> (__refs)
  { }

  numpunct_byname<char>::numpunct_byname(const char* /*__s*/, size_t __refs)
  : numpunct<char> (__refs)
  { }

  moneypunct_byname<char, false>::
  moneypunct_byname(const char* /*__s*/, size_t __refs)
  : moneypunct<char, false> (__refs)
  { }
  
  moneypunct_byname<char, true>::
  moneypunct_byname(const char* /*__s*/, size_t __refs)
  : moneypunct<char, true> (__refs)
  { }
  
  messages_byname<char>::messages_byname(const char* /*__s*/, size_t __refs)
  : messages<char> (__refs)
  { }

#ifdef _GLIBCPP_USE_WCHAR_T  
  locale::id ctype<wchar_t>::id;

  ctype<wchar_t>::~ctype() { }

  bool
  ctype<wchar_t>::do_is(mask __m, char_type __c) const
  { 
    return ((static_cast<__table_type>(__c) < _S_table_size) 
	    && (_M_ctable[__c] & __m)); 
  }
  
  const wchar_t* 
  ctype<wchar_t>::do_is(const wchar_t* __low, const wchar_t* __high,
			mask* __vec) const
  {
    for (; __low < __high; ++__low, ++__vec)
      *__vec = ((static_cast<__table_type>(*__low) < _S_table_size) 
		? _M_ctable[*__low] : mask(0));
    return __high;
  }
  
  const wchar_t* 
  ctype<wchar_t>::do_scan_is(mask __m, const wchar_t* __low,
			     const wchar_t* __high) const
  {
    while (__low < __high 
	   && (_S_table_size < static_cast<__table_type>(*__low) 
	       || !(_M_ctable[*__low] & __m)))
      ++__low;
    return __low;
  }

  const wchar_t*
  ctype<wchar_t>::do_scan_not(mask __m, const char_type* __low,
			      const char_type* __high) const
  {
    while (__low < __high 
	   && static_cast<__table_type>(*__low) < _S_table_size 
	   && (_M_ctable[*__low] & __m))
      ++__low;
    return __low;
  }

  wchar_t
  ctype<wchar_t>::do_widen(char __c) const
  { return static_cast<wchar_t>((unsigned char)__c); }
  
  const char* 
  ctype<wchar_t>::do_widen(const char* __low, const char* __high,
			      wchar_t* __dest) const
  {
    while (__low < __high)
      *__dest++ = static_cast<wchar_t>((unsigned char)*__low++);
    return __high;
  }

  char
  ctype<wchar_t>::do_narrow(wchar_t __c, char __dfault) const
  { 
    return ((static_cast<__table_type>(__c) < _S_table_size) 
	    ? static_cast<char>(__c) : __dfault); 
  }

  const wchar_t*
  ctype<wchar_t>::do_narrow(const wchar_t* __low, const wchar_t* __high,
			    char __dfault, char* __dest) const
  {
    for (; __low < __high; ++__dest, ++__low)
      *__dest = (static_cast<__table_type>(*__low) < _S_table_size) 
		? static_cast<char>(*__low) : __dfault;
    return __high;
  }

  ctype_byname<wchar_t>::ctype_byname(const char* /*__s*/, size_t __refs)
  : ctype<wchar_t> (__refs)
  { }

  locale::id codecvt<wchar_t, char, mbstate_t>::id;

  codecvt<wchar_t, char, mbstate_t>::codecvt (size_t __refs)
  : _Codecvt<wchar_t, char, mbstate_t> (__refs)
  { }

  codecvt<wchar_t, char, mbstate_t>::~codecvt() { }
  
  codecvt_base::result
  codecvt<wchar_t, char, mbstate_t>::
  do_out(state_type& /*__state*/, const intern_type* __from, 
	 const intern_type* __from_end, const intern_type*& __from_next,
	 extern_type* __to, extern_type* __to_limit,
	 extern_type*& __to_next) const
  {
    for (; __from < __from_end && __to < __to_limit; ++__from, ++__to)
      *__to = static_cast<char>(*__from);
    __from_next = __from; __to_next = __to;
    return __from == __from_end ? ok : partial;
  }
  
  codecvt_base::result
  codecvt<wchar_t, char, mbstate_t>::
  do_unshift (state_type& /*__state*/, extern_type* __to,
              extern_type* /*__to_limit*/,
	      extern_type*& __to_next) const
  {
    __to_next = __to;
    return noconv;
  }
  
  codecvt_base::result
  codecvt<wchar_t, char, mbstate_t>::
  do_in(state_type& /*__state*/, const extern_type* __from, 
	const extern_type* __from_end, const extern_type*& __from_next,
	intern_type* __to, intern_type* __to_limit,
	intern_type*& __to_next) const
  {
    for (; __from < __from_end && __to < __to_limit; ++__from, ++__to)
      *__to = static_cast<wchar_t>(*__from);
    __from_next = __from; 
    __to_next = __to;
    return __from == __from_end ? ok : partial;
  }
  
  int codecvt<wchar_t, char, mbstate_t>::do_encoding() const throw ()
  { return 1; }
  
  bool 
  codecvt<wchar_t, char, mbstate_t>::do_always_noconv() const throw ()
  { return false; }
  
  int 
  codecvt<wchar_t, char, mbstate_t>::
  do_length(const state_type& /*__state*/, const extern_type* __from,
	    const extern_type* __end, size_t __max) const
  { return (__max < size_t(__end - __from)) ? __max : __end - __from; }
  
  int 
  codecvt<wchar_t, char, mbstate_t>::do_max_length() const throw ()
  { return 1; }

  codecvt_byname<wchar_t, char, mbstate_t>::
  codecvt_byname(const char* /*__s*/, size_t __refs)
  : codecvt<wchar_t, char, mbstate_t> (__refs)
  {
  }
  
  codecvt_byname<wchar_t, char, mbstate_t>::~codecvt_byname() {}

  locale::id collate<wchar_t>::id;

  collate<wchar_t>::collate(size_t __refs)
  : _Collate<wchar_t> (__refs)
  { }
  
  collate<wchar_t>::~collate() { }

  int collate<wchar_t>::
  do_compare(const wchar_t* /*__lo1*/, const wchar_t* /*__hi1*/,
	     const wchar_t* /*__lo2*/, const wchar_t* /*__hi2*/) const
  {
    return 0; // XXX not done
  }
  
  wstring collate<wchar_t>::do_transform(const wchar_t* /*__lo*/, 
					 const wchar_t* /*__hi*/) const
  {
    return wstring(); // XXX not done
  }
  
  long collate<wchar_t>::
  do_hash(const wchar_t* /*__lo*/, const wchar_t* /*__hi*/) const
  {
    return 0; // XXX not done
  }

  numpunct_byname<wchar_t>::numpunct_byname(const char* /*__s*/, size_t __refs)
  : numpunct<wchar_t> (__refs)
  { }

  collate_byname<wchar_t>::collate_byname(const char* /*__s*/, size_t __refs)
  : collate<wchar_t> (__refs)
  { }
  
  moneypunct_byname<wchar_t, false>::
  moneypunct_byname(const char* /*__s*/, size_t __refs)
  : moneypunct<wchar_t, false> (__refs)
  { }
  
  moneypunct_byname<wchar_t, true>::
  moneypunct_byname(const char* /*__s*/, size_t __refs)
  : moneypunct<wchar_t, true> (__refs)
  { }
    
  messages_byname<wchar_t>::messages_byname(const char* /*__s*/, size_t __refs)
  : messages<wchar_t> (__refs)
  { }
#endif //  _GLIBCPP_USE_WCHAR_T

} // namespace std






