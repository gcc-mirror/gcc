// Locale support -*- C++ -*-

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

//
// ISO C++ 14882: 22.1  Locales
//

/** @file localefwd.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _CPP_BITS_LOCCORE_H
#define _CPP_BITS_LOCCORE_H	1

#pragma GCC system_header

#include <bits/c++config.h>
#include <bits/c++locale.h>     // Defines __c_locale, config-specific includes
#include <climits>		// For CHAR_BIT
#include <cctype>		// For isspace, etc.
#include <string> 		// For string.
#include <bits/functexcept.h>
#include <bits/atomicity.h>

namespace std
{
  // 22.1.1 Locale
  class locale;

  // 22.1.3 Convenience interfaces
  template<typename _CharT> 
    inline bool 
    isspace(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    isprint(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    iscntrl(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    isupper(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    islower(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    isalpha(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    isdigit(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    ispunct(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    isxdigit(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    isalnum(_CharT, const locale&);

  template<typename _CharT> 
    inline bool 
    isgraph(_CharT, const locale&);

  template<typename _CharT> 
    inline _CharT 
    toupper(_CharT, const locale&);

  template<typename _CharT> 
    inline _CharT 
    tolower(_CharT, const locale&);


  // 22.2.1 and 22.2.1.3 ctype
  class ctype_base;
  template<typename _CharT> 
    class ctype;
  template<> class ctype<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template<> class ctype<wchar_t>;
#endif
  template<typename _CharT> 
    class ctype_byname;
  // NB: Specialized for char and wchar_t in locale_facets.h.

  class codecvt_base;
  class __enc_traits;
  template<typename _InternT, typename _ExternT, typename _StateT>
    class codecvt;
  template<> class codecvt<char, char, mbstate_t>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template<> class codecvt<wchar_t, char, mbstate_t>;
#endif
  template<typename _InternT, typename _ExternT, typename _StateT>
    class codecvt_byname;

  // 22.2.2 and 22.2.3 numeric
  template<typename _CharT, typename _InIter = istreambuf_iterator<_CharT> >
    class num_get;
  template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
    class num_put;
  template<typename _CharT> class numpunct;
  template<typename _CharT> class numpunct_byname;

  // 22.2.4 collation
  template<typename _CharT> 
    class collate;
  template<typename _CharT> class 
    collate_byname;

  // 22.2.5 date and time
  class time_base;
  template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
    class time_get;
  template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
    class time_get_byname;
  template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
    class time_put;
  template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
    class time_put_byname;

  // 22.2.6 money
  class money_base;
  template<typename _CharT, typename _InIter =  istreambuf_iterator<_CharT> >
    class money_get;
  template<typename _CharT, typename _OutIter = ostreambuf_iterator<_CharT> >
    class money_put;
  template<typename _CharT, bool _Intl = false> 
    class moneypunct;
  template<typename _CharT, bool _Intl = false> 
    class moneypunct_byname;

  // 22.2.7 message retrieval
  class messages_base;
  template<typename _CharT> 
    class messages;
  template<typename _CharT> 
    class messages_byname;

  // 22.1.1 Class locale
  class locale
  {
  public:
    // Types:
    typedef unsigned int 	category;

    // Forward decls and friends:
    class facet;
    class id;
    class _Impl;

    friend class facet;
    friend class _Impl;

    template<typename _Facet>
      friend const _Facet& 
      use_facet(const locale&);
    
    template<typename _Facet>
      friend bool 
      has_facet(const locale&) throw();
 
    // Category values:
    // NB: Order must match _S_facet_categories definition in locale.cc
    static const category none		= 0;
    static const category ctype 	= 1L << 0;
    static const category numeric 	= 1L << 1;
    static const category collate  	= 1L << 2;
    static const category time 		= 1L << 3;
    static const category monetary 	= 1L << 4;
    static const category messages 	= 1L << 5;
    static const category all 		= (ctype | numeric | collate |
				 	   time  | monetary | messages);

    // Construct/copy/destroy:
    locale() throw();

    locale(const locale& __other) throw();

    explicit  
    locale(const char* __s);

    locale(const locale& __base, const char* __s, category __cat);

    locale(const locale& __base, const locale& __add, category __cat);

    template<typename _Facet>
      locale(const locale& __other, _Facet* __f);

    ~locale() throw();

    const locale&  
    operator=(const locale& __other) throw();

    template<typename _Facet>
      locale  
      combine(const locale& __other) const;

    // Locale operations:
    string 
    name() const;

    bool 
    operator==(const locale& __other) const throw ();

    inline bool  
    operator!=(const locale& __other) const throw ()
    { return !(this->operator==(__other));  }

    template<typename _Char, typename _Traits, typename _Alloc>
      bool  
      operator()(const basic_string<_Char, _Traits, _Alloc>& __s1,
		 const basic_string<_Char, _Traits, _Alloc>& __s2) const;

    // Global locale objects:
    static locale 
    global(const locale&);

    static const locale& 
    classic();

  private:
    // The (shared) implementation
    _Impl* 		_M_impl;  

    // The "C" reference locale
    static _Impl* 	_S_classic; 

    // Current global reference locale
    static _Impl* 	_S_global;  

    // Number of standard categories. For C++, these categories are
    // collate, ctype, monetary, numeric, time, and messages. These
    // directly correspond to ISO C99 macros LC_COLLATE, LC_CTYPE,
    // LC_MONETARY, LC_NUMERIC, and LC_TIME. In addition, POSIX (IEEE
    // 1003.1-2001) specifies LC_MESSAGES.
    static const size_t	_S_categories_size = 6;

    // In addition to the standard categories, the underlying
    // operating system is allowed to define extra LC_*
    // macros. For GNU systems, the following are also valid:
    // LC_PAPER, LC_NAME, LC_ADDRESS, LC_TELEPHONE, LC_MEASUREMENT,
    // and LC_IDENTIFICATION.
    static const size_t	_S_extra_categories_size = _GLIBCPP_NUM_CATEGORIES;

    // Names of underlying locale categories.  
    // NB: locale::global() has to know how to modify all the
    // underlying categories, not just the ones required by the C++
    // standard.
    static const char* 	_S_categories[_S_categories_size 
				      + _S_extra_categories_size];

    explicit 
    locale(_Impl*) throw();

    static inline void  
    _S_initialize()
    { 
      if (!_S_classic) 
	classic();  
    }

    static category  
    _S_normalize_category(category);

    void
    _M_coalesce(const locale& __base, const locale& __add, category __cat);
  };


  // Implementation object for locale 
  class locale::_Impl
  {
  public:
    // Friends.
    friend class locale;
    friend class locale::facet;

    template<typename _Facet>
      friend const _Facet&  
      use_facet(const locale&);

    template<typename _Facet>
      friend bool  
      has_facet(const locale&) throw();

  private:
    // Data Members.
    _Atomic_word			_M_references;
    facet** 				_M_facets;
    size_t 				_M_facets_size;

    char* 				_M_names[_S_categories_size
						 + _S_extra_categories_size];
    static const locale::id* const 	_S_id_ctype[];
    static const locale::id* const 	_S_id_numeric[];
    static const locale::id* const 	_S_id_collate[];
    static const locale::id* const 	_S_id_time[];
    static const locale::id* const 	_S_id_monetary[];
    static const locale::id* const 	_S_id_messages[];
    static const locale::id* const* const _S_facet_categories[];

    inline void 
    _M_add_reference() throw()
    { __atomic_add(&_M_references, 1); }

    inline void 
    _M_remove_reference() throw()
    {
      if (__exchange_and_add(&_M_references, -1) == 1)
	{
	  try 
	    { delete this; } 
	  catch(...) 
	    { }
	}
    }

    _Impl(const _Impl&, size_t);
    _Impl(const char*, size_t);
    _Impl(facet**, size_t, bool);

   ~_Impl() throw();

    _Impl(const _Impl&);  // Not defined.

    void 
    operator=(const _Impl&);  // Not defined.

    inline bool
    _M_check_same_name()
    {
      bool __ret = true;
      for (size_t __i = 0; 
	   __ret && __i < _S_categories_size + _S_extra_categories_size - 1; 
	   ++__i)
	__ret &= (strcmp(_M_names[__i], _M_names[__i + 1]) == 0);
      return __ret;
    }

    void 
    _M_replace_categories(const _Impl*, category);

    void 
    _M_replace_category(const _Impl*, const locale::id* const*);

    void 
    _M_replace_facet(const _Impl*, const locale::id*);

    void 
    _M_install_facet(const locale::id*, facet*);

    template<typename _Facet>
      inline void 
      _M_init_facet(_Facet* __facet)
      { _M_install_facet(&_Facet::id, __facet);  }
  };

  template<typename _Facet>
    locale::locale(const locale& __other, _Facet* __f)
    {
      _M_impl = new _Impl(*__other._M_impl, 1);
      _M_impl->_M_install_facet(&_Facet::id, __f);
      for (size_t __i = 0; 
	   __i < _S_categories_size + _S_extra_categories_size; ++__i)
	{
	  delete [] _M_impl->_M_names[__i];
	  char* __new = new char[2];
	  strcpy(__new, "*");
	  _M_impl->_M_names[__i] = __new;
	}
    }

  // 22.1.1.1.2  Class locale::facet
  class locale::facet
  {
  private:
    friend class locale;
    friend class locale::_Impl;

    _Atomic_word _M_references;

  protected:
    // Contains data from the underlying "C" library for default "C"
    // or "POSIX" locale.
    static __c_locale		     _S_c_locale;
    
    explicit 
    facet(size_t __refs = 0) throw();

    virtual 
    ~facet();

    static void
    _S_create_c_locale(__c_locale& __cloc, const char* __s, 
		       __c_locale __old = 0);

    static __c_locale
    _S_clone_c_locale(__c_locale& __cloc);

    static void
    _S_destroy_c_locale(__c_locale& __cloc);

  private:
    void 
    _M_add_reference() throw();

    void 
    _M_remove_reference() throw();

    facet(const facet&);  // Not defined.

    void 
    operator=(const facet&);  // Not defined.
  };


  // 22.1.1.1.3 Class locale::id
  class locale::id
  {
  private:
    friend class locale;
    friend class locale::_Impl;
    template<typename _Facet>
      friend const _Facet&  
      use_facet(const locale&);
    template<typename _Facet>
      friend bool           
      has_facet(const locale&) throw ();

    // NB: There is no accessor for _M_index because it may be used
    // before the constructor is run; the effect of calling a member
    // function (even an inline) would be undefined.
    mutable size_t 		_M_index;

    // Last id number assigned.
    static _Atomic_word 	_S_highwater;   

    void 
    operator=(const id&);  // Not defined.

    id(const id&);  // Not defined.

  public:
    // NB: This class is always a static data member, and thus can be
    // counted on to be zero-initialized.
    id();

    inline size_t
    _M_id() const
    {
      if (!_M_index)
	_M_index = 1 + __exchange_and_add(&_S_highwater, 1);
      return _M_index - 1;
    }
  };

  template<typename _Facet>
    const _Facet&
    use_facet(const locale& __loc);

  template<typename _Facet>
    bool
    has_facet(const locale& __loc) throw();
} // namespace std

#endif
