// Locale support -*- C++ -*-

// Copyright (C) 1997-2000 Free Software Foundation, Inc.
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

#ifndef _CPP_BITS_LOCCORE_H
#define _CPP_BITS_LOCCORE_H	1

#include <bits/c++config.h>
#include <bits/std_climits.h>	// For CHAR_BIT
#include <bits/std_string.h> 	// For string
#include <bits/std_cctype.h>	// For isspace, etc.

namespace std
{

  // _Count_ones: compile-time computation of number of 1-bits in a value N
  // This takes only 5 (or 6) instantiations, doing recursive descent
  // in parallel -- ncm
  template<unsigned _Num, int _Shift = (sizeof(unsigned) * CHAR_BIT)/2,
           unsigned _Mask = (~0u >> _Shift) >
    struct _Count_ones;

  template<unsigned _Num, unsigned _Mask>
    struct _Count_ones<_Num,0,_Mask> 
    { static const unsigned _S_count = _Num; };

  template<unsigned _Num, int _Shift, unsigned _Mask>
    struct _Count_ones 
    {
      static const unsigned _S_halfcount =
        _Count_ones<_Num, _Shift/2, (_Mask^((~_Mask)>>(_Shift/2))) >::_S_count;
      static const unsigned _S_count
      = (_S_halfcount&_Mask) + ((_S_halfcount>>_Shift)&_Mask);
    };

  // 22.1.1 Locale
  template<typename _Tp> class allocator;
  template<typename _Tp, typename _Alloc> class vector;
  class locale;

  template<typename _Facet>
    const _Facet&  
    use_facet(const locale&);

  template<typename _Facet>
    bool           
    has_facet(const locale&) throw();

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
  // NB: Specialized for char and wchar_t in locfacets.h.

  class codecvt_base;
  template<typename _InternT, typename _ExternT, typename _StateT>
    class codecvt;
  template<> class codecvt<char, char, mbstate_t>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template<> class codecvt<wchar_t, char, mbstate_t>;
#endif

  template<typename _InternT, typename _ExternT, typename _StateT>
    class codecvt_byname;
  template<> class codecvt_byname<char, char, mbstate_t>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template<> class codecvt_byname<wchar_t, char, mbstate_t>;
#endif

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
  template<> class collate<char>;
#ifdef _GLIBCPP_USE_WCHAR_T
  template<> class collate<wchar_t>;
#endif
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
    // Forwrd decls and friends:
    class _Impl;
    friend _Impl;

    template<typename _Facet>
      friend const _Facet& 
      use_facet(const locale&);
    
    template<typename _Facet>
      friend bool 
      has_facet(const locale&) throw();
 
  public:
    // Types:
    class facet;
    class id;
    typedef int category;

    // Category values:
    // NB much depends on the order in which these appear:
    static const category none		= 0;
    static const category collate  	= 0x0100;
    static const category ctype 	= 0x0200;
    static const category monetary 	= 0x0400;
    static const category numeric 	= 0x0800;
    static const category time 		= 0x1000;
    static const category messages 	= 0x2000;
    static const category all 		= (collate | ctype | monetary |
				 	   numeric | time  | messages);

    // Construct/copy/destroy:
    inline  
    locale() throw();

    inline  
    locale(const locale& __other) throw();

    explicit  
    locale(const char* __std_name);

    locale(const locale& __other, const char* __std_name, category __cats);

    locale(const locale& __other, const locale& __one, category __cats);

    template<typename _Facet>
      locale(const locale& __other, _Facet* __f);

    inline  
    ~locale() throw();

    const locale&  
    operator=(const locale& __other) throw();

    template<typename _Facet>
      locale  
      combine(const locale& __other);

    // Locale operations:
    string 
    name() const;

    bool 
    operator==(const locale& __other) const throw ();

    inline bool  
    operator!=(const locale& __other) const throw ()
    { return !(operator==(__other));  }

    template<typename _Char, typename _Traits, typename _Alloc>
      bool  
      operator()(const basic_string<_Char,_Traits,_Alloc>& __s1,
		 const basic_string<_Char,_Traits,_Alloc>& __s2) const;

    // Global locale objects:
    static locale 
    global(const locale&);

    static const locale& 
    classic();

  private:
    _Impl* _M_impl;  // The (shared) implementation

    static _Impl* _S_classic; // The one true C reference locale
    static _Impl* _S_global;  // Current global reference locale

    explicit 
    locale(_Impl*) throw();

    static inline void  
    _S_initialize()
    { if (!_S_classic) classic();  }

    static int  
    _S_normalize_category(int);

    static const int 
    _S_num_categories = _Count_ones<all>::_S_count;
  };


  // locale implementation object
  class locale::_Impl
  {
    typedef vector<facet*, allocator<facet*> > __vec_facet;
    typedef vector<string, allocator<string> > __vec_string;

    // Friends:
    friend class locale;
    friend class facet;

    template<typename _Facet>
      friend const _Facet&  
      use_facet(const locale&);

    template<typename _Facet>
      friend bool  
      has_facet(const locale&) throw();

    size_t _M_num_references;
    __vec_facet* _M_facets;
    __vec_string* _M_category_names;
    bool _M_has_name;
    bool _M_cached_name_ok;
    string _M_cached_name;

    inline void 
    _M_add_reference() throw()
    { ++_M_num_references; }  // XXX MT

    inline void 
    _M_remove_reference() throw()
    {
      if (_M_num_references-- == 0)  // XXX MT
	{
	  try { 
	    delete this; 
	  } 
	  catch(...) { 
	  }
	}
    }

    _Impl(const _Impl&, size_t __refs);
    _Impl(const _Impl&, const string&, category, size_t __refs);
    _Impl(size_t __facets, size_t __refs);
   ~_Impl() throw();

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

    void 
    _M_construct_collate(const char*);

    void 
    _M_construct_ctype(const char*);

    void 
    _M_construct_monetary(const char*);

    void 
    _M_construct_numeric(const char*);

    void 
    _M_construct_time(const char*);

    void 
    _M_construct_messages(const char*);

    category 
    _M_normalize_category_names(const string&, category __cats);

    static const locale::id* const _S_id_collate[];
    static const locale::id* const _S_id_ctype[];
    static const locale::id* const _S_id_monetary[];
    static const locale::id* const _S_id_numeric[];
    static const locale::id* const _S_id_time[];
    static const locale::id* const _S_id_messages[];
    static const locale::id* const* const _S_facet_categories[];
  };

  // class locale inlines, that need declaration of locale::_Imp
  locale::locale() throw()
  { 
    _S_initialize(); 
    (_M_impl = _S_global)->_M_add_reference(); 
  } // XXX MT

  locale::locale(const locale& __other) throw()
  { (_M_impl = __other._M_impl)->_M_add_reference(); }

  template<typename _Facet>
    locale::locale(const locale& __other, _Facet* __f)
    {
      _M_impl = new _Impl(*__other._M_impl, 0);
      _M_impl->_M_install_facet(&_Facet::id, __f);
      _M_impl->_M_has_name = false;
    }

  locale::~locale() throw()
  { _M_impl->_M_remove_reference(); }

  // 22.1.1.1.2  Class locale::facet
  class locale::facet
  {
    friend class locale;
    friend class locale::_Impl;

  protected:
    explicit 
    facet(size_t __refs = 0) throw();

    virtual 
    ~facet() {};

  private:
    size_t _M_num_references;

    void 
    _M_add_reference() throw();

    void 
    _M_remove_reference() throw();

    facet(const facet&);  // not defined

    void 
    operator=(const facet&);  // not defined
  };


  // 22.1.1.1.3 Class locale::id
  class locale::id
  {
    friend class locale;
    friend class locale::_Impl;
    template<typename _Facet>
      friend const _Facet&  
      use_facet(const locale&);
    template<typename _Facet>
      friend bool           
      has_facet(const locale&) throw ();
  public:
    id() {};
  private:
    // NB: there is no accessor for _M_index because it may be used
    // before the constructor is run; the effect of calling a member
    // function (even an inline) would be undefined.
    mutable size_t _M_index;
    static size_t _S_highwater;   // last id number assigned

    void 
    operator=(const id&);  // not defined

    id(const id&);  // not defined
  };

  template<typename _Facet>
    const _Facet&
    use_facet(const locale& __loc);

  template<typename _Facet>
    bool
    has_facet(const locale& __loc) throw();

} // namespace std

#endif	/* _CPP_BITS_LOCCORE_H */

// Local Variables:
// mode:c++
// End:

