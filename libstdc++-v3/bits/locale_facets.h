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

// Warning: this file is not meant for user inclusion.  Use <locale>.

#ifndef _CPP_BITS_LOCFACETS_H
#define _CPP_BITS_LOCFACETS_H	1

#include <bits/std_ctime.h>	// For struct tm
#include <bits/std_typeinfo.h> 	// For bad_cast, which shouldn't be here.
#include <bits/std_ios.h>	// For ios_base

namespace std
{

  // XXX This function is to be specialized for the "required" facets to 
  // be constructed lazily.   The specializations must be declared after 
  // the definitions of the facets themselves; but they shouldn't be 
  // inline.  Corresponding new's in locale::classic() should then be 
  // eliminated.  Note that ctype<> should not get this treatment; 
  // see the use_facet<> specializations below.
  //
  struct _Bad_use_facet : public bad_cast 
  {
    _Bad_use_facet() throw() {}

    _Bad_use_facet(_Bad_use_facet const&  __b) throw() 
    : bad_cast(__b) { }

    _Bad_use_facet& 
    operator=(_Bad_use_facet const& __b) throw() 
    { 
      static_cast<bad_cast*>(this)->operator=(__b); 
      return *this; 
    }

    virtual char const* 
    what() const throw();

    virtual 
    ~_Bad_use_facet() throw();
  };

  template<typename _Facet>
    const _Facet& 
    _Use_facet_failure_handler(const locale&)
    { throw _Bad_use_facet(); }

  // 22.2.1  The ctype category
  // Include host-specific ctype enums for ctype_base.
  #include <bits/ctype_base.h>

  // 22.2.1.1  Template class ctype
  // _Ctype_nois is the common base for ctype<char>.  It lacks "do_is"
  // and related virtuals.  These are filled in by _Ctype, below.
  template<typename _CharT>
    class _Ctype_nois : public locale::facet, public ctype_base
    {
      // Types:
      typedef _CharT char_type;

    public:
      char_type 
      toupper(char_type __c) const
      { return this->do_toupper(__c); }

      const char_type*
      toupper(char_type *__low, const char_type* __high) const
      { return this->do_toupper(__low, __high); }

      char_type 
      tolower(char_type __c) const
      { return this->do_tolower(__c); }

      const char_type*
      tolower(char_type* __low, const char_type* __high) const
      { return this->do_tolower(__low, __high); }

      char_type 
      widen(char __c) const
      { return this->do_widen(__c); }

      const char*
      widen(const char* __low, const char* __high, char_type* __to) const
      { return this->do_widen(__low, __high, __to); }

      char 
      narrow(char_type __c, char __dfault) const
      { return this->do_narrow(__c, __dfault); }

      const char_type*
      narrow(const char_type* __low, const char_type* __high,
	      char __dfault, char *__to) const
      { return this->do_narrow(__low, __high, __dfault, __to); }

    protected:
      explicit 
      _Ctype_nois(size_t __refs = 0): locale::facet(__refs) { }

      virtual 
      ~_Ctype_nois() { }
      
      virtual char_type 
      do_toupper(char_type) const = 0;

      virtual const char_type*
      do_toupper(char_type* __low, const char_type* __high) const = 0;

      virtual char_type 
      do_tolower(char_type) const = 0;

      virtual const char_type*
      do_tolower(char_type* __low, const char_type* __high) const = 0;
      
      virtual char_type 
      do_widen(char) const = 0;

      virtual const char*
      do_widen(const char* __low, const char* __high,
	       char_type* __dest) const = 0;

      virtual char 
      do_narrow(char_type, char __dfault) const = 0;

      virtual const char_type*
      do_narrow(const char_type* __low, const char_type* __high,
		 char __dfault, char* __dest) const = 0;
    };


  template<typename _CharT>
    class _Ctype : public _Ctype_nois<_CharT>
    {
    public:

      // Types:
      typedef _CharT 					char_type;
      typedef typename _Ctype_nois<_CharT>::mask 	mask;

      bool 
      is(mask __m, char_type __c) const
      { return this->do_is(__m, __c); }

      const char_type*
      is(const char_type *__lo, const char_type *__hi, mask *__vec) const   
      { return this->do_is(__lo, __hi, __vec); }

      const char_type*
      scan_is(mask __m, const char_type* __lo, const char_type* __hi) const
      { return this->do_scan_is(__m, __lo, __hi); }

      const char_type*
      scan_not(mask __m, const char_type* __lo, const char_type* __hi) const
      { return this->do_scan_not(__m, __lo, __hi); }

    protected:
      explicit 
      _Ctype(size_t __refs = 0) : _Ctype_nois<_CharT>(__refs) { }

      virtual 
      ~_Ctype() { }

      virtual bool 
      do_is(mask __m, char_type __c) const = 0;

      virtual const char_type*
      do_is(const char_type* __lo, const char_type* __hi, 
	    mask* __vec) const = 0;

      virtual const char_type*
      do_scan_is(mask __m, const char_type* __lo, 
		 const char_type* __hi) const = 0;

      virtual const char_type*
      do_scan_not(mask __m, const char_type* __lo, 
		  const char_type* __hi) const = 0;
    };

  template<typename _CharT>
    class ctype : public _Ctype<_CharT>
    {
    public:
      // Types:
      typedef _CharT 					char_type;
      typedef typename ctype::mask 			mask;

      explicit 
      ctype(size_t __refs = 0) : _Ctype<_CharT>(__refs) { }

      static locale::id id;

   protected:
      virtual 
      ~ctype() { }

      virtual bool 
      do_is(mask, char_type) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual const char_type*
      do_is(const char_type*, const char_type*, mask*) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual const char_type*
      do_scan_is(mask, const char_type*, const char_type*) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual const char_type*
      do_scan_not(mask, const char_type*, const char_type*) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual char_type 
      do_toupper(char_type) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual const char_type*
      do_toupper(char_type*, const char_type*) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual char_type 
      do_tolower(char_type) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual const char_type*
      do_tolower(char_type*, const char_type*) const
      {
	// XXX Need definitions for these abstract mf's.
      }
      
      virtual char_type 
      do_widen(char) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual const char*
      do_widen(const char*, const char*, char_type*) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual char 
      do_narrow(char_type, char) const
      {
	// XXX Need definitions for these abstract mf's.
      }

      virtual const char_type*
      do_narrow(const char_type*, const char_type*, char, char*) const
      {
	// XXX Need definitions for these abstract mf's.
      }
    };


  // 22.2.1.3  ctype specializations
  // NB: Can use _Ctype_nois to actually implement the is
  // functionality in the non-virtual (thus inline-able) member
  // fuctions.
  template<>
    class ctype<char> : public _Ctype_nois<char>
    {
    public:
      // Types:
      typedef char 					char_type;
      typedef ctype::mask 				mask;
      typedef size_t					__table_type;

    private:
      // Data Members:
      bool 			_M_del;
      __to_type const& 		_M_toupper;
      __to_type const& 		_M_tolower;
      const mask* const& 	_M_ctable;
      const mask* 		_M_table;
      
    public:
      static locale::id 	id;
      static const __table_type table_size = 1 +static_cast<unsigned char>(-1);

      explicit 
      ctype(const mask* __table = 0, bool __del = false, 
	    size_t __refs = 0) throw();

      inline bool 
      is(mask __m, char __c) const throw();
 
      inline const char*
      is(const char* __low, const char* __high, mask* __vec) const throw();
 
      inline const char*
      scan_is(mask __m, const char* __low, const char* __high) const throw();

      inline const char*
      scan_not(mask __m, const char* __low, const char* __high) const throw();
     
    protected:
      virtual 
      ~ctype();

      inline const mask* 
      table() const throw()
      { return _M_table; }

      inline const mask* 
      classic_table() throw()
      { return _M_ctable; }

      virtual char_type 
      do_toupper(char_type) const;

      virtual const char_type*
      do_toupper(char_type* __low, const char_type* __high) const;

      virtual char_type 
      do_tolower(char_type) const;

      virtual const char_type*
      do_tolower(char_type* __low, const char_type* __high) const;
      
      virtual char_type 
      do_widen(char) const;

      virtual const char*
      do_widen(const char* __low, const char* __high,
	       char_type* __dest) const;

      virtual char 
      do_narrow(char_type, char __dfault) const;

      virtual const char_type*
      do_narrow(const char_type* __low, const char_type* __high,
		 char __dfault, char* __dest) const;
    };
 
  template<>
    const ctype<char>&
    use_facet<const ctype<char> > (const locale& __loc);

#ifdef _GLIBCPP_USE_WCHAR_T
  // ctype<wchar_t> specialization
  template<>
    class ctype<wchar_t> : public _Ctype<wchar_t>
    {
    public:
     // Types:
      typedef wchar_t 					char_type;
      typedef ctype::mask 				mask;
      typedef size_t					__table_type;

    private:
      __to_type const& 		_M_toupper;
      __to_type const& 		_M_tolower;
      const mask* const& 	_M_ctable;
      static const __table_type	_S_table_size = ctype<char>::table_size;
      
    public:
      static locale::id id;

      explicit 
      ctype(size_t __refs = 0) throw();

    protected:
      virtual 
      ~ctype();

      virtual bool 
      do_is(mask __m, char_type __c) const;

      virtual const char_type*
      do_is(const char_type* __lo, const char_type* __hi, 
	    mask* __vec) const;

      virtual const char_type*
      do_scan_is(mask __m, const char_type* __lo, 
		 const char_type* __hi) const;

      virtual const char_type*
      do_scan_not(mask __m, const char_type* __lo, 
		  const char_type* __hi) const;

      virtual char_type 
      do_toupper(char_type) const;

      virtual const char_type*
      do_toupper(char_type* __low, const char_type* __high) const;

      virtual char_type 
      do_tolower(char_type) const;

      virtual const char_type*
      do_tolower(char_type* __low, const char_type* __high) const;
      
      virtual char_type 
      do_widen(char) const;

      virtual const char*
      do_widen(const char* __low, const char* __high,
	       char_type* __dest) const;

      virtual char 
      do_narrow(char_type, char __dfault) const;

      virtual const char_type*
      do_narrow(const char_type* __low, const char_type* __high,
		 char __dfault, char* __dest) const;

    };

  template<>
    const ctype<wchar_t>&
    use_facet< const ctype<wchar_t> > (const locale& __loc);
#endif //_GLIBCPP_USE_WCHAR_T

  // Include host-specific ctype specializations.
  #include <bits/ctype_specializations.h>

  // 22.2.1.2  Template class ctype_byname
  template<typename _CharT>
    class ctype_byname : public ctype<_CharT>
    {
    public:
      typedef _CharT 		char_type;

      explicit 
      ctype_byname(const char*, size_t __refs = 0);

    protected:
      virtual 
      ~ctype_byname() { }
    };

  //  22.2.1.4  Class ctype_byname specializations
  template<>
    ctype_byname<char>::ctype_byname(const char*, size_t refs);
#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    ctype_byname<wchar_t>::ctype_byname(const char*, size_t refs);
#endif


  //  22.2.1.5  Template class codecvt
  class codecvt_base
  {
  public:
    enum result
    {
      ok,
      partial,
      error,
      noconv
    };
  };

  template<typename _InternT, typename _ExternT, typename _StateT>
    class _Codecvt : public locale::facet, public codecvt_base
    {
    public:
      // Types:
      typedef _InternT intern_type;
      typedef _ExternT extern_type;
      typedef _StateT  state_type;
      
    protected:
      explicit 
      _Codecvt (size_t __refs = 0) : locale::facet(__refs) { }

    public:
      result
      out(state_type& __state, const intern_type* __from, 
	  const intern_type* __from_end, const intern_type* &__from_next,
	  extern_type* __to, extern_type* __to_limit, 
	  extern_type*& __to_next) const
      { 
	return do_out(__state, __from, __from_end, __from_next, __to, 
		      __to_limit, __to_next); 
      }

      result
      unshift(state_type& __state, extern_type* __to, extern_type* __to_limit,
	      extern_type*& __to_next) const
      { return do_unshift(__state, __to,__to_limit,__to_next); }

      result
      in(state_type& __state, const extern_type* __from, 
	 const extern_type* __from_end, const extern_type*& __from_next,
	 intern_type* __to, intern_type* __to_limit,
	 intern_type*& __to_next) const
      { 
	return do_in(__state, __from, __from_end, __from_next,
		     __to, __to_limit, __to_next); 
      }

      int 
      encoding() const throw()
      { return do_encoding(); }

      bool 
      always_noconv() const throw()
      { return do_always_noconv(); }

      int
      length(const state_type& __state, const extern_type* __from,
	     const extern_type* __end, size_t __max) const
      { return do_length(__state, __from, __end, __max); }

      int 
      max_length() const throw()
      { return do_max_length(); }

    protected:
      virtual 
      ~_Codecvt() { }

      virtual result
      do_out(state_type& __state,
	     const intern_type* __from, const intern_type* __from_end,
	     const intern_type*& __from_next,
	     extern_type* __to, extern_type* __to_limit,
	     extern_type*& __to_next) const = 0;

      virtual result
      do_unshift(state_type& __state,
		 extern_type* __to, extern_type* __to_limit,
		 extern_type*& __to_next) const = 0;
      
      virtual result
      do_in(state_type& __state,
	    const extern_type* __from, const extern_type* __from_end,
	    const extern_type*& __from_next,
	    intern_type* __to, intern_type* __to_limit,
	    intern_type*& __to_next) const = 0;
      
      virtual int 
      do_encoding() const throw() = 0;

      virtual bool 
      do_always_noconv() const throw() = 0;

      virtual int 
      do_length(const state_type&, const extern_type* __from,
		const extern_type* __end, size_t __max) const = 0;

      virtual int 
      do_max_length() const throw() = 0;
    };
  

  template<typename _InternT, typename _ExternT, typename _StateT>
    class codecvt : public _Codecvt<_InternT,_ExternT,_StateT>
    {
    public:      
      // Types:
      typedef _InternT intern_type;
      typedef _ExternT extern_type;
      typedef _StateT state_type;

      // Data Members:
      static locale::id id;

      explicit 
      codecvt(size_t __refs = 0) 
      : _Codecvt<_InternT,_ExternT,_StateT> (__refs) { }

    protected:
      virtual 
      ~codecvt() { }
    };

  // codecvt<char,char,mbstate_t> specialization
  template<>
    class codecvt<char, char, mbstate_t> 
    : public _Codecvt<char, char, mbstate_t>
    {
    public:      
      // Types:
      typedef char intern_type;
      typedef char extern_type;
      typedef mbstate_t state_type;

      explicit codecvt (size_t __refs = 0);
      static locale::id id;

    protected:
      virtual ~codecvt();
      virtual result
      do_out(state_type& __state, const intern_type* __from, 
	     const intern_type* __from_end, const intern_type*& __from_next,
	     extern_type* __to, extern_type* __to_limit,
	     extern_type*& __to_next) const;

      virtual result
      do_unshift(state_type& __state, extern_type* __to, 
		 extern_type* __to_limit, extern_type*& __to_next) const;

      virtual result
      do_in(state_type& __state, const extern_type* __from, 
	    const extern_type* __from_end, const extern_type*& __from_next,
	    intern_type* __to, intern_type* __to_limit, 
	    intern_type*& __to_next) const;

      virtual int do_encoding() const throw();
      virtual bool do_always_noconv() const throw();
      virtual int do_length(const state_type&, const extern_type* __from,
			    const extern_type* __end, size_t __max) const;
      virtual int do_max_length() const throw();
  };

#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    class codecvt<wchar_t,char,mbstate_t> 
    : public _Codecvt<wchar_t,char,mbstate_t>
    {
    public:
      // Types:
      typedef wchar_t intern_type;
      typedef char extern_type;
      typedef mbstate_t state_type;

      explicit codecvt(size_t __refs = 0);
      static locale::id id;

    protected:
      virtual ~codecvt();
      virtual result
      do_out(state_type& __state, const intern_type* __from, 
	     const intern_type* __from_end, const intern_type*& __from_next,
	     extern_type* __to, extern_type* __to_limit,
	     extern_type*& __to_next) const;

      virtual result
      do_unshift(state_type& __state,
		 extern_type* __to, extern_type* __to_limit,
		 extern_type*& __to_next) const;

      virtual result
      do_in(state_type& __state,
	     const extern_type* __from, const extern_type* __from_end,
	     const extern_type*& __from_next,
	     intern_type* __to, intern_type* __to_limit,
	     intern_type*& __to_next) const;

      virtual int do_encoding() const throw();
      virtual bool do_always_noconv() const throw();
      virtual int do_length(const state_type&, const extern_type* __from,
			    const extern_type* __end, size_t __max) const;
      virtual int do_max_length() const throw();
    };
#endif //_GLIBCPP_USE_WCHAR_T


  // 22.2.1.6  Template class codecvt_byname
  template<typename _InternT, typename _ExternT, typename _StateT>
    class codecvt_byname : public codecvt<_InternT,_ExternT,_StateT>
    {
    public:
      explicit 
      codecvt_byname(const char*, size_t __refs = 0) 
      : codecvt<_InternT,_ExternT,_StateT> (__refs) { }
    protected:
      virtual 
      ~codecvt_byname() { }
    };

  template<>
    class codecvt_byname<char,char,mbstate_t>
    : public codecvt<char,char,mbstate_t>
    {
    public:
      explicit 
      codecvt_byname(const char*, size_t __refs = 0);

    protected:
      virtual 
      ~codecvt_byname();
    };
  
#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    class codecvt_byname<wchar_t,char,mbstate_t>
      : public codecvt<wchar_t,char,mbstate_t>
    {
    public:
      explicit 
      codecvt_byname(const char*, size_t __refs = 0);

    protected:
      virtual 
      ~codecvt_byname();
    };
#endif

  template<typename _CharT, typename _InIter>
    class _Numeric_get;  // forward

  // _Format_cache holds the information extracted from the numpunct<>
  // and moneypunct<> facets in a form optimized for parsing and
  // formatting.  It is stored via a void* pointer in the pword()
  // array of an iosbase object passed to the _get and _put facets.
  // NB: contains no user-serviceable parts.
  template<typename _CharT>
    class _Format_cache
    {
    public: 
      // Types:
      typedef _CharT 				char_type;
      typedef char_traits<_CharT> 		traits_type;
      typedef basic_string<_CharT>		string_type;
      typedef typename string_type::size_type	size_type;

      // Forward decls and Friends:
      friend class locale;
      template<typename _Char, typename _InIter>
        friend class _Numeric_get;
      friend class num_get<_CharT>;
      friend class num_put<_CharT>;
      friend class time_get<_CharT>;
      friend class money_get<_CharT>;
      friend class time_put<_CharT>;
      friend class money_put<_CharT>;

      // Data Members:

      // ios_base::pword() reserved cell
      static int 		_S_pword_ix; 

      // True iff data members are consistent with the current locale,
      // ie imbue sets this to false.
      bool 			_M_valid;

      // A list of valid numeric literals: for the standard "C" locale,
      // this would usually be: "-+xX0123456789abcdef0123456789ABCDEF"
      static const char _S_literals[];

      // NB: Code depends on the order of definitions of the names
      // these are indices into _S_literals, above.
      // This string is formatted for putting, not getting. (output, not input)
      enum 
      {  
	_S_minus, 
	_S_plus, 
	_S_ecks, 
	_S_Ecks, 
	_S_digits,
	_S_digits_end = _S_digits + 16,
	_S_udigits = _S_digits_end,  
	_S_udigits_end = _S_udigits + 16,
	_S_ee = _S_digits + 14, // For scientific notation, 'E'
	_S_Ee = _S_udigits + 14 // For scientific notation, 'e'
      };

      // The sign used to separate decimal values: for standard US
      // locales, this would usually be: "."
      // Abstracted from numpunct::decimal_point().
      char_type 		_M_decimal_point;

      // The sign used to separate groups of digits into smaller
      // strings that the eye can parse with less difficulty: for
      // standard US locales, this would usually be: ","
      // Abstracted from numpunct::thousands_sep().
      char_type			_M_thousands_sep;

      // However the US's "false" and "true" are translated.
      // From numpunct::truename() and numpunct::falsename(), respectively.
      string_type 		_M_truename;
      string_type 		_M_falsename;

      // If we are checking groupings. This should be equivalent to 
      // numpunct::groupings().size() != 0
      bool 			_M_use_grouping;

      // If we are using numpunct's groupings, this is the current
      // grouping string in effect (from numpunct::grouping()).
      string 			_M_grouping;

      _Format_cache();

      ~_Format_cache() throw() { }

      // Given a member of the ios heirarchy as an argument, extract
      // out all the current formatting information into a
      // _Format_cache object and return a pointer to it.
      static _Format_cache<_CharT>* 
      _S_get(ios_base& __ios);

      void 
      _M_populate(ios_base&);

      static void 
      _S_callback(ios_base::event __event, ios_base& __ios, int __ix) throw();
    };

   template<> _Format_cache<char>::_Format_cache();
#ifdef _GLIBCPP_USE_WCHAR_T
   template<> _Format_cache<wchar_t>::_Format_cache();
#endif

  // _Numeric_get is used by num_get, money_get, and time_get to help
  // in parsing out numbers.
  template<typename _CharT, typename _InIter>
    class _Numeric_get
    {
    public:
      // Types:
      typedef _CharT     char_type;
      typedef _InIter    iter_type;

      // Forward decls and Friends:
      template<typename _Char, typename _InIterT>
      friend class num_get;
      template<typename _Char, typename _InIterT>
      friend class time_get;
      template<typename _Char, typename _InIterT>
      friend class money_get;
      template<typename _Char, typename _InIterT>
      friend class num_put;
      template<typename _Char, typename _InIterT>
      friend class time_put;
      template<typename _Char, typename _InIterT>
      friend class money_put;

    private:
      explicit 
      _Numeric_get() { }

      virtual 
      ~_Numeric_get() { }

      iter_type 
      _M_get_digits(iter_type __in, iter_type __end) const;
    };

  template<typename _CharT, typename _InIter>
    class num_get : public locale::facet
    {
    public:
      // Types:
      typedef _CharT   			char_type;
      typedef _InIter  			iter_type;
      typedef char_traits<_CharT> 	__traits_type;

      explicit 
      num_get(size_t __refs = 0) : locale::facet(__refs) { }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, bool& __v) const
      { return do_get(__in, __end, __io, __err, __v); }

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, short& __v) const
      { return do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, int& __v)   const
      { return do_get(__in, __end, __io, __err, __v); }
#endif

      iter_type
      get(iter_type __in, iter_type __end, ios_base& __io, 
	  ios_base::iostate& __err, long& __v) const
      { return do_get(__in, __end, __io, __err, __v); }

#ifdef _GLIBCPP_USE_LONG_LONG
      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, long long& __v) const
      { return do_get(__in, __end, __io, __err, __v); }
#endif

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, unsigned short& __v) const
      { return do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, unsigned int& __v)   const
      { return do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, unsigned long& __v)  const
      { return do_get(__in, __end, __io, __err, __v); }

#ifdef _GLIBCPP_USE_LONG_LONG
      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, unsigned long long& __v)  const
      { return do_get(__in, __end, __io, __err, __v); }
#endif

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, float& __v) const
      { return do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, double& __v) const
      { return do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, long double& __v) const
      { return do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, void*& __v) const
      { return do_get(__in, __end, __io, __err, __v); }      
      static locale::id id;

    protected:
      virtual ~num_get() { }

      // This consolidates the extraction, storage and
      // error-processing parts of the do_get(...) overloaded member
      // functions. NB: this is specialized for char.
      void 
      _M_extract(iter_type __beg, iter_type __end, ios_base& __io, 
		 ios_base::iostate& __err, char* __xtrc, 
		 int& __base, bool __fp = true) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, bool&) const;

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, short&) const;
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, int&) const;
#endif
      virtual iter_type 
      do_get (iter_type, iter_type, ios_base&, ios_base::iostate&, long&) const;
#ifdef _GLIBCPP_USE_LONG_LONG 
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     long long&) const;
#endif
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	      unsigned short&) const;
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&,
	      ios_base::iostate& __err, unsigned int&) const;
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&,
	      ios_base::iostate& __err, unsigned long&) const;
#ifdef _GLIBCPP_USE_LONG_LONG 
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&,
	     ios_base::iostate& __err, unsigned long long&) const;
#endif
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     float&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     double&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, 
	     ios_base::iostate& __err, long double&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     void*&) const;
    };

  // Declare specialized extraction member function.
  template<>
    void
    num_get<char, istreambuf_iterator<char> >::    
    _M_extract(istreambuf_iterator<char> __beg, 
	       istreambuf_iterator<char> __end, ios_base& __io, 
	       ios_base::iostate& __err, char* __xtrc, 
	       int& __base, bool __fp) const;

  // _Numeric_put is used by num_put, money_put, and time_put
  //   to help in formatting out numbers.
  template<typename _CharT, typename _OutIter>
    class _Numeric_put
    {
    public:
      typedef _CharT      char_type;
      typedef _OutIter    iter_type;
    protected:
      explicit 
      _Numeric_put() { }

      virtual 
      ~_Numeric_put() { }
    };

  template<typename _CharT, typename _OutIter>
    class num_put : public locale::facet
    {
    public:
      // Types:
      typedef _CharT       char_type;
      typedef _OutIter     iter_type;

      explicit 
      num_put(size_t __refs = 0) : locale::facet(__refs) { }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, bool __v) const
      { return do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, long __v) const
      { return do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, 
	  unsigned long __v) const
      { return do_put(__s, __f, __fill, __v); }

#ifdef _GLIBCPP_USE_LONG_LONG 
      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, long long __v) const
      { return do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, 
	  unsigned long long __v) const
      { return do_put(__s, __f, __fill, __v); }
#endif

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, double __v) const
      { return do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, 
	  long double __v) const
      { return do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, 
	  const void* __v) const
      { return do_put(__s, __f, __fill, __v); }

      static locale::id id;

    protected:
      virtual 
      ~num_put() { };

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, bool __v) const;

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, long __v) const;

#ifdef _GLIBCPP_USE_LONG_LONG 
      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, long long __v) const;
#endif

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, unsigned long) const;

#ifdef _GLIBCPP_USE_LONG_LONG
      virtual iter_type
      do_put(iter_type, ios_base&, char_type __fill, unsigned long long) const;
#endif

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, double __v) const;

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, long double __v) const;

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, const void* __v) const;
    };

  template<typename _CharT>
    class _Punct : public locale::facet
    {
    public:
      // Types:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      char_type    
      decimal_point() const
      { return do_decimal_point(); }

      char_type    
      thousands_sep() const
      { return do_thousands_sep(); }

      string       
      grouping() const
      { return do_grouping(); }
    protected:

      explicit 
      _Punct(size_t __refs = 0) : locale::facet(__refs) { }

      virtual 
      ~_Punct() { }

      virtual char_type    
      do_decimal_point() const
      { return _M_decimal_point; }

      virtual char_type    
      do_thousands_sep() const
      { return _M_thousands_sep; }

      virtual string       
      do_grouping() const
      { return _M_grouping; }

    private:
      char_type _M_decimal_point;
      char_type _M_thousands_sep;
      string    _M_grouping;
      
    protected:
      // for use at construction time only:
      void 
      _M_init(char_type __d, char_type __t, const string& __g)
      {
	_M_decimal_point = __d;
	_M_thousands_sep = __t;
	_M_grouping = __g;
      }

    };

  template<typename _CharT>
    class _Numpunct : public _Punct<_CharT>
    {
    public:
      // Types:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      string_type  
      truename() const
      { return do_truename(); }

      string_type  
      falsename() const
      { return do_falsename(); }

    protected:
      explicit 
      _Numpunct(size_t __refs = 0) : _Punct<_CharT> (__refs) { }

      virtual 
      ~_Numpunct() { }

      virtual string_type  
      do_truename() const
      { return _M_truename; }

      virtual string_type  
      do_falsename() const
      { return _M_falsename; }

    private:
      string_type _M_truename;
      string_type _M_falsename;
      
    protected:
      // For use only during construction
      void 
      _M_init_boolnames(const string_type& __t, const string_type& __f)
      {
	_M_truename = __t;
	_M_falsename = __f;
      }
	
    };

  template<typename _CharT>
    class numpunct : public _Numpunct<_CharT>
    {
    public:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      static locale::id id;

      explicit 
      numpunct(size_t __refs = 0) : _Numpunct<_CharT>(__refs) { }
    protected:

      virtual 
      ~numpunct() { }
    };

  template<> 
    numpunct<char>::numpunct(size_t __refs): _Numpunct<char>(__refs)
    {
      _M_init('.', ',', "");
      _M_init_boolnames("true", "false");
    }

#ifdef _GLIBCPP_USE_WCHAR_T
  template<> 
    numpunct<wchar_t>::numpunct(size_t __refs): _Numpunct<wchar_t>(__refs)
    {
      _M_init(L'.', L',', "");
      _M_init_boolnames(L"true", L"false");
    }
#endif

  template<typename _CharT>
    class numpunct_byname : public numpunct<_CharT>
    {
    public:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      explicit 
      numpunct_byname(const char*, size_t __refs = 0);
      
    protected:
      virtual 
      ~numpunct_byname() { }
    };

  template<>
    numpunct_byname<char>::numpunct_byname(const char*, size_t __refs);
#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    numpunct_byname<wchar_t>::numpunct_byname(const char*, size_t __refs);
#endif

  template<typename _CharT>
    class _Collate : public locale::facet
    {
    public:
      // Types:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      int 
      compare(const _CharT* __lo1, const _CharT* __hi1,
	      const _CharT* __lo2, const _CharT* __hi2) const
      { return do_compare(__lo1, __hi1, __lo2, __hi2); }

      string_type 
      transform(const _CharT* __lo, const _CharT* __hi) const
      { return do_transform(__lo, __hi); }

      long 
      hash(const _CharT* __lo, const _CharT* __hi) const
      { return do_hash(__lo, __hi); }
      
  protected:
      explicit 
      _Collate(size_t __refs = 0) : locale::facet(__refs) { }

      ~_Collate() { } // virtual

      virtual int  
      do_compare(const _CharT* __lo1, const _CharT* __hi1,
		 const _CharT* __lo2, const _CharT* __hi2) const = 0;

      virtual string_type 
      do_transform(const _CharT* __lo, const _CharT* __hi) const = 0;

      virtual long   
      do_hash(const _CharT* __lo, const _CharT* __hi) const = 0;
    };

  template<typename _CharT>
    class collate : public _Collate<_CharT>
    {
    public:      
      // Types:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      explicit 
      collate(size_t __refs = 0) : _Collate<_CharT> (__refs) { }

      static locale::id id;
      
    protected:
      virtual 
      ~collate() { }
    };

  template<>
    class collate<char> : public _Collate<char>
    {
    public:      
      // Types:
      typedef char               char_type;
      typedef basic_string<char> string_type;

      explicit 
      collate(size_t __refs = 0);

      static locale::id id;
      
    protected:
      virtual 
      ~collate();

      virtual int  
      do_compare(const char* __lo1, const char* __hi1,
		 const char* __lo2, const char* __hi2) const;

      virtual string_type 
      do_transform(const char* __lo, const char* __hi) const;

      virtual long   
      do_hash(const char* __lo, const char* __hi) const;
    };

#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    class collate<wchar_t> : public _Collate<wchar_t>
    {
    public:
      // Types:
      typedef wchar_t               char_type;
      typedef basic_string<wchar_t> string_type;
      
      explicit 
      collate(size_t __refs = 0);

      static locale::id id;
      
    protected:
      virtual 
      ~collate();

      virtual int   
      do_compare(const wchar_t* __lo1, const wchar_t* __hi1,
		 const wchar_t* __lo2, const wchar_t* __hi2) const;

      virtual string_type 
      do_transform(const wchar_t* __lo, const wchar_t* __hi) const;

      virtual long   
      do_hash(const wchar_t* __lo, const wchar_t* __hi) const;
    };
#endif

  template<typename _CharT>
    class collate_byname : public collate<_CharT>
    {
    public:
      // Types:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      explicit 
      collate_byname(const char*, size_t __refs = 0);

    protected:
      virtual 
      ~collate_byname() { }
    };

  template<>
    collate_byname<char>::collate_byname(const char*, size_t __refs);
#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    collate_byname<wchar_t>::collate_byname(const char*, size_t __refs);
#endif

  class time_base
  {
  public:
    enum dateorder { no_order, dmy, mdy, ymd, ydm };
  };

  template<typename _CharT, typename _InIter>
    class time_get : public locale::facet, public time_base
    {
    public:
      // Types:
      typedef _CharT     char_type;
      typedef _InIter    iter_type;

      explicit 
      time_get(size_t __refs = 0) 
      : locale::facet (__refs), _M_daynames(0), _M_monthnames(0) { }

      dateorder 
      date_order()  const
      { return do_date_order(); }

      iter_type 
      get_time(iter_type __s, iter_type __end, ios_base& __f, 
	       ios_base::iostate& __err, tm* __t)  const
      { return do_get_time(__s, __end, __f, __err, __t); }

      iter_type 
      get_date(iter_type __s, iter_type __end, ios_base& __f,
	       ios_base::iostate& __err, tm* __t)  const
      { return do_get_date(__s, __end, __f, __err, __t); }

      iter_type 
      get_weekday(iter_type __s, iter_type __end, ios_base& __f,
		  ios_base::iostate& __err, tm* __t) const
      { return do_get_weekday(__s,__end,__f,__err,__t); }

      iter_type 
      get_monthname(iter_type __s, iter_type __end, ios_base& __f, 
		    ios_base::iostate& __err, tm* __t) const
      { return do_get_monthname(__s,__end,__f,__err,__t); }

      iter_type 
      get_year(iter_type __s, iter_type __end, ios_base& __f,
	       ios_base::iostate& __err, tm* __t) const
      { return do_get_year(__s,__end,__f,__err,__t); }

      static locale::id id;

    protected:
      virtual 
      ~time_get() 
      {      
	delete [] _M_monthnames; 
	delete [] _M_daynames; 
      }

      virtual dateorder 
      do_date_order()  const
      { return time_base::ymd; }

      virtual iter_type 
      do_get_time(iter_type __s, iter_type /*__end*/, ios_base&,
		  ios_base::iostate& /*__err*/, tm* /*__t*/) const
      { return __s; }

      virtual iter_type 
      do_get_date(iter_type __s, iter_type /*__end*/, ios_base&,
		  ios_base::iostate& /*__err*/, tm* /*__t*/) const
      { return __s; }

      virtual iter_type 
      do_get_weekday(iter_type __s, iter_type __end, ios_base&,
		     ios_base::iostate& __err, tm* __t) const;

      virtual iter_type 
      do_get_monthname(iter_type __s, iter_type __end, ios_base&, 
		       ios_base::iostate& __err, tm* __t) const;

      virtual iter_type 
      do_get_year(iter_type __s, iter_type /*__end*/, ios_base&,
		   ios_base::iostate& /*__err*/, tm* /*__t*/) const
      { return __s; }

      mutable basic_string<_CharT>* _M_daynames;
      mutable basic_string<_CharT>* _M_monthnames;
    };

  template<typename _CharT, typename _InIter>
    class time_get_byname : public time_get<_CharT, _InIter>
    {
    public:
      typedef _CharT     char_type;
      typedef _InIter    iter_type;

      explicit 
      time_get_byname(const char*, size_t __refs = 0) 
      : time_get<_CharT, _InIter>(__refs) { }
    protected:
      virtual 
      ~time_get_byname() { }
    };

  template<typename _CharT, typename _OutIter>
    class time_put : public locale::facet, public time_base
    {
    public:
      typedef _CharT     char_type;
      typedef _OutIter   iter_type;

      explicit 
      time_put(size_t __refs = 0) : locale::facet (__refs) { }

      // NB: this is a nonvirtual, calls do_put in a loop.
      iter_type 
      put(iter_type __s, ios_base& /*__f*/, char_type /*__fill*/,
          const tm* /*__tmb*/, const _CharT* /*__pattern*/,
          const _CharT* /*__pat_end*/) const
      { return __s; }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill,
	  const tm* __tmb, char __format, char __modifier = 0) const
      { return do_put(__s, __f, __fill, __tmb, __format, __modifier); }

      static locale::id id;

    protected:
      virtual 
      ~time_put() { }

      virtual iter_type 
      do_put(iter_type __s, ios_base&, char_type, const tm* /*__t*/, 
	     char /*__format*/, char /*__mod*/) const
      { return __s; }
    };

  template<typename _CharT, typename _OutIter>
    class time_put_byname : time_put<_CharT, _OutIter>
    {
    public:
      typedef _CharT     char_type;
      typedef _OutIter   iter_type;

      explicit 
      time_put_byname(const char*, size_t __refs = 0) 
      : time_put<_CharT, _OutIter> (__refs) { }
    protected:
      virtual 
      ~time_put_byname() { }
    };


  template<typename _CharT, typename _InIter>
    class money_get : public locale::facet
    {
    public:
      typedef _CharT        char_type;
      typedef _InIter       iter_type;
      typedef basic_string<_CharT> string_type;

      explicit 
      money_get(size_t __refs = 0) : locale::facet(__refs) { }

      iter_type 
      get(iter_type __s, iter_type __end, bool __intl,
	  ios_base& __f, ios_base::iostate& __err, long double& __units) const
      { return do_get(__s, __end, __intl, __f, __err, __units); }

      iter_type 
      get(iter_type __s, iter_type __end, bool __intl, ios_base& __f, 
	   ios_base::iostate& __err, string_type& __digits) const
      { return do_get(__s, __end, __intl, __f, __err, __digits); }

      static locale::id id;

    protected:
      virtual 
      ~money_get() { }

      virtual iter_type 
      do_get(iter_type __s, iter_type /*__end*/, bool /*__intl*/,
             ios_base& /*__io*/, ios_base::iostate& /*__err*/,
             long double& /*__units*/) const
      { return __s; }

      virtual iter_type 
      do_get(iter_type __s, iter_type /*__end*/, bool /*__intl*/,
             ios_base& /*__io*/, ios_base::iostate& /*__err*/,
             string_type& /*__digits*/) const
      { return __s; }
    };

  template<typename _CharT, typename _OutIter>
    class money_put : public locale::facet
    {
    public:
      typedef _CharT              char_type;
      typedef _OutIter            iter_type;
      typedef basic_string<_CharT> string_type;

      explicit 
      money_put(size_t __refs = 0) : locale::facet(__refs) { }

      iter_type 
      put(iter_type __s, bool __intl, ios_base& __f,
	  char_type __fill, long double __units) const
      { return do_put(__s, __intl, __f, __fill, __units); }

      iter_type 
      put(iter_type __s, bool __intl, ios_base& __f,
	  char_type __fill, const string_type& __digits) const
      { return do_put(__s, __intl, __f, __fill, __digits); }

      static locale::id id;

    protected:
      virtual 
      ~money_put() { }

      virtual iter_type
      do_put(iter_type __s, bool, ios_base& /*__io*/, char_type /*__fill*/,
	     long double /*__units*/) const
      { return __s; }

      virtual iter_type
      do_put(iter_type __s, bool, ios_base& /*__io*/, char_type /*__fill*/,
	     const string_type& /*__digits*/) const
      { return __s; }
    };

  struct money_base
  {
    enum part { none, space, symbol, sign, value };
    struct pattern { char field[4]; };

    static const pattern __default_pattern;
  };

  template<typename _CharT>
    class _Moneypunct : public _Punct<_CharT>, public money_base
    {
    public:
      typedef _CharT char_type;
      typedef basic_string<_CharT> string_type;

      string_type  
      curr_symbol()   const
      { return do_curr_symbol(); }

      string_type  
      positive_sign() const
      { return do_positive_sign(); }

      string_type  
      negative_sign() const
      { return do_negative_sign(); }

      int          
      frac_digits()   const
      { return do_frac_digits(); }

      pattern      
      pos_format()    const
      { return do_pos_format(); }

      pattern      
      neg_format()    const
      { return do_neg_format(); }

    protected:
      explicit 
      _Moneypunct(size_t __refs = 0) : _Punct<_CharT> (__refs) { }

      virtual 
      ~_Moneypunct() { }

      virtual string_type  
      do_curr_symbol()   const
      { return basic_string<_CharT>(); }

      virtual string_type  
      do_positive_sign() const
      { return basic_string<_CharT>(); }

      virtual string_type  
      do_negative_sign() const
      { return basic_string<_CharT>(); }

      virtual int          
      do_frac_digits() const
      { return 0; }

      virtual pattern      
      do_pos_format() const
      {
	return money_base::__default_pattern;
      }

      virtual pattern      
      do_neg_format() const
      {
	return money_base::__default_pattern;
      }

    };

  template<typename _CharT, bool _Intl>
    class moneypunct : public _Moneypunct<_CharT>
    {
    public:
      // Types:
      typedef _CharT 			char_type;
      typedef basic_string<_CharT> 	string_type;

      static const bool intl = _Intl;
      static locale::id id;

      explicit 
      moneypunct(size_t __refs = 0) : _Moneypunct<_CharT> (__refs) { }
    protected:
      virtual 
      ~moneypunct() { }
    };

  template<typename _CharT, bool _Intl>
    class moneypunct_byname : public moneypunct<_CharT,_Intl>
    {
    public:
      typedef _CharT char_type;
      typedef basic_string<_CharT> string_type;
      static const bool intl = _Intl;

      explicit 
      moneypunct_byname(const char*, size_t __refs = 0);

    protected:
      virtual 
      ~moneypunct_byname() { }
    };

  template<>
    moneypunct_byname<char, false>::
    moneypunct_byname(const char*, size_t __refs);
  template<>
    moneypunct_byname<char, true>::
    moneypunct_byname(const char*, size_t __refs);
#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    moneypunct_byname<wchar_t,false>::
    moneypunct_byname(const char*, size_t __refs);
  template<>
    moneypunct_byname<wchar_t,true>::
    moneypunct_byname (const char*, size_t __refs);
#endif

  struct messages_base
  {
    typedef int catalog;
  };

  template<typename _CharT>
    class _Messages : public locale::facet, public messages_base
    {
    public:
      typedef _CharT char_type;
      typedef basic_string<_CharT> string_type;

      catalog 
      open(const basic_string<char>& __s, const locale& __loc) const
      { return do_open(__s, __loc); }

      string_type  
      get(catalog __c, int __set, int __msgid, const string_type& __s) const
      { return do_get(__c,__set,__msgid,__s); }

      void 
      close(catalog __c) const
      { return do_close(__c); }

    protected:
      explicit 
      _Messages(size_t __refs = 0) : locale::facet(__refs) { }

      virtual 
      ~_Messages() { }

      // NB: Probably these should be pure, and implemented only in
      //  specializations of messages<>.  But for now...
      virtual catalog 
      do_open(const basic_string<char>&, const locale&) const
      { return 0; }

      virtual string_type  
      do_get(catalog, int, int /*__msgid*/, const string_type& __dfault) const
      { return __dfault; }

      virtual void    
      do_close (catalog) const { }
    };

  template<typename _CharT>
    class messages : public _Messages<_CharT>
    {
    public:
      typedef _CharT char_type;
      typedef basic_string<_CharT> string_type;
      static locale::id id;

      explicit 
      messages(size_t __refs = 0) : _Messages<_CharT> (__refs) { }
    protected:
      virtual 
      ~messages() { }
    };

  template<typename _CharT>
    class messages_byname : public messages<_CharT>
    {
    public:
      typedef _CharT char_type;
      typedef basic_string<_CharT> string_type;

      explicit 
      messages_byname(const char*, size_t __refs = 0);

    protected:
      virtual 
      ~messages_byname() { }
    };

  template<>
    messages_byname<char>::messages_byname(const char*, size_t __refs);
#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    messages_byname<wchar_t>::messages_byname(const char*, size_t __refs);
#endif

  // Subclause convenience interfaces, inlines 
  // NB: these are inline
  // because, when used in a loop, some compilers can hoist the body
  // out of the loop; then it's just as fast as the C is*() function.
  template<typename _CharT>
    inline bool 
    isspace(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::space, __c); }

  template<typename _CharT>
    inline bool 
    isprint(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::print, __c); }

  template<typename _CharT>
    inline bool 
    iscntrl(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::cntrl, __c); }

  template<typename _CharT>
    inline bool 
    isupper(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::upper, __c); }

  template<typename _CharT>
    inline bool islower(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::lower, __c); }

  template<typename _CharT>
    inline bool 
    isalpha(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::alpha, __c); }

  template<typename _CharT>
    inline bool 
    isdigit(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::digit, __c); }

  template<typename _CharT>
    inline bool 
    ispunct(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::punct, __c); }

  template<typename _CharT>
    inline bool 
    isxdigit(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::xdigit, __c); }

  template<typename _CharT>
    inline bool 
    isalnum(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::alnum, __c); }

  template<typename _CharT>
    inline bool 
    isgraph(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).is(ctype_base::graph, __c); }

  template<typename _CharT>
    inline _CharT 
    toupper(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).toupper(__c); }

  template<typename _CharT>
    inline _CharT 
    tolower(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> > (__loc).tolower(__c); }

} // namespace std

#endif	/* _CPP_BITS_LOCFACETS_H */

// Local Variables:
// mode:c++
// End:

