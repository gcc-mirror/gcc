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

//
// ISO C++ 14882: 22.1  Locales
//

// Warning: this file is not meant for user inclusion.  Use <locale>.

#ifndef _CPP_BITS_LOCFACETS_H
#define _CPP_BITS_LOCFACETS_H	1

#pragma GCC system_header

#include <bits/std_ctime.h>	// For struct tm
#include <bits/std_ios.h>	// For ios_base
#ifdef _GLIBCPP_USE_WCHAR_T
# include <bits/std_cwctype.h>	// For wctype_t
#endif 

namespace std
{
  // 22.2.1.1  Template class ctype
  // Include host and configuration specific ctype enums for ctype_base.
  #include <bits/ctype_base.h>

  // __ctype_abstract_base is the common base for ctype<_CharT>.  
  template<typename _CharT>
    class __ctype_abstract_base : public locale::facet, public ctype_base
    {
    public:
      // Types:
      typedef _CharT char_type;

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

      char_type 
      toupper(char_type __c) const
      { return this->do_toupper(__c); }

      const char_type*
      toupper(char_type *__lo, const char_type* __hi) const
      { return this->do_toupper(__lo, __hi); }

      char_type 
      tolower(char_type __c) const
      { return this->do_tolower(__c); }

      const char_type*
      tolower(char_type* __lo, const char_type* __hi) const
      { return this->do_tolower(__lo, __hi); }

      char_type 
      widen(char __c) const
      { return this->do_widen(__c); }

      const char*
      widen(const char* __lo, const char* __hi, char_type* __to) const
      { return this->do_widen(__lo, __hi, __to); }

      char 
      narrow(char_type __c, char __dfault) const
      { return this->do_narrow(__c, __dfault); }

      const char_type*
      narrow(const char_type* __lo, const char_type* __hi,
	      char __dfault, char *__to) const
      { return this->do_narrow(__lo, __hi, __dfault, __to); }

    protected:
      explicit 
      __ctype_abstract_base(size_t __refs = 0): locale::facet(__refs) { }

      virtual 
      ~__ctype_abstract_base() { }
      
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

      virtual char_type 
      do_toupper(char_type) const = 0;

      virtual const char_type*
      do_toupper(char_type* __lo, const char_type* __hi) const = 0;

      virtual char_type 
      do_tolower(char_type) const = 0;

      virtual const char_type*
      do_tolower(char_type* __lo, const char_type* __hi) const = 0;
      
      virtual char_type 
      do_widen(char) const = 0;

      virtual const char*
      do_widen(const char* __lo, const char* __hi, 
	       char_type* __dest) const = 0;

      virtual char 
      do_narrow(char_type, char __dfault) const = 0;

      virtual const char_type*
      do_narrow(const char_type* __lo, const char_type* __hi,
		 char __dfault, char* __dest) const = 0;
    };

  // NB: Generic, mostly useless implementation.
  template<typename _CharT>
    class ctype : public __ctype_abstract_base<_CharT>
    {
    public:
      // Types:
      typedef _CharT 		  	char_type;
      typedef typename ctype::mask 	mask;

      explicit 
      ctype(size_t __refs = 0) : __ctype_abstract_base<_CharT>(__refs) { }

      static locale::id id;

   protected:
      virtual 
      ~ctype() { }

      virtual bool 
      do_is(mask __m, char_type __c) const
      { return false; }

      virtual const char_type*
      do_is(const char_type* __lo, const char_type* __hi, mask* __vec) const
      { return __hi; }

      virtual const char_type*
      do_scan_is(mask __m, const char_type* __lo, const char_type* __hi) const
      { return __hi; }

      virtual const char_type*
      do_scan_not(mask __m, const char_type* __lo,
		  const char_type* __hi) const
      { return __hi; }

      virtual char_type 
      do_toupper(char_type __c) const
      { return __c; }

      virtual const char_type*
      do_toupper(char_type* __lo, const char_type* __hi) const
      { return __hi; }

      virtual char_type 
      do_tolower(char_type __c) const
      { return __c; }

      virtual const char_type*
      do_tolower(char_type* __lo, const char_type* __hi) const
      { return __hi; }
      
      virtual char_type 
      do_widen(char __c) const
      { return char_type(); }

      virtual const char*
      do_widen(const char* __lo, const char* __hi, char_type* __dest) const
      { return __hi; }

      virtual char 
      do_narrow(char_type, char __dfault) const
      { return __dfault; }

      virtual const char_type*
      do_narrow(const char_type* __lo, const char_type* __hi,
		char __dfault, char* __dest) const
      { return __hi; }
    };

  template<typename _CharT>
    locale::id ctype<_CharT>::id;

  // 22.2.1.3  ctype specializations
  template<>
    class ctype<char> : public __ctype_abstract_base<char>
    {
    public:
      // Types:
      typedef char 	       char_type;

    private:
      // Data Members:
      bool 		       _M_del;
      __to_type const& 	       _M_toupper;
      __to_type const& 	       _M_tolower;
      const mask* const&       _M_ctable;
      const mask*              _M_table;
      
    public:
      static locale::id        id;
      static const size_t      table_size = 1 + static_cast<unsigned char>(-1);

      explicit 
      ctype(const mask* __table = 0, bool __del = false, size_t __refs = 0);

      inline bool 
      is(mask __m, char __c) const;
 
      inline const char*
      is(const char* __lo, const char* __hi, mask* __vec) const;
 
      inline const char*
      scan_is(mask __m, const char* __lo, const char* __hi) const;

      inline const char*
      scan_not(mask __m, const char* __lo, const char* __hi) const;
     
    protected:
      virtual 
      ~ctype();

      const mask* 
      table() const throw()
      { return _M_table; }

      const mask* 
      classic_table() throw()
      { return _M_ctable; }

      virtual bool 
      do_is(mask __m, char_type __c) const;

      virtual const char_type*
      do_is(const char_type* __lo, const char_type* __hi, mask* __vec) const;

      virtual const char_type*
      do_scan_is(mask __m, const char_type* __lo, const char_type* __hi) const;

      virtual const char_type*
      do_scan_not(mask __m, const char_type* __lo, 
		  const char_type* __hi) const;

      virtual char_type 
      do_toupper(char_type) const;

      virtual const char_type*
      do_toupper(char_type* __lo, const char_type* __hi) const;

      virtual char_type 
      do_tolower(char_type) const;

      virtual const char_type*
      do_tolower(char_type* __lo, const char_type* __hi) const;
      
      virtual char_type 
      do_widen(char) const;

      virtual const char*
      do_widen(const char* __lo, const char* __hi, char_type* __dest) const;

      virtual char 
      do_narrow(char_type, char __dfault) const;

      virtual const char_type*
      do_narrow(const char_type* __lo, const char_type* __hi,
		 char __dfault, char* __dest) const;
    };
 
  template<>
    const ctype<char>&
    use_facet<ctype<char> >(const locale& __loc);

#ifdef _GLIBCPP_USE_WCHAR_T
  // ctype<wchar_t> specialization
  template<>
    class ctype<wchar_t> : public __ctype_abstract_base<wchar_t>
    {
    public:
      // Types:
      typedef wchar_t 	       char_type;
      typedef wctype_t	       __wmask_type;

      // Data Members:
      static locale::id        id;

      explicit 
      ctype(size_t __refs = 0);

    protected:
      __wmask_type
      _M_convert_to_wmask(const mask __m) const;

      virtual 
      ~ctype();

      virtual bool 
      do_is(mask __m, char_type __c) const;

      virtual const char_type*
      do_is(const char_type* __lo, const char_type* __hi, mask* __vec) const;

      virtual const char_type*
      do_scan_is(mask __m, const char_type* __lo, const char_type* __hi) const;

      virtual const char_type*
      do_scan_not(mask __m, const char_type* __lo, 
		  const char_type* __hi) const;

      virtual char_type 
      do_toupper(char_type) const;

      virtual const char_type*
      do_toupper(char_type* __lo, const char_type* __hi) const;

      virtual char_type 
      do_tolower(char_type) const;

      virtual const char_type*
      do_tolower(char_type* __lo, const char_type* __hi) const;
      
      virtual char_type 
      do_widen(char) const;

      virtual const char*
      do_widen(const char* __lo, const char* __hi, char_type* __dest) const;

      virtual char 
      do_narrow(char_type, char __dfault) const;

      virtual const char_type*
      do_narrow(const char_type* __lo, const char_type* __hi,
		 char __dfault, char* __dest) const;

    };

  template<>
    const ctype<wchar_t>&
    use_facet<ctype<wchar_t> >(const locale& __loc);
#endif //_GLIBCPP_USE_WCHAR_T

  // Include host and configuration specific ctype inlines.
  #include <bits/ctype_inline.h>

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

  // 22.2.1.4  Class ctype_byname specialization
  template<>
    ctype_byname<char>::ctype_byname(const char*, size_t refs);


  // 22.2.1.5  Template class codecvt
  #include <bits/codecvt.h>

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
      static const char 	_S_literals[];

      // NB: Code depends on the order of definitions of the names
      // these are indices into _S_literals, above.
      // This string is formatted for putting, not getting. (output, not input)
      enum 
      {  
	_S_minus, 
	_S_plus, 
	_S_x, 
	_S_X, 
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

  template<typename _CharT>
    int _Format_cache<_CharT>::_S_pword_ix;

  template<typename _CharT>
    const char _Format_cache<_CharT>::
    _S_literals[] = "-+xX0123456789abcdef0123456789ABCDEF";

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

      static locale::id id;

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

    protected:
      virtual ~num_get() { }

      // This consolidates the extraction, storage and
      // error-processing parts of the do_get(...) overloaded member
      // functions. 
      // NB: This is specialized for char.
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

  template<typename _CharT, typename _InIter>
    locale::id num_get<_CharT, _InIter>::id;

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

      static locale::id id;

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

  template <typename _CharT, typename _OutIter>
    locale::id num_put<_CharT, _OutIter>::id;


  template<typename _CharT>
    class numpunct : public locale::facet
    {
    public:
      // Types:
      typedef _CharT          		char_type;
      typedef basic_string<_CharT> 	string_type;

      static locale::id id;

    private:
      char_type 	_M_decimal_point;
      char_type 	_M_thousands_sep;
      string 		_M_grouping;
      string_type 	_M_truename;
      string_type 	_M_falsename;

    public:
      explicit 
      numpunct(size_t __refs = 0) : locale::facet(__refs) 
      { _M_initialize_numpunct(); }

      explicit 
      numpunct(__c_locale __cloc, size_t __refs = 0) : locale::facet(__refs) 
      { _M_initialize_numpunct(__cloc); }

      char_type    
      decimal_point() const
      { return this->do_decimal_point(); }

      char_type    
      thousands_sep() const
      { return this->do_thousands_sep(); }

      string       
      grouping() const
      { return this->do_grouping(); }

      string_type  
      truename() const
      { return this->do_truename(); }

      string_type  
      falsename() const
      { return this->do_falsename(); }

    protected:
      virtual 
      ~numpunct() { }

      virtual char_type    
      do_decimal_point() const
      { return _M_decimal_point; }

      virtual char_type    
      do_thousands_sep() const
      { return _M_thousands_sep; }

      virtual string
      do_grouping() const
      { return _M_grouping; }

      virtual string_type  
      do_truename() const
      { return _M_truename; }

      virtual string_type  
      do_falsename() const
      { return _M_falsename; }

      // For use at construction time only.
      void 
      _M_initialize_numpunct(__c_locale __cloc = NULL);
    };

  template<typename _CharT>
    locale::id numpunct<_CharT>::id;

  template<typename _CharT>
    void
    numpunct<_CharT>::_M_initialize_numpunct(__c_locale /*__cloc*/)
    { 
      // NB: Cannot be made generic. 
    }

  template<> 
    void
    numpunct<char>::_M_initialize_numpunct(__c_locale __cloc);

#ifdef _GLIBCPP_USE_WCHAR_T
  template<> 
    void
    numpunct<wchar_t>::_M_initialize_numpunct(__c_locale __cloc);
#endif


  template<typename _CharT>
    class numpunct_byname : public numpunct<_CharT>
    {
      // Data Member.
      __c_locale			_M_c_locale_numpunct;

    public:
      typedef _CharT               	char_type;
      typedef basic_string<_CharT> 	string_type;

      explicit 
      numpunct_byname(const char* __s, size_t __refs = 0)
      : numpunct<_CharT>(__refs)
      {
	_S_create_c_locale(_M_c_locale_numpunct, __s);
	_M_initialize_numpunct(_M_c_locale_numpunct);	
      }

    protected:
      virtual 
      ~numpunct_byname() 
      { _S_destroy_c_locale(_M_c_locale_numpunct); }
    };


  template<typename _CharT>
    class collate : public locale::facet
    {
    public:
      // Types:
      typedef _CharT               	char_type;
      typedef basic_string<_CharT> 	string_type;

    protected:
      // Underlying "C" library locale information saved from
      // initialization, needed by collate_byname as well.
      __c_locale			_M_c_locale_collate;
 
    public:
      static locale::id id;

      explicit 
      collate(size_t __refs = 0)
      : locale::facet(__refs), _M_c_locale_collate(NULL)
      { } 

      // Non-standard.
      explicit 
      collate(__c_locale __cloc, size_t __refs = 0) 
      : locale::facet(__refs)
      { 
	if (__cloc)
	  _M_c_locale_collate = _S_clone_c_locale(__cloc); 
      }

      int 
      compare(const _CharT* __lo1, const _CharT* __hi1,
	      const _CharT* __lo2, const _CharT* __hi2) const
      { return this->do_compare(__lo1, __hi1, __lo2, __hi2); }

      string_type 
      transform(const _CharT* __lo, const _CharT* __hi) const
      { return this->do_transform(__lo, __hi); }

      long 
      hash(const _CharT* __lo, const _CharT* __hi) const
      { return this->do_hash(__lo, __hi); }
      
      // Used to abstract out _CharT bits in virtual member functions, below.
      int
      _M_compare_helper(const _CharT*, const _CharT*) const;

      size_t
      _M_transform_helper(_CharT*, const _CharT*, size_t) const;;

  protected:
      virtual
      ~collate() 
      {
	if (_M_c_locale_collate)
	  _S_destroy_c_locale(_M_c_locale_collate); 
      }

      virtual int  
      do_compare(const _CharT* __lo1, const _CharT* __hi1,
		 const _CharT* __lo2, const _CharT* __hi2) const;

      virtual string_type 
      do_transform(const _CharT* __lo, const _CharT* __hi) const;

      virtual long   
      do_hash(const _CharT* __lo, const _CharT* __hi) const;
    };

  template<typename _CharT>
    locale::id collate<_CharT>::id;

  // Specializations.
  template<>
    int 
    collate<char>::_M_compare_helper(const char*, const char*) const;

  template<>
    size_t
    collate<char>::_M_transform_helper(char*, const char*, size_t) const;

#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    int 
    collate<wchar_t>::_M_compare_helper(const wchar_t*, const wchar_t*) const;

  template<>
    size_t
    collate<wchar_t>::_M_transform_helper(wchar_t*, const wchar_t*, 
					  size_t) const;
#endif

  template<typename _CharT>
    class collate_byname : public collate<_CharT>
    {
    public:
      // Types:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      explicit 
      collate_byname(const char* __s, size_t __refs = 0)
      : collate<_CharT>(__refs) 
      { _S_create_c_locale(_M_c_locale_collate, __s); }

    protected:
      virtual 
      ~collate_byname() { }
    };


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

      static locale::id id;

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
    locale::id time_get<_CharT, _InIter>::id;

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

      static locale::id id;

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

    protected:
      virtual 
      ~time_put() { }

      virtual iter_type 
      do_put(iter_type __s, ios_base&, char_type, const tm* /*__t*/, 
	     char /*__format*/, char /*__mod*/) const
      { return __s; }
    };

  template<typename _CharT, typename _OutIter>
    locale::id time_put<_CharT, _OutIter>::id;

  template<typename _CharT, typename _OutIter>
    class time_put_byname : public time_put<_CharT, _OutIter>
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

      static locale::id id;

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

  template<typename _CharT, typename _InIter>
    locale::id money_get<_CharT, _InIter>::id;

  template<typename _CharT, typename _OutIter>
    class money_put : public locale::facet
    {
    public:
      typedef _CharT              char_type;
      typedef _OutIter            iter_type;
      typedef basic_string<_CharT> string_type;

      static locale::id id;

      explicit 
      money_put(size_t __refs = 0) : locale::facet(__refs) { }

      iter_type 
      put(iter_type __s, bool __intl, ios_base& __f,
	  char_type __fill, long double __units) const
      { return this->do_put(__s, __intl, __f, __fill, __units); }

      iter_type 
      put(iter_type __s, bool __intl, ios_base& __f,
	  char_type __fill, const string_type& __digits) const
      { return this->do_put(__s, __intl, __f, __fill, __digits); }

    protected:
      virtual 
      ~money_put() { }

      virtual iter_type
      do_put(iter_type __s, bool __intl, ios_base& __io, char_type __fill,
	     long double __units) const;

      virtual iter_type
      do_put(iter_type __s, bool __intl, ios_base& __io, char_type __fill,
	     const string_type& __digits) const;
    };

  template<typename _CharT, typename _OutIter>
    locale::id money_put<_CharT, _OutIter>::id;

  struct money_base
  {
    enum part { none, space, symbol, sign, value };
    struct pattern { char field[4]; };

    static const pattern _S_default_pattern;

    // Construct and return valid pattern consisting of some combination of:
    // space none symbol sign value
    static pattern 
    _S_construct_pattern(char __preceeds, char __space, char __posn);
  };

  template<typename _CharT, bool _Intl>
    class moneypunct : public locale::facet, public money_base
    {
    public:
      // Types:
      typedef _CharT 			char_type;
      typedef basic_string<_CharT> 	string_type;

      static const bool intl = _Intl;
      static locale::id id;

    private:
      char_type 	_M_decimal_point;
      char_type 	_M_thousands_sep;
      string 		_M_grouping;
      string_type 	_M_curr_symbol;
      string_type 	_M_positive_sign;
      string_type 	_M_negative_sign;
      int 		_M_frac_digits;
      pattern 		_M_pos_format;
      pattern 		_M_neg_format;

    public:
      explicit 
      moneypunct(size_t __refs = 0) : locale::facet(__refs)
      { _M_initialize_moneypunct(); }

      explicit 
      moneypunct(__c_locale __cloc, size_t __refs = 0) : locale::facet(__refs)
      { _M_initialize_moneypunct(__cloc); }

      char_type
      decimal_point() const
      { return this->do_decimal_point(); }
      
      char_type
      thousands_sep() const
      { return this->do_thousands_sep(); }
      
      string 
      grouping() const
      { return this->do_grouping(); }

      string_type  
      curr_symbol() const
      { return this->do_curr_symbol(); }

      string_type  
      positive_sign() const
      { return this->do_positive_sign(); }

      string_type  
      negative_sign() const
      { return this->do_negative_sign(); }

      int          
      frac_digits() const
      { return this->do_frac_digits(); }

      pattern      
      pos_format() const
      { return this->do_pos_format(); }

      pattern      
      neg_format() const
      { return this->do_neg_format(); }

    protected:
      virtual 
      ~moneypunct() { }

      virtual char_type
      do_decimal_point() const
      { return _M_decimal_point; }
      
      virtual char_type
      do_thousands_sep() const
      { return _M_thousands_sep; }
      
      virtual string 
      do_grouping() const
      { return _M_grouping; }

      virtual string_type  
      do_curr_symbol()   const
      { return _M_curr_symbol; }

      virtual string_type  
      do_positive_sign() const
      { return _M_positive_sign; }

      virtual string_type  
      do_negative_sign() const
      { return _M_negative_sign; }

      virtual int          
      do_frac_digits() const
      { return _M_frac_digits; }

      virtual pattern      
      do_pos_format() const
      { return _M_pos_format; }

      virtual pattern      
      do_neg_format() const
      { return _M_neg_format; }

      // For use at construction time only.
       void 
       _M_initialize_moneypunct(__c_locale __cloc = NULL);
    };

  template<typename _CharT, bool _Intl>
    locale::id moneypunct<_CharT, _Intl>::id;

  template<typename _CharT, bool _Intl>
    const bool moneypunct<_CharT, _Intl>::intl;

  // NB: Cannot be made generic. 
  template<typename _CharT, bool _Intl>
    void
    moneypunct<_CharT, _Intl>::_M_initialize_moneypunct(__c_locale)
    { }

  template<> 
    void
    moneypunct<char, true>::_M_initialize_moneypunct(__c_locale __cloc);

  template<> 
    void
    moneypunct<char, false>::_M_initialize_moneypunct(__c_locale __cloc);

#ifdef _GLIBCPP_USE_WCHAR_T
  template<> 
    void
    moneypunct<wchar_t, true>::_M_initialize_moneypunct(__c_locale __cloc);

  template<> 
    void
    moneypunct<wchar_t, false>::_M_initialize_moneypunct(__c_locale __cloc);
#endif

  template<typename _CharT, bool _Intl>
    class moneypunct_byname : public moneypunct<_CharT, _Intl>
    {
      __c_locale			_M_c_locale_moneypunct;

    public:
      typedef _CharT 			char_type;
      typedef basic_string<_CharT> 	string_type;

      static const bool intl = _Intl;

      explicit 
      moneypunct_byname(const char* __s, size_t __refs = 0)
      : moneypunct<_CharT, _Intl>(__refs)
      {
	_S_create_c_locale(_M_c_locale_moneypunct, __s);
	_M_initialize_moneypunct(_M_c_locale_moneypunct);	
      }

    protected:
      virtual 
      ~moneypunct_byname() 
      { _S_destroy_c_locale(_M_c_locale_moneypunct); }
    };

  template<typename _CharT, bool _Intl>
    const bool moneypunct_byname<_CharT, _Intl>::intl;


  struct messages_base
  {
    typedef int catalog;
  };

  template<typename _CharT>
    class messages : public locale::facet, public messages_base
    {
    public:
      // Types:
      typedef _CharT 			char_type;
      typedef basic_string<_CharT> 	string_type;

    protected:
      // Underlying "C" library locale information saved from
      // initialization, needed by messages_byname as well.
      __c_locale			_M_c_locale_messages;
      #if 1
      // Only needed if glibc < 2.3
      const char*			_M_name_messages;
      #endif

    public:
      static locale::id id;

      explicit 
      messages(size_t __refs = 0) 
      : locale::facet(__refs), _M_c_locale_messages(NULL), 
      _M_name_messages("C")
      { }

      // Non-standard.
      explicit 
      messages(__c_locale __cloc, const char* __name, size_t __refs = 0) 
      : locale::facet(__refs)
      { 
	_M_name_messages = __name;
	if (__cloc)
	  _M_c_locale_messages = _S_clone_c_locale(__cloc); 
      }

      catalog 
      open(const basic_string<char>& __s, const locale& __loc) const
      { return this->do_open(__s, __loc); }

      // Non-standard and unorthodox, yet effective.
      catalog 
      open(const basic_string<char>&, const locale&, const char*) const;

      string_type  
      get(catalog __c, int __set, int __msgid, const string_type& __s) const
      { return this->do_get(__c, __set, __msgid, __s); }

      void 
      close(catalog __c) const
      { return this->do_close(__c); }

    protected:
      virtual 
      ~messages();

      virtual catalog 
      do_open(const basic_string<char>&, const locale&) const;

      virtual string_type  
      do_get(catalog, int, int, const string_type& __dfault) const;

      virtual void    
      do_close(catalog) const;

      // Returns a locale and codeset-converted string, given a char* message.
      char*
      _M_convert_to_char(const string_type& __msg) const
      {
	// XXX
	return reinterpret_cast<char*>(const_cast<_CharT*>(__msg.c_str()));
      }

      // Returns a locale and codeset-converted string, given a char* message.
      string_type
      _M_convert_from_char(char* __msg) const
      {
	// Length of message string without terminating null.
	size_t __len = char_traits<char>::length(__msg) - 1;

	// "everybody can easily convert the string using
	// mbsrtowcs/wcsrtombs or with iconv()"
#if 0
	// Convert char* to _CharT in locale used to open catalog.
	// XXX need additional template parameter on messages class for this..
	// typedef typename codecvt<char, _CharT, _StateT> __codecvt_type;
	typedef typename codecvt<char, _CharT, mbstate_t> __codecvt_type;      

	__codecvt_type::state_type __state;
	// XXX may need to initialize state.
	//initialize_state(__state._M_init());
	
	char* __from_next;
	// XXX what size for this string?
	_CharT* __to = static_cast<_CharT*>(__builtin_alloca(__len + 1));
	const __codecvt_type& __cvt = use_facet<__codecvt_type>(_M_locale_conv);
	__cvt.out(__state, __msg, __msg + __len, __from_next,
		  __to, __to + __len + 1, __to_next);
	return string_type(__to);
#endif
#if 0
	typedef ctype<_CharT> __ctype_type;
	// const __ctype_type& __cvt = use_facet<__ctype_type>(_M_locale_msg);
	const __ctype_type& __cvt = use_facet<__ctype_type>(locale());
	// XXX Again, proper length of converted string an issue here.
	// For now, assume the converted length is not larger.
	_CharT* __dest = static_cast<_CharT*>(__builtin_alloca(__len + 1));
	__cvt.widen(__msg, __msg + __len, __dest);
	return basic_string<_CharT>(__dest);
#endif
	return string_type();
      }
     };

  template<typename _CharT>
    locale::id messages<_CharT>::id;

  // Specializations for required instantiations.
  template<>
    string
    messages<char>::do_get(catalog, int, int, const string&) const;

  // Include host and configuration specific messages virtual functions.
  #include <bits/messages_members.h>

  template<typename _CharT>
    class messages_byname : public messages<_CharT>
    {
    public:
      typedef _CharT               	char_type;
      typedef basic_string<_CharT> 	string_type;

      explicit 
      messages_byname(const char* __s, size_t __refs = 0)
      : messages<_CharT>(__refs) 
      { 
	_S_create_c_locale(_M_c_locale_messages, __s); 
	_M_name_messages = __s;
      }

    protected:
      virtual 
      ~messages_byname() 
      { }
    };


  // Subclause convenience interfaces, inlines.
  // NB: These are inline because, when used in a loop, some compilers
  // can hoist the body out of the loop; then it's just as fast as the
  // C is*() function.
  template<typename _CharT>
    inline bool 
    isspace(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::space, __c); }

  template<typename _CharT>
    inline bool 
    isprint(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::print, __c); }

  template<typename _CharT>
    inline bool 
    iscntrl(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::cntrl, __c); }

  template<typename _CharT>
    inline bool 
    isupper(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::upper, __c); }

  template<typename _CharT>
    inline bool islower(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::lower, __c); }

  template<typename _CharT>
    inline bool 
    isalpha(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::alpha, __c); }

  template<typename _CharT>
    inline bool 
    isdigit(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::digit, __c); }

  template<typename _CharT>
    inline bool 
    ispunct(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::punct, __c); }

  template<typename _CharT>
    inline bool 
    isxdigit(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::xdigit, __c); }

  template<typename _CharT>
    inline bool 
    isalnum(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::alnum, __c); }

  template<typename _CharT>
    inline bool 
    isgraph(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).is(ctype_base::graph, __c); }

  template<typename _CharT>
    inline _CharT 
    toupper(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).toupper(__c); }

  template<typename _CharT>
    inline _CharT 
    tolower(_CharT __c, const locale& __loc)
    { return use_facet<ctype<_CharT> >(__loc).tolower(__c); }
} // namespace std

#endif
