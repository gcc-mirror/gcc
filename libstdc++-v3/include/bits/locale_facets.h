// Locale support -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
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

/** @file locale_facets.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _LOCALE_FACETS_H
#define _LOCALE_FACETS_H 1

#pragma GCC system_header

#include <ctime>	// For struct tm
#include <cwctype>	// For wctype_t
#include <iosfwd>
#include <bits/ios_base.h>  // For ios_base, ios_base::iostate
#include <streambuf>

namespace std
{
  // NB: Don't instantiate required wchar_t facets if no wchar_t support.
#ifdef _GLIBCXX_USE_WCHAR_T
# define  _GLIBCXX_NUM_FACETS 28
#else
# define  _GLIBCXX_NUM_FACETS 14
#endif

  // Convert string to numeric value of type _Tv and store results.  
  // NB: This is specialized for all required types, there is no
  // generic definition.
  template<typename _Tv>
    void
    __convert_to_v(const char* __in, _Tv& __out, ios_base::iostate& __err, 
		   const __c_locale& __cloc);

  // Explicit specializations for required types.
  template<>
    void
    __convert_to_v(const char*, float&, ios_base::iostate&, 
		   const __c_locale&);

  template<>
    void
    __convert_to_v(const char*, double&, ios_base::iostate&, 
		   const __c_locale&);

  template<>
    void
    __convert_to_v(const char*, long double&, ios_base::iostate&, 
		   const __c_locale&);

  // NB: __pad is a struct, rather than a function, so it can be
  // partially-specialized.
  template<typename _CharT, typename _Traits>
    struct __pad
    {
      static void
      _S_pad(ios_base& __io, _CharT __fill, _CharT* __news, 
	     const _CharT* __olds, const streamsize __newlen, 
	     const streamsize __oldlen, const bool __num);
    };

  // Used by both numeric and monetary facets.
  // Check to make sure that the __grouping_tmp string constructed in
  // money_get or num_get matches the canonical grouping for a given
  // locale.
  // __grouping_tmp is parsed L to R
  // 1,222,444 == __grouping_tmp of "\1\3\3"
  // __grouping is parsed R to L
  // 1,222,444 == __grouping of "\3" == "\3\3\3"
  template<typename _CharT>
    bool
    __verify_grouping(const basic_string<_CharT>& __grouping, 
		      const basic_string<_CharT>& __grouping_tmp);

  // Used by both numeric and monetary facets.
  // Inserts "group separator" characters into an array of characters.
  // It's recursive, one iteration per group.  It moves the characters
  // in the buffer this way: "xxxx12345" -> "12,345xxx".  Call this
  // only with __gbeg != __gend.
  template<typename _CharT>
    _CharT*
    __add_grouping(_CharT* __s, _CharT __sep,  
		   const char* __gbeg, const char* __gend, 
		   const _CharT* __first, const _CharT* __last);

  // This template permits specializing facet output code for
  // ostreambuf_iterator.  For ostreambuf_iterator, sputn is
  // significantly more efficient than incrementing iterators.
  template<typename _CharT>
    inline
    ostreambuf_iterator<_CharT>
    __write(ostreambuf_iterator<_CharT> __s, const _CharT* __ws, int __len)
    {
      __s._M_put(__ws, __len);
      return __s;
    }

  // This is the unspecialized form of the template.
  template<typename _CharT, typename _OutIter>
    inline
    _OutIter
    __write(_OutIter __s, const _CharT* __ws, int __len)
    {
      for (int __j = 0; __j < __len; __j++, ++__s)
	*__s = __ws[__j];
      return __s;
    }


  // 22.2.1.1  Template class ctype
  // Include host and configuration specific ctype enums for ctype_base.
  #include <bits/ctype_base.h>

  // Common base for ctype<_CharT>.  
  /**
   *  @brief  Common base for ctype facet
   *
   *  This template class provides implementations of the public functions
   *  that forward to the protected virtual functions.
   *
   *  This template also provides abtract stubs for the protected virtual
   *  functions.
  */
  template<typename _CharT>
    class __ctype_abstract_base : public locale::facet, public ctype_base
    {
    public:
      // Types:
      /// Typedef for the template parameter
      typedef _CharT char_type;

      /**
       *  @brief  Test char_type classification.
       *
       *  This function finds a mask M for @a c and compares it to mask @a m.
       *  It does so by returning the value of ctype<char_type>::do_is().
       *
       *  @param c  The char_type to compare the mask of.
       *  @param m  The mask to compare against.
       *  @return  (M & m) != 0.
      */
      bool 
      is(mask __m, char_type __c) const
      { return this->do_is(__m, __c); }

      /**
       *  @brief  Return a mask array.
       *
       *  This function finds the mask for each char_type in the range [lo,hi)
       *  and successively writes it to vec.  vec must have as many elements
       *  as the char array.  It does so by returning the value of
       *  ctype<char_type>::do_is().
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param vec  Pointer to an array of mask storage.
       *  @return  @a hi.
      */
      const char_type*
      is(const char_type *__lo, const char_type *__hi, mask *__vec) const   
      { return this->do_is(__lo, __hi, __vec); }

      /**
       *  @brief  Find char_type matching a mask
       *
       *  This function searches for and returns the first char_type c in
       *  [lo,hi) for which is(m,c) is true.  It does so by returning
       *  ctype<char_type>::do_scan_is().
       *
       *  @param m  The mask to compare against.
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  Pointer to matching char_type if found, else @a hi.
      */
      const char_type*
      scan_is(mask __m, const char_type* __lo, const char_type* __hi) const
      { return this->do_scan_is(__m, __lo, __hi); }

      /**
       *  @brief  Find char_type not matching a mask
       *
       *  This function searches for and returns the first char_type c in
       *  [lo,hi) for which is(m,c) is false.  It does so by returning
       *  ctype<char_type>::do_scan_not().
       *
       *  @param m  The mask to compare against.
       *  @param lo  Pointer to first char in range.
       *  @param hi  Pointer to end of range.
       *  @return  Pointer to non-matching char if found, else @a hi.
      */
      const char_type*
      scan_not(mask __m, const char_type* __lo, const char_type* __hi) const
      { return this->do_scan_not(__m, __lo, __hi); }

      /**
       *  @brief  Convert to uppercase.
       *
       *  This function converts the argument to uppercase if possible.
       *  If not possible (for example, '2'), returns the argument.  It does
       *  so by returning ctype<char_type>::do_toupper().
       *
       *  @param c  The char_type to convert.
       *  @return  The uppercase char_type if convertible, else @a c.
      */
      char_type 
      toupper(char_type __c) const
      { return this->do_toupper(__c); }

      /**
       *  @brief  Convert array to uppercase.
       *
       *  This function converts each char_type in the range [lo,hi) to
       *  uppercase if possible.  Other elements remain untouched.  It does so
       *  by returning ctype<char_type>:: do_toupper(lo, hi).
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      const char_type*
      toupper(char_type *__lo, const char_type* __hi) const
      { return this->do_toupper(__lo, __hi); }

      /**
       *  @brief  Convert to lowercase.
       *
       *  This function converts the argument to lowercase if possible.  If
       *  not possible (for example, '2'), returns the argument.  It does so
       *  by returning ctype<char_type>::do_tolower(c).
       *
       *  @param c  The char_type to convert.
       *  @return  The lowercase char_type if convertible, else @a c.
      */
      char_type
      tolower(char_type __c) const
      { return this->do_tolower(__c); }

      /**
       *  @brief  Convert array to lowercase.
       *
       *  This function converts each char_type in the range [lo,hi) to
       *  lowercase if possible.  Other elements remain untouched.  It does so
       *  by returning ctype<char_type>:: do_tolower(lo, hi).
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      const char_type*
      tolower(char_type* __lo, const char_type* __hi) const
      { return this->do_tolower(__lo, __hi); }

      /**
       *  @brief  Widen char to char_type
       *
       *  This function converts the char argument to char_type using the
       *  simplest reasonable transformation.  It does so by returning
       *  ctype<char_type>::do_widen(c).
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char to convert.
       *  @return  The converted char_type.
      */
      char_type
      widen(char __c) const
      { return this->do_widen(__c); }

      /**
       *  @brief  Widen array to char_type
       *
       *  This function converts each char in the input to char_type using the
       *  simplest reasonable transformation.  It does so by returning
       *  ctype<char_type>::do_widen(c).
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      const char*
      widen(const char* __lo, const char* __hi, char_type* __to) const
      { return this->do_widen(__lo, __hi, __to); }

      /**
       *  @brief  Narrow char_type to char
       *
       *  This function converts the char_type to char using the simplest
       *  reasonable transformation.  If the conversion fails, dfault is
       *  returned instead.  It does so by returning
       *  ctype<char_type>::do_narrow(c).
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char_type to convert.
       *  @param dfault  Char to return if conversion fails.
       *  @return  The converted char.
      */
      char 
      narrow(char_type __c, char __dfault) const
      { return this->do_narrow(__c, __dfault); }

      /**
       *  @brief  Narrow array to char array
       *
       *  This function converts each char_type in the input to char using the
       *  simplest reasonable transformation and writes the results to the
       *  destination array.  For any char_type in the input that cannot be
       *  converted, @a dfault is used instead.  It does so by returning
       *  ctype<char_type>::do_narrow(lo, hi, dfault, to).
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param dfault  Char to use if conversion fails.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      const char_type*
      narrow(const char_type* __lo, const char_type* __hi,
	      char __dfault, char *__to) const
      { return this->do_narrow(__lo, __hi, __dfault, __to); }

    protected:
      explicit 
      __ctype_abstract_base(size_t __refs = 0): facet(__refs) { }

      virtual 
      ~__ctype_abstract_base() { }
      
      /**
       *  @brief  Test char_type classification.
       *
       *  This function finds a mask M for @a c and compares it to mask @a m.
       *
       *  do_is() is a hook for a derived facet to change the behavior of
       *  classifying.  do_is() must always return the same result for the
       *  same input.
       *
       *  @param c  The char_type to find the mask of.
       *  @param m  The mask to compare against.
       *  @return  (M & m) != 0.
      */
      virtual bool
      do_is(mask __m, char_type __c) const = 0;

      /**
       *  @brief  Return a mask array.
       *
       *  This function finds the mask for each char_type in the range [lo,hi)
       *  and successively writes it to vec.  vec must have as many elements
       *  as the input.
       *
       *  do_is() is a hook for a derived facet to change the behavior of
       *  classifying.  do_is() must always return the same result for the
       *  same input.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param vec  Pointer to an array of mask storage.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_is(const char_type* __lo, const char_type* __hi, 
	    mask* __vec) const = 0;

      /**
       *  @brief  Find char_type matching mask
       *
       *  This function searches for and returns the first char_type c in
       *  [lo,hi) for which is(m,c) is true.
       *
       *  do_scan_is() is a hook for a derived facet to change the behavior of
       *  match searching.  do_is() must always return the same result for the
       *  same input.
       *
       *  @param m  The mask to compare against.
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  Pointer to a matching char_type if found, else @a hi.
      */
      virtual const char_type*
      do_scan_is(mask __m, const char_type* __lo,
		 const char_type* __hi) const = 0;

      /**
       *  @brief  Find char_type not matching mask
       *
       *  This function searches for and returns a pointer to the first
       *  char_type c of [lo,hi) for which is(m,c) is false.
       *
       *  do_scan_is() is a hook for a derived facet to change the behavior of
       *  match searching.  do_is() must always return the same result for the
       *  same input.
       *
       *  @param m  The mask to compare against.
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  Pointer to a non-matching char_type if found, else @a hi.
      */
      virtual const char_type*
      do_scan_not(mask __m, const char_type* __lo, 
		  const char_type* __hi) const = 0;

      /**
       *  @brief  Convert to uppercase.
       *
       *  This virtual function converts the char_type argument to uppercase
       *  if possible.  If not possible (for example, '2'), returns the
       *  argument.
       *
       *  do_toupper() is a hook for a derived facet to change the behavior of
       *  uppercasing.  do_toupper() must always return the same result for
       *  the same input.
       *
       *  @param c  The char_type to convert.
       *  @return  The uppercase char_type if convertible, else @a c.
      */
      virtual char_type 
      do_toupper(char_type) const = 0;

      /**
       *  @brief  Convert array to uppercase.
       *
       *  This virtual function converts each char_type in the range [lo,hi)
       *  to uppercase if possible.  Other elements remain untouched.
       *
       *  do_toupper() is a hook for a derived facet to change the behavior of
       *  uppercasing.  do_toupper() must always return the same result for
       *  the same input.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_toupper(char_type* __lo, const char_type* __hi) const = 0;

      /**
       *  @brief  Convert to lowercase.
       *
       *  This virtual function converts the argument to lowercase if
       *  possible.  If not possible (for example, '2'), returns the argument.
       *
       *  do_tolower() is a hook for a derived facet to change the behavior of
       *  lowercasing.  do_tolower() must always return the same result for
       *  the same input.
       *
       *  @param c  The char_type to convert.
       *  @return  The lowercase char_type if convertible, else @a c.
      */
      virtual char_type
      do_tolower(char_type) const = 0;

      /**
       *  @brief  Convert array to lowercase.
       *
       *  This virtual function converts each char_type in the range [lo,hi)
       *  to lowercase if possible.  Other elements remain untouched.
       *
       *  do_tolower() is a hook for a derived facet to change the behavior of
       *  lowercasing.  do_tolower() must always return the same result for
       *  the same input.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_tolower(char_type* __lo, const char_type* __hi) const = 0;
      
      /**
       *  @brief  Widen char
       *
       *  This virtual function converts the char to char_type using the
       *  simplest reasonable transformation.
       *
       *  do_widen() is a hook for a derived facet to change the behavior of
       *  widening.  do_widen() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char to convert.
       *  @return  The converted char_type
      */
      virtual char_type 
      do_widen(char) const = 0;

      /**
       *  @brief  Widen char array
       *
       *  This function converts each char in the input to char_type using the
       *  simplest reasonable transformation.
       *
       *  do_widen() is a hook for a derived facet to change the behavior of
       *  widening.  do_widen() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start range.
       *  @param hi  Pointer to end of range.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      virtual const char*
      do_widen(const char* __lo, const char* __hi, 
	       char_type* __dest) const = 0;

      /**
       *  @brief  Narrow char_type to char
       *
       *  This virtual function converts the argument to char using the
       *  simplest reasonable transformation.  If the conversion fails, dfault
       *  is returned instead.
       *
       *  do_narrow() is a hook for a derived facet to change the behavior of
       *  narrowing.  do_narrow() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char_type to convert.
       *  @param dfault  Char to return if conversion fails.
       *  @return  The converted char.
      */
      virtual char 
      do_narrow(char_type, char __dfault) const = 0;

      /**
       *  @brief  Narrow char_type array to char
       *
       *  This virtual function converts each char_type in the range [lo,hi) to
       *  char using the simplest reasonable transformation and writes the
       *  results to the destination array.  For any element in the input that
       *  cannot be converted, @a dfault is used instead.
       *
       *  do_narrow() is a hook for a derived facet to change the behavior of
       *  narrowing.  do_narrow() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param dfault  Char to use if conversion fails.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_narrow(const char_type* __lo, const char_type* __hi,
		char __dfault, char* __dest) const = 0;
    };

  // NB: Generic, mostly useless implementation.
  /**
   *  @brief  Template ctype facet
   *
   *  This template class defines classification and conversion functions for
   *  character sets.  It wraps <cctype> functionality.  Ctype gets used by
   *  streams for many I/O operations.
   *
   *  This template provides the protected virtual functions the developer
   *  will have to replace in a derived class or specialization to make a
   *  working facet.  The public functions that access them are defined in
   *  __ctype_abstract_base, to allow for implementation flexibility.  See
   *  ctype<wchar_t> for an example.  The functions are documented in
   *  __ctype_abstract_base.
   *
   *  Note: implementations are provided for all the protected virtual
   *  functions, but will likely not be useful.
  */
  template<typename _CharT>
    class ctype : public __ctype_abstract_base<_CharT>
    {
    public:
      // Types:
      typedef _CharT 		  	char_type;
      typedef typename ctype::mask 	mask;

      /// The facet id for ctype<char_type>
      static locale::id 	       	id;

      explicit 
      ctype(size_t __refs = 0) : __ctype_abstract_base<_CharT>(__refs) { }

   protected:
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
      do_toupper(char_type __c) const;

      virtual const char_type*
      do_toupper(char_type* __lo, const char_type* __hi) const;

      virtual char_type 
      do_tolower(char_type __c) const;

      virtual const char_type*
      do_tolower(char_type* __lo, const char_type* __hi) const;

      virtual char_type 
      do_widen(char __c) const;

      virtual const char*
      do_widen(const char* __lo, const char* __hi, char_type* __dest) const;

      virtual char 
      do_narrow(char_type, char __dfault) const;

      virtual const char_type*
      do_narrow(const char_type* __lo, const char_type* __hi,
		char __dfault, char* __dest) const;
    };

  template<typename _CharT>
    locale::id ctype<_CharT>::id;

  // 22.2.1.3  ctype<char> specialization.
  /**
   *  @brief  The ctype<char> specialization.
   *
   *  This class defines classification and conversion functions for
   *  the char type.  It gets used by char streams for many I/O
   *  operations.  The char specialization provides a number of
   *  optimizations as well.
  */
  template<>
    class ctype<char> : public locale::facet, public ctype_base
    {
    public:
      // Types:
      /// Typedef for the template parameter char.
      typedef char 	       	char_type;

    protected:
      // Data Members:
      __c_locale		_M_c_locale_ctype;
      bool 		       	_M_del;
      __to_type 	       	_M_toupper;
      __to_type  	       	_M_tolower;
      const mask*              	_M_table;
      mutable char		_M_widen_ok;
      mutable char		_M_widen[1 + static_cast<unsigned char>(-1)];
      mutable char		_M_narrow[1 + static_cast<unsigned char>(-1)];
      mutable char		_M_narrow_ok;	// 0 uninitialized, 1 init,
						// 2 non-consecutive
      
    public:
      /// The facet id for ctype<char>
      static locale::id        id;
      /// The size of the mask table.  It is SCHAR_MAX + 1.
      static const size_t      table_size = 1 + static_cast<unsigned char>(-1);

      /**
       *  @brief  Constructor performs initialization.
       *
       *  This is the constructor provided by the standard.
       *
       *  @param table If non-zero, table is used as the per-char mask.
       *               Else classic_table() is used.
       *  @param del   If true, passes ownership of table to this facet.
       *  @param refs  Passed to the base facet class.
      */
      explicit 
      ctype(const mask* __table = 0, bool __del = false, size_t __refs = 0);

      /**
       *  @brief  Constructor performs static initialization.
       *
       *  This constructor is used to construct the initial C locale facet.
       *
       *  @param cloc  Handle to C locale data.
       *  @param table If non-zero, table is used as the per-char mask.
       *  @param del   If true, passes ownership of table to this facet.
       *  @param refs  Passed to the base facet class.
      */
      explicit 
      ctype(__c_locale __cloc, const mask* __table = 0, bool __del = false, 
	    size_t __refs = 0);

      /**
       *  @brief  Test char classification.
       *
       *  This function compares the mask table[c] to @a m.
       *
       *  @param c  The char to compare the mask of.
       *  @param m  The mask to compare against.
       *  @return  True if m & table[c] is true, false otherwise.
      */
      inline bool
      is(mask __m, char __c) const;
 
      /**
       *  @brief  Return a mask array.
       *
       *  This function finds the mask for each char in the range [lo, hi) and
       *  successively writes it to vec.  vec must have as many elements as
       *  the char array.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param vec  Pointer to an array of mask storage.
       *  @return  @a hi.
      */
      inline const char*
      is(const char* __lo, const char* __hi, mask* __vec) const;
 
      /**
       *  @brief  Find char matching a mask
       *
       *  This function searches for and returns the first char in [lo,hi) for
       *  which is(m,char) is true.
       *
       *  @param m  The mask to compare against.
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  Pointer to a matching char if found, else @a hi.
      */
      inline const char*
      scan_is(mask __m, const char* __lo, const char* __hi) const;

      /**
       *  @brief  Find char not matching a mask
       *
       *  This function searches for and returns a pointer to the first char
       *  in [lo,hi) for which is(m,char) is false.
       *
       *  @param m  The mask to compare against.
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  Pointer to a non-matching char if found, else @a hi.
      */
      inline const char*
      scan_not(mask __m, const char* __lo, const char* __hi) const;
     
      /**
       *  @brief  Convert to uppercase.
       *
       *  This function converts the char argument to uppercase if possible.
       *  If not possible (for example, '2'), returns the argument.
       *
       *  toupper() acts as if it returns ctype<char>::do_toupper(c).
       *  do_toupper() must always return the same result for the same input.
       *
       *  @param c  The char to convert.
       *  @return  The uppercase char if convertible, else @a c.
      */
      char_type 
      toupper(char_type __c) const
      { return this->do_toupper(__c); }

      /**
       *  @brief  Convert array to uppercase.
       *
       *  This function converts each char in the range [lo,hi) to uppercase
       *  if possible.  Other chars remain untouched.
       *
       *  toupper() acts as if it returns ctype<char>:: do_toupper(lo, hi).
       *  do_toupper() must always return the same result for the same input.
       *
       *  @param lo  Pointer to first char in range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      const char_type*
      toupper(char_type *__lo, const char_type* __hi) const
      { return this->do_toupper(__lo, __hi); }

      /**
       *  @brief  Convert to lowercase.
       *
       *  This function converts the char argument to lowercase if possible.
       *  If not possible (for example, '2'), returns the argument.
       *
       *  tolower() acts as if it returns ctype<char>::do_tolower(c).
       *  do_tolower() must always return the same result for the same input.
       *
       *  @param c  The char to convert.
       *  @return  The lowercase char if convertible, else @a c.
      */
      char_type 
      tolower(char_type __c) const
      { return this->do_tolower(__c); }

      /**
       *  @brief  Convert array to lowercase.
       *
       *  This function converts each char in the range [lo,hi) to lowercase
       *  if possible.  Other chars remain untouched.
       *
       *  tolower() acts as if it returns ctype<char>:: do_tolower(lo, hi).
       *  do_tolower() must always return the same result for the same input.
       *
       *  @param lo  Pointer to first char in range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      const char_type*
      tolower(char_type* __lo, const char_type* __hi) const
      { return this->do_tolower(__lo, __hi); }

      /**
       *  @brief  Widen char
       *
       *  This function converts the char to char_type using the simplest
       *  reasonable transformation.  For an underived ctype<char> facet, the
       *  argument will be returned unchanged.
       *
       *  This function works as if it returns ctype<char>::do_widen(c).
       *  do_widen() must always return the same result for the same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char to convert.
       *  @return  The converted character.
      */
      char_type 
      widen(char __c) const
      { 
	if (_M_widen_ok) return _M_widen[__c];
	this->_M_widen_init();
	return this->do_widen(__c);
      }

      /**
       *  @brief  Widen char array
       *
       *  This function converts each char in the input to char using the
       *  simplest reasonable transformation.  For an underived ctype<char>
       *  facet, the argument will be copied unchanged.
       *
       *  This function works as if it returns ctype<char>::do_widen(c).
       *  do_widen() must always return the same result for the same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to first char in range.
       *  @param hi  Pointer to end of range.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      const char*
      widen(const char* __lo, const char* __hi, char_type* __to) const
      {
	if (_M_widen_ok == 1)
	  {
	    memcpy(__to, __lo, __hi - __lo);
	    return __hi;
	  }
	if (!_M_widen_ok) _M_widen_init();
	return this->do_widen(__lo, __hi, __to);
      }

      /**
       *  @brief  Narrow char
       *
       *  This function converts the char to char using the simplest
       *  reasonable transformation.  If the conversion fails, dfault is
       *  returned instead.  For an underived ctype<char> facet, @a c
       *  will be returned unchanged.
       *
       *  This function works as if it returns ctype<char>::do_narrow(c).
       *  do_narrow() must always return the same result for the same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char to convert.
       *  @param dfault  Char to return if conversion fails.
       *  @return  The converted character.
      */
      char 
      narrow(char_type __c, char __dfault) const
      {
	if (_M_narrow[__c]) return _M_narrow[__c];
	const char __t = do_narrow(__c, __dfault);
	if (__t != __dfault) _M_narrow[__c] = __t;
	return __t;
      }

      /**
       *  @brief  Narrow char array
       *
       *  This function converts each char in the input to char using the
       *  simplest reasonable transformation and writes the results to the
       *  destination array.  For any char in the input that cannot be
       *  converted, @a dfault is used instead.  For an underived ctype<char>
       *  facet, the argument will be copied unchanged.
       *
       *  This function works as if it returns ctype<char>::do_narrow(lo, hi,
       *  dfault, to).  do_narrow() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param dfault  Char to use if conversion fails.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      const char_type*
      narrow(const char_type* __lo, const char_type* __hi,
	     char __dfault, char *__to) const
      {
	if (__builtin_expect(_M_narrow_ok == 1,true))
	  {
	    memcpy(__to, __lo, __hi - __lo);
	    return __hi;
	  }
	if (!_M_narrow_ok)
	  _M_narrow_init();
	return this->do_narrow(__lo, __hi, __dfault, __to);
      }

    protected:
      /// Returns a pointer to the mask table provided to the constructor, or
      /// the default from classic_table() if none was provided.
      const mask* 
      table() const throw()
      { return _M_table; }

      /// Returns a pointer to the C locale mask table.
      static const mask* 
      classic_table() throw();

      /**
       *  @brief  Destructor.
       *
       *  This function deletes table() if @a del was true in the
       *  constructor.
      */
      virtual 
      ~ctype();

      /**
       *  @brief  Convert to uppercase.
       *
       *  This virtual function converts the char argument to uppercase if
       *  possible.  If not possible (for example, '2'), returns the argument.
       *
       *  do_toupper() is a hook for a derived facet to change the behavior of
       *  uppercasing.  do_toupper() must always return the same result for
       *  the same input.
       *
       *  @param c  The char to convert.
       *  @return  The uppercase char if convertible, else @a c.
      */
      virtual char_type 
      do_toupper(char_type) const;

      /**
       *  @brief  Convert array to uppercase.
       *
       *  This virtual function converts each char in the range [lo,hi) to
       *  uppercase if possible.  Other chars remain untouched.
       *
       *  do_toupper() is a hook for a derived facet to change the behavior of
       *  uppercasing.  do_toupper() must always return the same result for
       *  the same input.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_toupper(char_type* __lo, const char_type* __hi) const;

      /**
       *  @brief  Convert to lowercase.
       *
       *  This virtual function converts the char argument to lowercase if
       *  possible.  If not possible (for example, '2'), returns the argument.
       *
       *  do_tolower() is a hook for a derived facet to change the behavior of
       *  lowercasing.  do_tolower() must always return the same result for
       *  the same input.
       *
       *  @param c  The char to convert.
       *  @return  The lowercase char if convertible, else @a c.
      */
      virtual char_type 
      do_tolower(char_type) const;

      /**
       *  @brief  Convert array to lowercase.
       *
       *  This virtual function converts each char in the range [lo,hi) to
       *  lowercase if possible.  Other chars remain untouched.
       *
       *  do_tolower() is a hook for a derived facet to change the behavior of
       *  lowercasing.  do_tolower() must always return the same result for
       *  the same input.
       *
       *  @param lo  Pointer to first char in range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_tolower(char_type* __lo, const char_type* __hi) const;
      
      /**
       *  @brief  Widen char
       *
       *  This virtual function converts the char to char using the simplest
       *  reasonable transformation.  For an underived ctype<char> facet, the
       *  argument will be returned unchanged.
       *
       *  do_widen() is a hook for a derived facet to change the behavior of
       *  widening.  do_widen() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char to convert.
       *  @return  The converted character.
      */
      virtual char_type 
      do_widen(char __c) const
      { return __c; }

      /**
       *  @brief  Widen char array
       *
       *  This function converts each char in the range [lo,hi) to char using
       *  the simplest reasonable transformation.  For an underived
       *  ctype<char> facet, the argument will be copied unchanged.
       *
       *  do_widen() is a hook for a derived facet to change the behavior of
       *  widening.  do_widen() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      virtual const char*
      do_widen(const char* __lo, const char* __hi, char_type* __dest) const
      {
	memcpy(__dest, __lo, __hi - __lo);
	return __hi;
      }

      /**
       *  @brief  Narrow char
       *
       *  This virtual function converts the char to char using the simplest
       *  reasonable transformation.  If the conversion fails, dfault is
       *  returned instead.  For an underived ctype<char> facet, @a c will be
       *  returned unchanged.
       *
       *  do_narrow() is a hook for a derived facet to change the behavior of
       *  narrowing.  do_narrow() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char to convert.
       *  @param dfault  Char to return if conversion fails.
       *  @return  The converted char.
      */
      virtual char 
      do_narrow(char_type __c, char) const
      { return __c; }

      /**
       *  @brief  Narrow char array to char array
       *
       *  This virtual function converts each char in the range [lo,hi) to
       *  char using the simplest reasonable transformation and writes the
       *  results to the destination array.  For any char in the input that
       *  cannot be converted, @a dfault is used instead.  For an underived
       *  ctype<char> facet, the argument will be copied unchanged.
       *
       *  do_narrow() is a hook for a derived facet to change the behavior of
       *  narrowing.  do_narrow() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param dfault  Char to use if conversion fails.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_narrow(const char_type* __lo, const char_type* __hi,
		char, char* __dest) const
      {
	memcpy(__dest, __lo, __hi - __lo);
	return __hi;
      }

    private:

      void _M_widen_init() const
      {
	char __tmp[sizeof(_M_widen)];
	for (size_t __i = 0; __i < sizeof(_M_widen); ++__i)
	  __tmp[__i] = __i;
	do_widen(__tmp, __tmp + sizeof(__tmp), _M_widen);
	    
	_M_widen_ok = 1;
	// Set _M_widen_ok to 2 if memcpy can't be used.
	for (size_t __i = 0; __i < sizeof(_M_widen); ++__i)
	  if (__tmp[__i] != _M_widen[__i])
	    {
	      _M_widen_ok = 2;
	      break;
	    }
      }

      // Fill in the narrowing cache and flag whether all values are
      // valid or not.  _M_narrow_ok is set to 1 if the whole table is
      // narrowed, 2 if only some values could be narrowed.
      void _M_narrow_init() const
      {
	char __tmp[sizeof(_M_narrow)];
	for (size_t __i = 0; __i < sizeof(_M_narrow); ++__i)
	  __tmp[__i] = __i;
	do_narrow(__tmp, __tmp + sizeof(__tmp), 0, _M_narrow);

	// Check if any default values were created.  Do this by
	// renarrowing with a different default value and comparing.
	bool __consecutive = true;
	for (size_t __i = 0; __i < sizeof(_M_narrow); ++__i)
	  if (!_M_narrow[__i])
	    {
	      char __c;
	      do_narrow(__tmp + __i, __tmp + __i + 1, 1, &__c);
	      if (__c == 1)
		{
		  __consecutive = false;
		  break;
		}
	    }
	_M_narrow_ok = __consecutive ? 1 : 2;
      }
    };
 
  template<>
    const ctype<char>&
    use_facet<ctype<char> >(const locale& __loc);

#ifdef _GLIBCXX_USE_WCHAR_T
  // 22.2.1.3  ctype<wchar_t> specialization
  /**
   *  @brief  The ctype<wchar_t> specialization.
   *
   *  This class defines classification and conversion functions for the
   *  wchar_t type.  It gets used by wchar_t streams for many I/O operations.
   *  The wchar_t specialization provides a number of optimizations as well.
   *
   *  ctype<wchar_t> inherits its public methods from
   *  __ctype_abstract_base<wchar_t>.
  */
  template<>
    class ctype<wchar_t> : public __ctype_abstract_base<wchar_t>
    {
    public:
      // Types:
      /// Typedef for the template parameter wchar_t.
      typedef wchar_t 	       	char_type;
      typedef wctype_t	       	__wmask_type;

    protected:
      __c_locale		_M_c_locale_ctype;

      // Pre-computed narrowed and widened chars.
      bool                      _M_narrow_ok;
      char                      _M_narrow[128];
      wint_t                    _M_widen[1 + static_cast<unsigned char>(-1)];

      // Pre-computed elements for do_is.
      mask                      _M_bit[16];
      __wmask_type              _M_wmask[16];

    public:
      // Data Members:
      /// The facet id for ctype<wchar_t>
      static locale::id        	id;

      /**
       *  @brief  Constructor performs initialization.
       *
       *  This is the constructor provided by the standard.
       *
       *  @param refs  Passed to the base facet class.
      */
      explicit 
      ctype(size_t __refs = 0);

      /**
       *  @brief  Constructor performs static initialization.
       *
       *  This constructor is used to construct the initial C locale facet.
       *
       *  @param cloc  Handle to C locale data.
       *  @param refs  Passed to the base facet class.
      */
      explicit 
      ctype(__c_locale __cloc, size_t __refs = 0);

    protected:
      __wmask_type
      _M_convert_to_wmask(const mask __m) const;

      /// Destructor
      virtual 
      ~ctype();

      /**
       *  @brief  Test wchar_t classification.
       *
       *  This function finds a mask M for @a c and compares it to mask @a m.
       *
       *  do_is() is a hook for a derived facet to change the behavior of
       *  classifying.  do_is() must always return the same result for the
       *  same input.
       *
       *  @param c  The wchar_t to find the mask of.
       *  @param m  The mask to compare against.
       *  @return  (M & m) != 0.
      */
      virtual bool 
      do_is(mask __m, char_type __c) const;

      /**
       *  @brief  Return a mask array.
       *
       *  This function finds the mask for each wchar_t in the range [lo,hi)
       *  and successively writes it to vec.  vec must have as many elements
       *  as the input.
       *
       *  do_is() is a hook for a derived facet to change the behavior of
       *  classifying.  do_is() must always return the same result for the
       *  same input.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param vec  Pointer to an array of mask storage.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_is(const char_type* __lo, const char_type* __hi, mask* __vec) const;

      /**
       *  @brief  Find wchar_t matching mask
       *
       *  This function searches for and returns the first wchar_t c in
       *  [lo,hi) for which is(m,c) is true.
       *
       *  do_scan_is() is a hook for a derived facet to change the behavior of
       *  match searching.  do_is() must always return the same result for the
       *  same input.
       *
       *  @param m  The mask to compare against.
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  Pointer to a matching wchar_t if found, else @a hi.
      */
      virtual const char_type*
      do_scan_is(mask __m, const char_type* __lo, const char_type* __hi) const;

      /**
       *  @brief  Find wchar_t not matching mask
       *
       *  This function searches for and returns a pointer to the first
       *  wchar_t c of [lo,hi) for which is(m,c) is false.
       *
       *  do_scan_is() is a hook for a derived facet to change the behavior of
       *  match searching.  do_is() must always return the same result for the
       *  same input.
       *
       *  @param m  The mask to compare against.
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  Pointer to a non-matching wchar_t if found, else @a hi.
      */
      virtual const char_type*
      do_scan_not(mask __m, const char_type* __lo, 
		  const char_type* __hi) const;

      /**
       *  @brief  Convert to uppercase.
       *
       *  This virtual function converts the wchar_t argument to uppercase if
       *  possible.  If not possible (for example, '2'), returns the argument.
       *
       *  do_toupper() is a hook for a derived facet to change the behavior of
       *  uppercasing.  do_toupper() must always return the same result for
       *  the same input.
       *
       *  @param c  The wchar_t to convert.
       *  @return  The uppercase wchar_t if convertible, else @a c.
      */
      virtual char_type 
      do_toupper(char_type) const;

      /**
       *  @brief  Convert array to uppercase.
       *
       *  This virtual function converts each wchar_t in the range [lo,hi) to
       *  uppercase if possible.  Other elements remain untouched.
       *
       *  do_toupper() is a hook for a derived facet to change the behavior of
       *  uppercasing.  do_toupper() must always return the same result for
       *  the same input.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_toupper(char_type* __lo, const char_type* __hi) const;

      /**
       *  @brief  Convert to lowercase.
       *
       *  This virtual function converts the argument to lowercase if
       *  possible.  If not possible (for example, '2'), returns the argument.
       *
       *  do_tolower() is a hook for a derived facet to change the behavior of
       *  lowercasing.  do_tolower() must always return the same result for
       *  the same input.
       *
       *  @param c  The wchar_t to convert.
       *  @return  The lowercase wchar_t if convertible, else @a c.
      */
      virtual char_type 
      do_tolower(char_type) const;

      /**
       *  @brief  Convert array to lowercase.
       *
       *  This virtual function converts each wchar_t in the range [lo,hi) to
       *  lowercase if possible.  Other elements remain untouched.
       *
       *  do_tolower() is a hook for a derived facet to change the behavior of
       *  lowercasing.  do_tolower() must always return the same result for
       *  the same input.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_tolower(char_type* __lo, const char_type* __hi) const;
      
      /**
       *  @brief  Widen char to wchar_t
       *
       *  This virtual function converts the char to wchar_t using the
       *  simplest reasonable transformation.  For an underived ctype<wchar_t>
       *  facet, the argument will be cast to wchar_t.
       *
       *  do_widen() is a hook for a derived facet to change the behavior of
       *  widening.  do_widen() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The char to convert.
       *  @return  The converted wchar_t.
      */
      virtual char_type 
      do_widen(char) const;

      /**
       *  @brief  Widen char array to wchar_t array
       *
       *  This function converts each char in the input to wchar_t using the
       *  simplest reasonable transformation.  For an underived ctype<wchar_t>
       *  facet, the argument will be copied, casting each element to wchar_t.
       *
       *  do_widen() is a hook for a derived facet to change the behavior of
       *  widening.  do_widen() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start range.
       *  @param hi  Pointer to end of range.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      virtual const char*
      do_widen(const char* __lo, const char* __hi, char_type* __dest) const;

      /**
       *  @brief  Narrow wchar_t to char
       *
       *  This virtual function converts the argument to char using the
       *  simplest reasonable transformation.  If the conversion fails, dfault
       *  is returned instead.  For an underived ctype<wchar_t> facet, @a c will
       *  be cast to char and returned.
       *
       *  do_narrow() is a hook for a derived facet to change the behavior of
       *  narrowing.  do_narrow() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param c  The wchar_t to convert.
       *  @param dfault  Char to return if conversion fails.
       *  @return  The converted char.
      */
      virtual char 
      do_narrow(char_type, char __dfault) const;

      /**
       *  @brief  Narrow wchar_t array to char array
       *
       *  This virtual function converts each wchar_t in the range [lo,hi) to
       *  char using the simplest reasonable transformation and writes the
       *  results to the destination array.  For any wchar_t in the input that
       *  cannot be converted, @a dfault is used instead.  For an underived
       *  ctype<wchar_t> facet, the argument will be copied, casting each
       *  element to char.
       *
       *  do_narrow() is a hook for a derived facet to change the behavior of
       *  narrowing.  do_narrow() must always return the same result for the
       *  same input.
       *
       *  Note: this is not what you want for codepage conversions.  See
       *  codecvt for that.
       *
       *  @param lo  Pointer to start of range.
       *  @param hi  Pointer to end of range.
       *  @param dfault  Char to use if conversion fails.
       *  @param to  Pointer to the destination array.
       *  @return  @a hi.
      */
      virtual const char_type*
      do_narrow(const char_type* __lo, const char_type* __hi,
		char __dfault, char* __dest) const;

      // For use at construction time only.
      void 
      _M_initialize_ctype();
    };

  template<>
    const ctype<wchar_t>&
    use_facet<ctype<wchar_t> >(const locale& __loc);
#endif //_GLIBCXX_USE_WCHAR_T

  // Include host and configuration specific ctype inlines.
  #include <bits/ctype_inline.h>

  // 22.2.1.2  Template class ctype_byname
  template<typename _CharT>
    class ctype_byname : public ctype<_CharT>
    {
    public:
      typedef _CharT 		char_type;

      explicit 
      ctype_byname(const char* __s, size_t __refs = 0);

    protected:
      virtual 
      ~ctype_byname() { };
    };

  // 22.2.1.4  Class ctype_byname specializations.
  template<>
    ctype_byname<char>::ctype_byname(const char*, size_t refs);

  template<>
    ctype_byname<wchar_t>::ctype_byname(const char*, size_t refs);

  // 22.2.1.5  Template class codecvt
  #include <bits/codecvt.h>

  // 22.2.2  The numeric category.
  class __num_base 
  {
  public:
    // NB: Code depends on the order of _S_atoms_out elements.
    // Below are the indices into _S_atoms_out.
    enum 
      {  
        _S_ominus, 
        _S_oplus, 
        _S_ox, 
        _S_oX, 
        _S_odigits,
        _S_odigits_end = _S_odigits + 16,
        _S_oudigits = _S_odigits_end,  
        _S_oudigits_end = _S_oudigits + 16,
        _S_oe = _S_odigits + 14,  // For scientific notation, 'e'
        _S_oE = _S_oudigits + 14, // For scientific notation, 'E'
	_S_oend = _S_oudigits_end
      };
    
    // A list of valid numeric literals for output.  This array
    // contains chars that will be passed through the current locale's
    // ctype<_CharT>.widen() and then used to render numbers.
    // For the standard "C" locale, this is
    // "-+xX0123456789abcdef0123456789ABCDEF".
    static const char* _S_atoms_out;

    // String literal of acceptable (narrow) input, for num_get.
    // "-+xX0123456789abcdefABCDEF"
    static const char* _S_atoms_in;

    enum 
    {  
      _S_iminus, 
      _S_iplus, 
      _S_ix, 
      _S_iX, 
      _S_izero,
      _S_ie = _S_izero + 14,
      _S_iE = _S_izero + 20,
      _S_iend = 26
    };

    // num_put
    // Construct and return valid scanf format for floating point types.
    static void
    _S_format_float(const ios_base& __io, char* __fptr, char __mod);
  };

  template<typename _CharT>
    struct __numpunct_cache : public locale::facet
    {
      const char* 			_M_grouping;
      bool				_M_use_grouping;
      const _CharT* 			_M_truename;
      const _CharT*			_M_falsename;
      _CharT 				_M_decimal_point;
      _CharT 				_M_thousands_sep;
      
      // A list of valid numeric literals for output: in the standard
      // "C" locale, this is "-+xX0123456789abcdef0123456789ABCDEF".
      // This array contains the chars after having been passed
      // through the current locale's ctype<_CharT>.widen().
      _CharT                    	_M_atoms_out[__num_base::_S_oend + 1];

      // A list of valid numeric literals for input: in the standard
      // "C" locale, this is "-+xX0123456789abcdefABCDEF"
      // This array contains the chars after having been passed
      // through the current locale's ctype<_CharT>.widen().
      _CharT                    	_M_atoms_in[__num_base::_S_iend + 1];

      bool				_M_allocated;

      __numpunct_cache(size_t __refs = 0) : facet(__refs), 
      _M_grouping(NULL), _M_use_grouping(false), _M_truename(NULL), 
      _M_falsename(NULL), _M_decimal_point(_CharT()), 
      _M_thousands_sep(_CharT()), _M_allocated(false)
      { } 

      ~__numpunct_cache();

      void
      _M_cache(const locale& __loc);
    };

  template<typename _CharT>
    void
    __numpunct_cache<_CharT>::_M_cache(const locale& __loc)
    {
      _M_allocated = true;

      const numpunct<_CharT>& __np = use_facet<numpunct<_CharT> >(__loc);

      const string::size_type __len = __np.grouping().size();
      char* __grouping = new char[__len + 1];
      __np.grouping().copy(__grouping, __len);
      __grouping[__len] = char();
      _M_grouping = __grouping;
      _M_use_grouping = __len && __np.grouping()[0] != 0;

      typedef basic_string<_CharT> __string_type;
      typename __string_type::size_type __lentf = __np.truename().size();
      _CharT* __truename = new _CharT[__lentf + 1];
      __np.truename().copy(__truename, __lentf);
      __truename[__lentf] = _CharT();
      _M_truename = __truename;
      
      __lentf = __np.falsename().size();
      _CharT* __falsename = new _CharT[__lentf + 1];
      __np.falsename().copy(__falsename, __lentf);
      __falsename[__lentf] = _CharT();
      _M_falsename = __falsename;
          
      _M_decimal_point = __np.decimal_point();
      _M_thousands_sep = __np.thousands_sep();
      
      const ctype<_CharT>& __ct = use_facet<ctype<_CharT> >(__loc);
      __ct.widen(__num_base::_S_atoms_out, 
		 __num_base::_S_atoms_out + __num_base::_S_oend, _M_atoms_out);
      _M_atoms_out[__num_base::_S_oend] = _CharT();
      __ct.widen(__num_base::_S_atoms_in, 
		 __num_base::_S_atoms_in + __num_base::_S_iend, _M_atoms_in);
      _M_atoms_in[__num_base::_S_iend] = _CharT();
    }

  template<typename _CharT>
    __numpunct_cache<_CharT>::~__numpunct_cache()
    {
      if (_M_allocated)
	{
	  delete [] _M_grouping;
	  delete [] _M_truename;
	  delete [] _M_falsename;
	}
    }

  template<typename _CharT>
    class numpunct : public locale::facet
    {
    public:
      // Types:
      typedef _CharT          		char_type;
      typedef basic_string<_CharT> 	string_type;
      typedef __numpunct_cache<_CharT>  __cache_type;

    protected:
      __cache_type*			_M_data;

    public:
      static locale::id 		id;

      explicit 
      numpunct(size_t __refs = 0) : facet(__refs), _M_data(NULL)
      { _M_initialize_numpunct(); }

      explicit 
      numpunct(__cache_type* __cache, size_t __refs = 0) 
      : facet(__refs), _M_data(__cache)
      { _M_initialize_numpunct(); }

      explicit 
      numpunct(__c_locale __cloc, size_t __refs = 0) 
      : facet(__refs), _M_data(NULL)
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
      ~numpunct();

      virtual char_type    
      do_decimal_point() const
      { return _M_data->_M_decimal_point; }

      virtual char_type    
      do_thousands_sep() const
      { return _M_data->_M_thousands_sep; }

      virtual string
      do_grouping() const
      { return _M_data->_M_grouping; }

      virtual string_type  
      do_truename() const
      { return _M_data->_M_truename; }

      virtual string_type  
      do_falsename() const
      { return _M_data->_M_falsename; }

      // For use at construction time only.
      void 
      _M_initialize_numpunct(__c_locale __cloc = NULL);
    };

  template<typename _CharT>
    locale::id numpunct<_CharT>::id;

  template<> 
    numpunct<char>::~numpunct();

  template<> 
    void
    numpunct<char>::_M_initialize_numpunct(__c_locale __cloc);

#ifdef _GLIBCXX_USE_WCHAR_T
  template<> 
    numpunct<wchar_t>::~numpunct();

  template<> 
    void
    numpunct<wchar_t>::_M_initialize_numpunct(__c_locale __cloc);
#endif

  template<typename _CharT>
    class numpunct_byname : public numpunct<_CharT>
    {
    public:
      typedef _CharT               	char_type;
      typedef basic_string<_CharT> 	string_type;

      explicit 
      numpunct_byname(const char* __s, size_t __refs = 0)
      : numpunct<_CharT>(__refs)
      {
	if (std::strcmp(__s, "C") != 0 && std::strcmp(__s, "POSIX") != 0)
	  {
	    __c_locale __tmp;
	    this->_S_create_c_locale(__tmp, __s);
	    this->_M_initialize_numpunct(__tmp);	
	    this->_S_destroy_c_locale(__tmp); 
	  }
      }

    protected:
      virtual 
      ~numpunct_byname() { }	
    };

  template<typename _CharT, typename _InIter>
    class num_get : public locale::facet, public __num_base
    {
    public:
      // Types:
      typedef _CharT   			char_type;
      typedef _InIter  			iter_type;

      static locale::id 		id;

      explicit 
      num_get(size_t __refs = 0) : facet(__refs) { }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, bool& __v) const
      { return this->do_get(__in, __end, __io, __err, __v); }

      iter_type
      get(iter_type __in, iter_type __end, ios_base& __io, 
	  ios_base::iostate& __err, long& __v) const
      { return this->do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, unsigned short& __v) const
      { return this->do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, unsigned int& __v)   const
      { return this->do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, unsigned long& __v)  const
      { return this->do_get(__in, __end, __io, __err, __v); }

#ifdef _GLIBCXX_USE_LONG_LONG
      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, long long& __v) const
      { return this->do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, unsigned long long& __v)  const
      { return this->do_get(__in, __end, __io, __err, __v); }
#endif

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, float& __v) const
      { return this->do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, double& __v) const
      { return this->do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, long double& __v) const
      { return this->do_get(__in, __end, __io, __err, __v); }

      iter_type 
      get(iter_type __in, iter_type __end, ios_base& __io,
	  ios_base::iostate& __err, void*& __v) const
      { return this->do_get(__in, __end, __io, __err, __v); }      

    protected:
      virtual ~num_get() { }

      iter_type 
      _M_extract_float(iter_type, iter_type, ios_base&, ios_base::iostate&, 
		       string& __xtrc) const;

      template<typename _ValueT>
        iter_type 
        _M_extract_int(iter_type, iter_type, ios_base&, ios_base::iostate&, 
		       _ValueT& __v) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, bool&) const;


      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, long&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	      unsigned short&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     unsigned int&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     unsigned long&) const;

#ifdef _GLIBCXX_USE_LONG_LONG 
      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     long long&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     unsigned long long&) const;
#endif

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     float&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     double&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     long double&) const;

      virtual iter_type 
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	     void*&) const;
    };

  template<typename _CharT, typename _InIter>
    locale::id num_get<_CharT, _InIter>::id;


  template<typename _CharT, typename _OutIter>
    class num_put : public locale::facet, public __num_base
    {
    public:
      // Types:
      typedef _CharT       	char_type;
      typedef _OutIter     	iter_type;
      static locale::id		id;

      explicit 
      num_put(size_t __refs = 0) : facet(__refs) { }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, bool __v) const
      { return this->do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, long __v) const
      { return this->do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, 
	  unsigned long __v) const
      { return this->do_put(__s, __f, __fill, __v); }

#ifdef _GLIBCXX_USE_LONG_LONG 
      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, long long __v) const
      { return this->do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, 
	  unsigned long long __v) const
      { return this->do_put(__s, __f, __fill, __v); }
#endif

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, double __v) const
      { return this->do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, 
	  long double __v) const
      { return this->do_put(__s, __f, __fill, __v); }

      iter_type 
      put(iter_type __s, ios_base& __f, char_type __fill, 
	  const void* __v) const
      { return this->do_put(__s, __f, __fill, __v); }

    protected:
      template<typename _ValueT>
        iter_type
        _M_insert_float(iter_type, ios_base& __io, char_type __fill, 
			char __mod, _ValueT __v) const;

      void
      _M_group_float(const string& __grouping, char_type __sep, 
		     const char_type* __p, char_type* __new, char_type* __cs,
		     int& __len) const;

      template<typename _ValueT>
        iter_type
        _M_insert_int(iter_type, ios_base& __io, char_type __fill, 
		      _ValueT __v) const;

      void
      _M_group_int(const string& __grouping, char_type __sep, 
		   ios_base& __io, char_type* __new, char_type* __cs, 
		   int& __len) const;

      void
      _M_pad(char_type __fill, streamsize __w, ios_base& __io, 
	     char_type* __new, const char_type* __cs, int& __len) const;

      virtual 
      ~num_put() { };

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, bool __v) const;

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, long __v) const;

      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, unsigned long) const;

#ifdef _GLIBCXX_USE_LONG_LONG 
      virtual iter_type 
      do_put(iter_type, ios_base&, char_type __fill, long long __v) const;

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
      static locale::id 		id;

      explicit 
      collate(size_t __refs = 0)
      : facet(__refs)
      { _M_c_locale_collate = _S_get_c_locale(); }

      explicit 
      collate(__c_locale __cloc, size_t __refs = 0) 
      : facet(__refs)
      { _M_c_locale_collate = _S_clone_c_locale(__cloc); }

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
      _M_compare(const _CharT*, const _CharT*) const;

      size_t
      _M_transform(_CharT*, const _CharT*, size_t) const;

  protected:
      virtual
      ~collate() 
      { _S_destroy_c_locale(_M_c_locale_collate); }

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
    collate<char>::_M_compare(const char*, const char*) const;

  template<>
    size_t
    collate<char>::_M_transform(char*, const char*, size_t) const;

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    int 
    collate<wchar_t>::_M_compare(const wchar_t*, const wchar_t*) const;

  template<>
    size_t
    collate<wchar_t>::_M_transform(wchar_t*, const wchar_t*, size_t) const;
#endif

  template<typename _CharT>
    class collate_byname : public collate<_CharT>
    {
    public:
      typedef _CharT               char_type;
      typedef basic_string<_CharT> string_type;

      explicit 
      collate_byname(const char* __s, size_t __refs = 0)
      : collate<_CharT>(__refs) 
      { 
	if (std::strcmp(__s, "C") != 0 && std::strcmp(__s, "POSIX") != 0)
	  {
	    this->_S_destroy_c_locale(this->_M_c_locale_collate);
	    this->_S_create_c_locale(this->_M_c_locale_collate, __s); 
	  }
      }

    protected:
      virtual   
      ~collate_byname() { }
    };


  class time_base
  {
  public:
    enum dateorder { no_order, dmy, mdy, ymd, ydm };
  };

  template<typename _CharT>
    struct __timepunct_cache : public locale::facet
    {
      // List of all known timezones, with GMT first.
      static const _CharT* 		_S_timezones[14];

      const _CharT* 			_M_date_format;
      const _CharT* 			_M_date_era_format;
      const _CharT* 			_M_time_format;
      const _CharT* 			_M_time_era_format;
      const _CharT*			_M_date_time_format;
      const _CharT*			_M_date_time_era_format;
      const _CharT* 			_M_am;
      const _CharT* 			_M_pm;
      const _CharT*			_M_am_pm_format;

      // Day names, starting with "C"'s Sunday.
      const _CharT*  			_M_day1;
      const _CharT*  			_M_day2;
      const _CharT*  			_M_day3;
      const _CharT*  			_M_day4;
      const _CharT*  			_M_day5;
      const _CharT*  			_M_day6;
      const _CharT*  			_M_day7;

      // Abbreviated day names, starting with "C"'s Sun.
      const _CharT*  			_M_aday1;
      const _CharT*  			_M_aday2;
      const _CharT*  			_M_aday3;
      const _CharT*  			_M_aday4;
      const _CharT*  			_M_aday5;
      const _CharT*  			_M_aday6;
      const _CharT*  			_M_aday7;

      // Month names, starting with "C"'s January.
      const _CharT*  			_M_month01;
      const _CharT*  			_M_month02;
      const _CharT*  			_M_month03;
      const _CharT*  			_M_month04;
      const _CharT*  			_M_month05;
      const _CharT*  			_M_month06;
      const _CharT*  			_M_month07;
      const _CharT*  			_M_month08;
      const _CharT*  			_M_month09;
      const _CharT*  			_M_month10;
      const _CharT*  			_M_month11;
      const _CharT*  			_M_month12;

      // Abbreviated month names, starting with "C"'s Jan.
      const _CharT*  			_M_amonth01;
      const _CharT*  			_M_amonth02;
      const _CharT*  			_M_amonth03;
      const _CharT*  			_M_amonth04;
      const _CharT*  			_M_amonth05;
      const _CharT*  			_M_amonth06;
      const _CharT*  			_M_amonth07;
      const _CharT*  			_M_amonth08;
      const _CharT*  			_M_amonth09;
      const _CharT*  			_M_amonth10;
      const _CharT*  			_M_amonth11;
      const _CharT*  			_M_amonth12;

      bool				_M_allocated;

      __timepunct_cache(size_t __refs = 0) : facet(__refs), 
      _M_date_format(NULL), _M_date_era_format(NULL), _M_time_format(NULL),
      _M_time_era_format(NULL), _M_date_time_format(NULL), 
      _M_date_time_era_format(NULL), _M_am(NULL), _M_pm(NULL), 
      _M_am_pm_format(NULL), _M_day1(NULL), _M_day2(NULL), _M_day3(NULL), 
      _M_day4(NULL), _M_day5(NULL), _M_day6(NULL), _M_day7(NULL), 
      _M_aday1(NULL), _M_aday2(NULL), _M_aday3(NULL), _M_aday4(NULL), 
      _M_aday5(NULL), _M_aday6(NULL), _M_aday7(NULL), _M_month01(NULL),
      _M_month02(NULL), _M_month03(NULL), _M_month04(NULL), _M_month05(NULL), 
      _M_month06(NULL), _M_month07(NULL), _M_month08(NULL), _M_month09(NULL), 
      _M_month10(NULL), _M_month11(NULL), _M_month12(NULL), _M_amonth01(NULL),
      _M_amonth02(NULL), _M_amonth03(NULL), _M_amonth04(NULL), 
      _M_amonth05(NULL), _M_amonth06(NULL), _M_amonth07(NULL), 
      _M_amonth08(NULL), _M_amonth09(NULL), _M_amonth10(NULL), 
      _M_amonth11(NULL), _M_amonth12(NULL), _M_allocated(false)
      { } 

      ~__timepunct_cache();

      void
      _M_cache(const locale& __loc);
    };

  template<typename _CharT>
    __timepunct_cache<_CharT>::~__timepunct_cache()
    {
      if (_M_allocated)
	{
	  // XXX.
	}
    }

  // Specializations.
  template<> 
    const char*
    __timepunct_cache<char>::_S_timezones[14];

#ifdef _GLIBCXX_USE_WCHAR_T
  template<> 
    const wchar_t*
    __timepunct_cache<wchar_t>::_S_timezones[14];
#endif

  // Generic.
  template<typename _CharT>
    const _CharT* __timepunct_cache<_CharT>::_S_timezones[14];

  template<typename _CharT>
    class __timepunct : public locale::facet
    {
    public:
      // Types:
      typedef _CharT          		__char_type;
      typedef basic_string<_CharT> 	__string_type;
      typedef __timepunct_cache<_CharT>	__cache_type;

    protected:
      __cache_type*			_M_data;
      __c_locale			_M_c_locale_timepunct;
      const char*			_M_name_timepunct;

    public:
      static locale::id 		id;

      explicit 
      __timepunct(size_t __refs = 0);

      explicit 
      __timepunct(__cache_type* __cache, size_t __refs = 0);

      explicit 
      __timepunct(__c_locale __cloc, const char* __s, size_t __refs = 0);

      void
      _M_put(_CharT* __s, size_t __maxlen, const _CharT* __format, 
	     const tm* __tm) const;

      void
      _M_date_formats(const _CharT** __date) const
      {
	// Always have default first.
	__date[0] = _M_data->_M_date_format;
	__date[1] = _M_data->_M_date_era_format;	
      }

      void
      _M_time_formats(const _CharT** __time) const
      {
	// Always have default first.
	__time[0] = _M_data->_M_time_format;
	__time[1] = _M_data->_M_time_era_format;	
      }

      void
      _M_ampm(const _CharT** __ampm) const
      { 
	__ampm[0] = _M_data->_M_am;
	__ampm[1] = _M_data->_M_pm;
      }      

      void
      _M_date_time_formats(const _CharT** __dt) const
      {
	// Always have default first.
	__dt[0] = _M_data->_M_date_time_format;
	__dt[1] = _M_data->_M_date_time_era_format;	
      }

      void
      _M_days(const _CharT** __days) const
      { 
	__days[0] = _M_data->_M_day1;
	__days[1] = _M_data->_M_day2;
	__days[2] = _M_data->_M_day3;
	__days[3] = _M_data->_M_day4;
	__days[4] = _M_data->_M_day5;
	__days[5] = _M_data->_M_day6;
	__days[6] = _M_data->_M_day7;
      }

      void
      _M_days_abbreviated(const _CharT** __days) const
      { 
	__days[0] = _M_data->_M_aday1;
	__days[1] = _M_data->_M_aday2;
	__days[2] = _M_data->_M_aday3;
	__days[3] = _M_data->_M_aday4;
	__days[4] = _M_data->_M_aday5;
	__days[5] = _M_data->_M_aday6;
	__days[6] = _M_data->_M_aday7;
      }

      void
      _M_months(const _CharT** __months) const
      { 
	__months[0] = _M_data->_M_month01;
	__months[1] = _M_data->_M_month02;
	__months[2] = _M_data->_M_month03;
	__months[3] = _M_data->_M_month04;
	__months[4] = _M_data->_M_month05;
	__months[5] = _M_data->_M_month06;
	__months[6] = _M_data->_M_month07;
	__months[7] = _M_data->_M_month08;
	__months[8] = _M_data->_M_month09;
	__months[9] = _M_data->_M_month10;
	__months[10] = _M_data->_M_month11;
	__months[11] = _M_data->_M_month12;
      }

      void
      _M_months_abbreviated(const _CharT** __months) const
      { 
	__months[0] = _M_data->_M_amonth01;
	__months[1] = _M_data->_M_amonth02;
	__months[2] = _M_data->_M_amonth03;
	__months[3] = _M_data->_M_amonth04;
	__months[4] = _M_data->_M_amonth05;
	__months[5] = _M_data->_M_amonth06;
	__months[6] = _M_data->_M_amonth07;
	__months[7] = _M_data->_M_amonth08;
	__months[8] = _M_data->_M_amonth09;
	__months[9] = _M_data->_M_amonth10;
	__months[10] = _M_data->_M_amonth11;
	__months[11] = _M_data->_M_amonth12;
      }

    protected:
      virtual 
      ~__timepunct();

      // For use at construction time only.
      void 
      _M_initialize_timepunct(__c_locale __cloc = NULL);
    };

  template<typename _CharT>
    locale::id __timepunct<_CharT>::id;

  // Specializations.
  template<> 
    void
    __timepunct<char>::_M_initialize_timepunct(__c_locale __cloc);

  template<>
    void
    __timepunct<char>::_M_put(char*, size_t, const char*, const tm*) const;

#ifdef _GLIBCXX_USE_WCHAR_T
  template<> 
    void
    __timepunct<wchar_t>::_M_initialize_timepunct(__c_locale __cloc);

  template<>
    void
    __timepunct<wchar_t>::_M_put(wchar_t*, size_t, const wchar_t*, 
				 const tm*) const;
#endif

  // Include host and configuration specific timepunct functions.
  #include <bits/time_members.h>

  template<typename _CharT, typename _InIter>
    class time_get : public locale::facet, public time_base
    {
    public:
      // Types:
      typedef _CharT     		char_type;
      typedef _InIter    		iter_type;
      typedef basic_string<_CharT> 	__string_type;

      static locale::id 		id;

      explicit 
      time_get(size_t __refs = 0) 
      : facet (__refs) { }

      dateorder 
      date_order()  const
      { return this->do_date_order(); }

      iter_type 
      get_time(iter_type __beg, iter_type __end, ios_base& __io, 
	       ios_base::iostate& __err, tm* __tm)  const
      { return this->do_get_time(__beg, __end, __io, __err, __tm); }

      iter_type 
      get_date(iter_type __beg, iter_type __end, ios_base& __io,
	       ios_base::iostate& __err, tm* __tm)  const
      { return this->do_get_date(__beg, __end, __io, __err, __tm); }

      iter_type 
      get_weekday(iter_type __beg, iter_type __end, ios_base& __io,
		  ios_base::iostate& __err, tm* __tm) const
      { return this->do_get_weekday(__beg, __end, __io, __err, __tm); }

      iter_type 
      get_monthname(iter_type __beg, iter_type __end, ios_base& __io, 
		    ios_base::iostate& __err, tm* __tm) const
      { return this->do_get_monthname(__beg, __end, __io, __err, __tm); }

      iter_type 
      get_year(iter_type __beg, iter_type __end, ios_base& __io,
	       ios_base::iostate& __err, tm* __tm) const
      { return this->do_get_year(__beg, __end, __io, __err, __tm); }

    protected:
      virtual 
      ~time_get() { }

      virtual dateorder 
      do_date_order() const;

      virtual iter_type 
      do_get_time(iter_type __beg, iter_type __end, ios_base& __io,
		  ios_base::iostate& __err, tm* __tm) const;

      virtual iter_type 
      do_get_date(iter_type __beg, iter_type __end, ios_base& __io,
		  ios_base::iostate& __err, tm* __tm) const;

      virtual iter_type 
      do_get_weekday(iter_type __beg, iter_type __end, ios_base&,
		     ios_base::iostate& __err, tm* __tm) const;

      virtual iter_type 
      do_get_monthname(iter_type __beg, iter_type __end, ios_base&, 
		       ios_base::iostate& __err, tm* __tm) const;

      virtual iter_type 
      do_get_year(iter_type __beg, iter_type __end, ios_base& __io,
		  ios_base::iostate& __err, tm* __tm) const;

      // Extract numeric component of length __len.
      void
      _M_extract_num(iter_type& __beg, iter_type& __end, int& __member,
		     int __min, int __max, size_t __len,
		     const ctype<_CharT>& __ctype, 
		     ios_base::iostate& __err) const;
      
      // Extract day or month name, or any unique array of string
      // literals in a const _CharT* array.
      void
      _M_extract_name(iter_type& __beg, iter_type& __end, int& __member,
		      const _CharT** __names, size_t __indexlen, 
		      const ctype<_CharT>& __ctype, 
		      ios_base::iostate& __err) const;

      // Extract on a component-by-component basis, via __format argument.
      void
      _M_extract_via_format(iter_type& __beg, iter_type& __end, ios_base& __io,
			    ios_base::iostate& __err, tm* __tm, 
			    const _CharT* __format) const;
    };

  template<typename _CharT, typename _InIter>
    locale::id time_get<_CharT, _InIter>::id;

  template<typename _CharT, typename _InIter>
    class time_get_byname : public time_get<_CharT, _InIter>
    {
    public:
      // Types:
      typedef _CharT     		char_type;
      typedef _InIter    		iter_type;

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
      // Types:
      typedef _CharT     		char_type;
      typedef _OutIter   		iter_type;

      static locale::id 	     	id;

      explicit 
      time_put(size_t __refs = 0) 
      : facet(__refs) { }

      iter_type 
      put(iter_type __s, ios_base& __io, char_type __fill, const tm* __tm, 
	  const _CharT* __beg, const _CharT* __end) const;

      iter_type 
      put(iter_type __s, ios_base& __io, char_type __fill,
	  const tm* __tm, char __format, char __mod = 0) const
      { return this->do_put(__s, __io, __fill, __tm, __format, __mod); }

    protected:
      virtual 
      ~time_put()
      { }

      virtual iter_type 
      do_put(iter_type __s, ios_base& __io, char_type __fill, const tm* __tm, 
	     char __format, char __mod) const;
    };

  template<typename _CharT, typename _OutIter>
    locale::id time_put<_CharT, _OutIter>::id;

  template<typename _CharT, typename _OutIter>
    class time_put_byname : public time_put<_CharT, _OutIter>
    {
    public:
      // Types:
      typedef _CharT     		char_type;
      typedef _OutIter   		iter_type;

      explicit 
      time_put_byname(const char*, size_t __refs = 0) 
      : time_put<_CharT, _OutIter>(__refs) 
      { };

    protected:
      virtual 
      ~time_put_byname() { }
    };


  class money_base
  {
  public:
    enum part { none, space, symbol, sign, value };
    struct pattern { char field[4]; };

    static const pattern _S_default_pattern;

    // Construct and return valid pattern consisting of some combination of:
    // space none symbol sign value
    static pattern 
    _S_construct_pattern(char __precedes, char __space, char __posn);
  };

  template<typename _CharT>
    struct __moneypunct_cache : public locale::facet
    {
      const char* 			_M_grouping;
      bool				_M_use_grouping;
      _CharT 				_M_decimal_point;
      _CharT 				_M_thousands_sep;
      const _CharT* 			_M_curr_symbol;
      const _CharT*			_M_positive_sign;
      const _CharT*			_M_negative_sign;
      int 				_M_frac_digits;
      money_base::pattern 		_M_pos_format;
      money_base::pattern 	        _M_neg_format;

      bool				_M_allocated;

      __moneypunct_cache(size_t __refs = 0) : facet(__refs), 
      _M_grouping(NULL), _M_use_grouping(false), _M_decimal_point(_CharT()), 
      _M_thousands_sep(_CharT()), _M_curr_symbol(NULL), _M_positive_sign(NULL),
      _M_negative_sign(NULL), _M_frac_digits(0), 
      _M_pos_format(money_base::pattern()), 
      _M_neg_format(money_base::pattern()), _M_allocated(false)
      { } 

      ~__moneypunct_cache();

      void
      _M_cache(const locale& __loc);
    };

  template<typename _CharT>
    __moneypunct_cache<_CharT>::~__moneypunct_cache()
    {
      if (_M_allocated)
	{
	  // XXX.
	}
    }

  template<typename _CharT, bool _Intl>
    class moneypunct : public locale::facet, public money_base
    {
    public:
      // Types:
      typedef _CharT 		       	char_type;
      typedef basic_string<_CharT> 	string_type;
      typedef __moneypunct_cache<_CharT>	__cache_type;

    private:
      __cache_type*			_M_data;

    public:
      static const bool 		intl = _Intl;
      static locale::id 		id;

      explicit 
      moneypunct(size_t __refs = 0) : facet(__refs), _M_data(NULL)
      { _M_initialize_moneypunct(); }

      explicit 
      moneypunct(__cache_type* __cache, size_t __refs = 0) 
      : facet(__refs), _M_data(__cache)
      { _M_initialize_moneypunct(); }

      explicit 
      moneypunct(__c_locale __cloc, const char* __s, size_t __refs = 0) 
      : facet(__refs), _M_data(NULL)
      { _M_initialize_moneypunct(__cloc, __s); }

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
      ~moneypunct();

      virtual char_type
      do_decimal_point() const
      { return _M_data->_M_decimal_point; }
      
      virtual char_type
      do_thousands_sep() const
      { return _M_data->_M_thousands_sep; }
      
      virtual string 
      do_grouping() const
      { return _M_data->_M_grouping; }

      virtual string_type  
      do_curr_symbol()   const
      { return _M_data->_M_curr_symbol; }

      virtual string_type  
      do_positive_sign() const
      { return _M_data->_M_positive_sign; }

      virtual string_type  
      do_negative_sign() const
      { return _M_data->_M_negative_sign; }

      virtual int          
      do_frac_digits() const
      { return _M_data->_M_frac_digits; }

      virtual pattern      
      do_pos_format() const
      { return _M_data->_M_pos_format; }

      virtual pattern      
      do_neg_format() const
      { return _M_data->_M_neg_format; }

      // For use at construction time only.
       void 
       _M_initialize_moneypunct(__c_locale __cloc = NULL, 
				const char* __name = NULL);
    };

  template<typename _CharT, bool _Intl>
    locale::id moneypunct<_CharT, _Intl>::id;

  template<typename _CharT, bool _Intl>
    const bool moneypunct<_CharT, _Intl>::intl;

  template<>
    moneypunct<char, true>::~moneypunct();

  template<>
    moneypunct<char, false>::~moneypunct();

  template<> 
    void
    moneypunct<char, true>::_M_initialize_moneypunct(__c_locale, const char*);

  template<> 
    void
    moneypunct<char, false>::_M_initialize_moneypunct(__c_locale, const char*);

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    moneypunct<wchar_t, true>::~moneypunct();

  template<>
    moneypunct<wchar_t, false>::~moneypunct();

  template<> 
    void
    moneypunct<wchar_t, true>::_M_initialize_moneypunct(__c_locale, 
							const char*);

  template<> 
    void
    moneypunct<wchar_t, false>::_M_initialize_moneypunct(__c_locale, 
							 const char*);
#endif

  template<typename _CharT, bool _Intl>
    class moneypunct_byname : public moneypunct<_CharT, _Intl>
    {
    public:
      typedef _CharT 			char_type;
      typedef basic_string<_CharT> 	string_type;

      static const bool intl = _Intl;

      explicit 
      moneypunct_byname(const char* __s, size_t __refs = 0)
      : moneypunct<_CharT, _Intl>(__refs)
      {
	if (std::strcmp(__s, "C") != 0 && std::strcmp(__s, "POSIX") != 0)
	  {
	    __c_locale __tmp;
	    this->_S_create_c_locale(__tmp, __s);
	    this->_M_initialize_moneypunct(__tmp);	
	    this->_S_destroy_c_locale(__tmp); 
	  }
      }

    protected:
      virtual 
      ~moneypunct_byname() { }
    };

  template<typename _CharT, bool _Intl>
    const bool moneypunct_byname<_CharT, _Intl>::intl;

  template<typename _CharT, typename _InIter>
    class money_get : public locale::facet
    {
    public:
      // Types:
      typedef _CharT        		char_type;
      typedef _InIter       		iter_type;
      typedef basic_string<_CharT> 	string_type;

      static locale::id 		id;

      explicit 
      money_get(size_t __refs = 0) : facet(__refs) { }

      iter_type 
      get(iter_type __s, iter_type __end, bool __intl, ios_base& __io, 
	  ios_base::iostate& __err, long double& __units) const
      { return this->do_get(__s, __end, __intl, __io, __err, __units); }

      iter_type 
      get(iter_type __s, iter_type __end, bool __intl, ios_base& __io, 
	  ios_base::iostate& __err, string_type& __digits) const
      { return this->do_get(__s, __end, __intl, __io, __err, __digits); }

    protected:
      virtual 
      ~money_get() { }

      virtual iter_type 
      do_get(iter_type __s, iter_type __end, bool __intl, ios_base& __io, 
	     ios_base::iostate& __err, long double& __units) const;

      virtual iter_type 
      do_get(iter_type __s, iter_type __end, bool __intl, ios_base& __io, 
	     ios_base::iostate& __err, string_type& __digits) const;
    };

  template<typename _CharT, typename _InIter>
    locale::id money_get<_CharT, _InIter>::id;

  template<typename _CharT, typename _OutIter>
    class money_put : public locale::facet
    {
    public:
      typedef _CharT              	char_type;
      typedef _OutIter            	iter_type;
      typedef basic_string<_CharT>	string_type;

      static locale::id 		id;

      explicit 
      money_put(size_t __refs = 0) : facet(__refs) { }

      iter_type 
      put(iter_type __s, bool __intl, ios_base& __io,
	  char_type __fill, long double __units) const
      { return this->do_put(__s, __intl, __io, __fill, __units); }

      iter_type 
      put(iter_type __s, bool __intl, ios_base& __io,
	  char_type __fill, const string_type& __digits) const
      { return this->do_put(__s, __intl, __io, __fill, __digits); }

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
      const char*			_M_name_messages;

    public:
      static locale::id 		id;

      explicit 
      messages(size_t __refs = 0);

      // Non-standard.
      explicit 
      messages(__c_locale __cloc, const char* __s, size_t __refs = 0);

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

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    wstring
    messages<wchar_t>::do_get(catalog, int, int, const wstring&) const;
#endif

  template<typename _CharT>
    class messages_byname : public messages<_CharT>
    {
    public:
      typedef _CharT               	char_type;
      typedef basic_string<_CharT> 	string_type;

      explicit 
      messages_byname(const char* __s, size_t __refs = 0);

    protected:
      virtual 
      ~messages_byname() 
      { }
    };

  // Include host and configuration specific messages functions.
  #include <bits/messages_members.h>


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
