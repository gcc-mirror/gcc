// <bitset> -*- C++ -*-

// Copyright (C) 2001 Free Software Foundation, Inc.
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

/*
 * Copyright (c) 1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */ 

#ifndef __SGI_STL_BITSET
#define __SGI_STL_BITSET

#pragma GCC system_header

// A bitset of size N has N % (sizeof(unsigned long) * CHAR_BIT) unused 
// bits.  (They are the high- order bits in the highest word.)  It is
// a class invariant of class bitset<> that those unused bits are
// always zero.

// Most of the actual code isn't contained in bitset<> itself, but in the 
// base class _Base_bitset.  The base class works with whole words, not with
// individual bits.  This allows us to specialize _Base_bitset for the
// important special case where the bitset is only a single word.

// The C++ standard does not define the precise semantics of operator[].
// In this implementation the const version of operator[] is equivalent
// to test(), except that it does no range checking.  The non-const version
// returns a reference to a bit, again without doing any range checking.


#include <bits/std_cstddef.h>     // for size_t
#include <bits/std_cstring.h>     // for memset
#include <bits/std_string.h>
#include <bits/std_stdexcept.h>   // for invalid_argument, out_of_range, 
				  // overflow_error

#include <bits/std_ostream.h>     // for ostream (operator<<)
#include <bits/std_istream.h>     // for istream (operator>>)

#define _GLIBCPP_BITSET_BITS_PER_WORD (CHAR_BIT*sizeof(unsigned long))
#define __BITSET_WORDS(__n) \
 ((__n) < 1 ? 1 : ((__n) + _GLIBCPP_BITSET_BITS_PER_WORD - 1)/_GLIBCPP_BITSET_BITS_PER_WORD)

namespace std
{

// structure to aid in counting bits
template<bool __dummy> 
struct _Bit_count {
  static unsigned char _S_bit_count[256];
};

// Mapping from 8 bit unsigned integers to the index of the first one
// bit:
template<bool __dummy> 
struct _First_one {
  static unsigned char _S_first_one[256];
};

//
// Base class: general case.
//

template<size_t _Nw>
struct _Base_bitset {
  typedef unsigned long _WordT;

  _WordT _M_w[_Nw];                // 0 is the least significant word.

  _Base_bitset( void ) { _M_do_reset(); }
  _Base_bitset(unsigned long __val) {
    _M_do_reset();
    _M_w[0] = __val;
  }

  static size_t _S_whichword( size_t __pos )
    { return __pos / _GLIBCPP_BITSET_BITS_PER_WORD; }
  static size_t _S_whichbyte( size_t __pos )
    { return (__pos % _GLIBCPP_BITSET_BITS_PER_WORD) / CHAR_BIT; }
  static size_t _S_whichbit( size_t __pos )
    { return __pos % _GLIBCPP_BITSET_BITS_PER_WORD; }
  static _WordT _S_maskbit( size_t __pos )
    { return (static_cast<_WordT>(1)) << _S_whichbit(__pos); }

  _WordT& _M_getword(size_t __pos)       { return _M_w[_S_whichword(__pos)]; }
  _WordT  _M_getword(size_t __pos) const { return _M_w[_S_whichword(__pos)]; }

  _WordT& _M_hiword()       { return _M_w[_Nw - 1]; }
  _WordT  _M_hiword() const { return _M_w[_Nw - 1]; }

  void _M_do_and(const _Base_bitset<_Nw>& __x) {
    for ( size_t __i = 0; __i < _Nw; __i++ ) {
      _M_w[__i] &= __x._M_w[__i];
    }
  }

  void _M_do_or(const _Base_bitset<_Nw>& __x) {
    for ( size_t __i = 0; __i < _Nw; __i++ ) {
      _M_w[__i] |= __x._M_w[__i];
    }
  }

  void _M_do_xor(const _Base_bitset<_Nw>& __x) {
    for ( size_t __i = 0; __i < _Nw; __i++ ) {
      _M_w[__i] ^= __x._M_w[__i];
    }
  }

  void _M_do_left_shift(size_t __shift);
  void _M_do_right_shift(size_t __shift);

  void _M_do_flip() {
    for ( size_t __i = 0; __i < _Nw; __i++ ) {
      _M_w[__i] = ~_M_w[__i];
    }
  }

  void _M_do_set() {
    for ( size_t __i = 0; __i < _Nw; __i++ ) {
      _M_w[__i] = ~static_cast<_WordT>(0);
    }
  }

  void _M_do_reset() { memset(_M_w, 0, _Nw * sizeof(_WordT)); }

  bool _M_is_equal(const _Base_bitset<_Nw>& __x) const {
    for (size_t __i = 0; __i < _Nw; ++__i) {
      if (_M_w[__i] != __x._M_w[__i])
        return false;
    }
    return true;
  }

  bool _M_is_any() const {
    for ( size_t __i = 0; __i < _Nw; __i++ ) {
      if ( _M_w[__i] != static_cast<_WordT>(0) )
        return true;
    }
    return false;
  }

  size_t _M_do_count() const {
    size_t __result = 0;
    const unsigned char* __byte_ptr = (const unsigned char*)_M_w;
    const unsigned char* __end_ptr = (const unsigned char*)(_M_w+_Nw);

    while ( __byte_ptr < __end_ptr ) {
      __result += _Bit_count<true>::_S_bit_count[*__byte_ptr];
      __byte_ptr++;
    }
    return __result;
  }

  unsigned long _M_do_to_ulong() const; 

  // find first "on" bit
  size_t _M_do_find_first(size_t __not_found) const;

  // find the next "on" bit that follows "prev"
  size_t _M_do_find_next(size_t __prev, size_t __not_found) const;
};

//
// Definitions of non-inline functions from _Base_bitset.
// 

template<size_t _Nw>
void _Base_bitset<_Nw>::_M_do_left_shift(size_t __shift) 
{
  if (__shift != 0) {
    const size_t __wshift = __shift / _GLIBCPP_BITSET_BITS_PER_WORD;
    const size_t __offset = __shift % _GLIBCPP_BITSET_BITS_PER_WORD;

    if (__offset == 0)
      for (size_t __n = _Nw - 1; __n >= __wshift; --__n)
        _M_w[__n] = _M_w[__n - __wshift];

    else {
      const size_t __sub_offset = _GLIBCPP_BITSET_BITS_PER_WORD - __offset;
      for (size_t __n = _Nw - 1; __n > __wshift; --__n)
        _M_w[__n] = (_M_w[__n - __wshift] << __offset) | 
                    (_M_w[__n - __wshift - 1] >> __sub_offset);
      _M_w[__wshift] = _M_w[0] << __offset;
    }

    fill(_M_w + 0, _M_w + __wshift, static_cast<_WordT>(0));
  }
}

template<size_t _Nw>
void _Base_bitset<_Nw>::_M_do_right_shift(size_t __shift) 
{
  if (__shift != 0) {
    const size_t __wshift = __shift / _GLIBCPP_BITSET_BITS_PER_WORD;
    const size_t __offset = __shift % _GLIBCPP_BITSET_BITS_PER_WORD;
    const size_t __limit = _Nw - __wshift - 1;

    if (__offset == 0)
      for (size_t __n = 0; __n <= __limit; ++__n)
        _M_w[__n] = _M_w[__n + __wshift];

    else {
      const size_t __sub_offset = _GLIBCPP_BITSET_BITS_PER_WORD - __offset;
      for (size_t __n = 0; __n < __limit; ++__n)
        _M_w[__n] = (_M_w[__n + __wshift] >> __offset) |
                    (_M_w[__n + __wshift + 1] << __sub_offset);
      _M_w[__limit] = _M_w[_Nw-1] >> __offset;
    }

    fill(_M_w + __limit + 1, _M_w + _Nw, static_cast<_WordT>(0));
  }
}

template<size_t _Nw>
unsigned long _Base_bitset<_Nw>::_M_do_to_ulong() const
{
  for (size_t __i = 1; __i < _Nw; ++__i) 
    if (_M_w[__i]) 
      __STL_THROW(overflow_error("bitset"));
  
  return _M_w[0];
}

template<size_t _Nw>
size_t _Base_bitset<_Nw>::_M_do_find_first(size_t __not_found) const 
{
  for ( size_t __i = 0; __i < _Nw; __i++ ) {
    _WordT __thisword = _M_w[__i];
    if ( __thisword != static_cast<_WordT>(0) ) {
      // find byte within word
      for ( size_t __j = 0; __j < sizeof(_WordT); __j++ ) {
        unsigned char __this_byte
          = static_cast<unsigned char>(__thisword & (~(unsigned char)0));
        if ( __this_byte )
          return __i*_GLIBCPP_BITSET_BITS_PER_WORD + __j*CHAR_BIT +
            _First_one<true>::_S_first_one[__this_byte];

        __thisword >>= CHAR_BIT;
      }
    }
  }
  // not found, so return an indication of failure.
  return __not_found;
}

template<size_t _Nw>
size_t
_Base_bitset<_Nw>::_M_do_find_next(size_t __prev, size_t __not_found) const
{
  // make bound inclusive
  ++__prev;

  // check out of bounds
  if ( __prev >= _Nw * _GLIBCPP_BITSET_BITS_PER_WORD )
    return __not_found;

    // search first word
  size_t __i = _S_whichword(__prev);
  _WordT __thisword = _M_w[__i];

    // mask off bits below bound
  __thisword &= (~static_cast<_WordT>(0)) << _S_whichbit(__prev);

  if ( __thisword != static_cast<_WordT>(0) ) {
    // find byte within word
    // get first byte into place
    __thisword >>= _S_whichbyte(__prev) * CHAR_BIT;
    for ( size_t __j = _S_whichbyte(__prev); __j < sizeof(_WordT); __j++ ) {
      unsigned char __this_byte
        = static_cast<unsigned char>(__thisword & (~(unsigned char)0));
      if ( __this_byte )
        return __i*_GLIBCPP_BITSET_BITS_PER_WORD + __j*CHAR_BIT +
          _First_one<true>::_S_first_one[__this_byte];

      __thisword >>= CHAR_BIT;
    }
  }

  // check subsequent words
  __i++;
  for ( ; __i < _Nw; __i++ ) {
    __thisword = _M_w[__i];
    if ( __thisword != static_cast<_WordT>(0) ) {
      // find byte within word
      for ( size_t __j = 0; __j < sizeof(_WordT); __j++ ) {
        unsigned char __this_byte
          = static_cast<unsigned char>(__thisword & (~(unsigned char)0));
        if ( __this_byte )
          return __i*_GLIBCPP_BITSET_BITS_PER_WORD + __j*CHAR_BIT +
            _First_one<true>::_S_first_one[__this_byte];

        __thisword >>= CHAR_BIT;
      }
    }
  }

  // not found, so return an indication of failure.
  return __not_found;
} // end _M_do_find_next


// ------------------------------------------------------------

//
// Base class: specialization for a single word.
//

template<> struct _Base_bitset<1> {
  typedef unsigned long _WordT;
  _WordT _M_w;

  _Base_bitset( void ) : _M_w(0) {}
  _Base_bitset(unsigned long __val) : _M_w(__val) {}

  static size_t _S_whichword( size_t __pos )
    { return __pos / _GLIBCPP_BITSET_BITS_PER_WORD; }
  static size_t _S_whichbyte( size_t __pos )
    { return (__pos % _GLIBCPP_BITSET_BITS_PER_WORD) / CHAR_BIT; }
  static size_t _S_whichbit( size_t __pos )
    {  return __pos % _GLIBCPP_BITSET_BITS_PER_WORD; }
  static _WordT _S_maskbit( size_t __pos )
    { return (static_cast<_WordT>(1)) << _S_whichbit(__pos); }

  _WordT& _M_getword(size_t)       { return _M_w; }
  _WordT  _M_getword(size_t) const { return _M_w; }

  _WordT& _M_hiword()       { return _M_w; }
  _WordT  _M_hiword() const { return _M_w; }

  void _M_do_and(const _Base_bitset<1>& __x) { _M_w &= __x._M_w; }
  void _M_do_or(const _Base_bitset<1>& __x)  { _M_w |= __x._M_w; }
  void _M_do_xor(const _Base_bitset<1>& __x) { _M_w ^= __x._M_w; }
  void _M_do_left_shift(size_t __shift)     { _M_w <<= __shift; }
  void _M_do_right_shift(size_t __shift)    { _M_w >>= __shift; }
  void _M_do_flip()                       { _M_w = ~_M_w; }
  void _M_do_set()                        { _M_w = ~static_cast<_WordT>(0); }
  void _M_do_reset()                      { _M_w = 0; }

  bool _M_is_equal(const _Base_bitset<1>& __x) const
    { return _M_w == __x._M_w; }
  bool _M_is_any() const
    { return _M_w != 0; }

  size_t _M_do_count() const {
    size_t __result = 0;
    const unsigned char* __byte_ptr = (const unsigned char*)&_M_w;
    const unsigned char* __end_ptr
      = ((const unsigned char*)&_M_w)+sizeof(_M_w);
    while ( __byte_ptr < __end_ptr ) {
      __result += _Bit_count<true>::_S_bit_count[*__byte_ptr];
      __byte_ptr++;
    }
    return __result;
  }

  unsigned long _M_do_to_ulong() const { return _M_w; }

  size_t _M_do_find_first(size_t __not_found) const;

  // find the next "on" bit that follows "prev"
  size_t _M_do_find_next(size_t __prev, size_t __not_found) const; 

};


// ------------------------------------------------------------
// Helper class to zero out the unused high-order bits in the highest word.

template <size_t _Extrabits> struct _Sanitize {
  static void _M_do_sanitize(unsigned long& __val)
    { __val &= ~((~static_cast<unsigned long>(0)) << _Extrabits); }
};

template<> struct _Sanitize<0> {
  static void _M_do_sanitize(unsigned long) {}
};



// ------------------------------------------------------------
// Class bitset.
//   _Nb may be any nonzero number of type size_t.

template<size_t _Nb>
class bitset : private _Base_bitset<__BITSET_WORDS(_Nb)>
{
private:
  typedef _Base_bitset<__BITSET_WORDS(_Nb)> _Base;
  typedef unsigned long _WordT;

private:
  void _M_do_sanitize() {
    _Sanitize<_Nb%_GLIBCPP_BITSET_BITS_PER_WORD>::_M_do_sanitize(this->_M_hiword());
  }

public:

  // bit reference:
  class reference;
  friend class reference;

  class reference {
    friend class bitset;

    _WordT *_M_wp;
    size_t _M_bpos;

    // left undefined
    reference();

  public:
    reference( bitset& __b, size_t __pos ) {
      _M_wp = &__b._M_getword(__pos);
      _M_bpos = _Base::_S_whichbit(__pos);
    }

    ~reference() {}

    // for b[i] = __x;
    reference& operator=(bool __x) {
      if ( __x )
        *_M_wp |= _Base::_S_maskbit(_M_bpos);
      else
        *_M_wp &= ~_Base::_S_maskbit(_M_bpos);

      return *this;
    }

    // for b[i] = b[__j];
    reference& operator=(const reference& __j) {
      if ( (*(__j._M_wp) & _Base::_S_maskbit(__j._M_bpos)) )
        *_M_wp |= _Base::_S_maskbit(_M_bpos);
      else
        *_M_wp &= ~_Base::_S_maskbit(_M_bpos);

      return *this;
    }

    // flips the bit
    bool operator~() const
      { return (*(_M_wp) & _Base::_S_maskbit(_M_bpos)) == 0; }

    // for __x = b[i];
    operator bool() const
      { return (*(_M_wp) & _Base::_S_maskbit(_M_bpos)) != 0; }

    // for b[i].flip();
    reference& flip() {
      *_M_wp ^= _Base::_S_maskbit(_M_bpos);
      return *this;
    }
  };

  // 23.3.5.1 constructors:
  bitset() {}
  bitset(unsigned long __val) : _Base_bitset<__BITSET_WORDS(_Nb)>(__val) 
    { _M_do_sanitize(); }

  template<class _CharT, class _Traits, class _Alloc>
  explicit bitset(const basic_string<_CharT, _Traits, _Alloc>& __s,
                  size_t __pos = 0)
    : _Base() 
  {
    if (__pos > __s.size()) 
      __STL_THROW(out_of_range("bitset"));
    _M_copy_from_string(__s, __pos,
                        basic_string<_CharT, _Traits, _Alloc>::npos);
  }
  template<class _CharT, class _Traits, class _Alloc>
  bitset(const basic_string<_CharT, _Traits, _Alloc>& __s,
         size_t __pos,
         size_t __n)
    : _Base() 
  {
    if (__pos > __s.size()) 
      __STL_THROW(out_of_range("bitset"));
    _M_copy_from_string(__s, __pos, __n);
  }

  // 23.3.5.2 bitset operations:
  bitset<_Nb>& operator&=(const bitset<_Nb>& __rhs) {
    this->_M_do_and(__rhs);
    return *this;
  }

  bitset<_Nb>& operator|=(const bitset<_Nb>& __rhs) {
    this->_M_do_or(__rhs);
    return *this;
  }

  bitset<_Nb>& operator^=(const bitset<_Nb>& __rhs) {
    this->_M_do_xor(__rhs);
    return *this;
  }

  bitset<_Nb>& operator<<=(size_t __pos) {
    this->_M_do_left_shift(__pos);
    this->_M_do_sanitize();
    return *this;
  }

  bitset<_Nb>& operator>>=(size_t __pos) {
    this->_M_do_right_shift(__pos);
    this->_M_do_sanitize();
    return *this;
  }

  //
  // Extension:
  // Versions of single-bit set, reset, flip, test with no range checking.
  //

  bitset<_Nb>& _Unchecked_set(size_t __pos) {
    this->_M_getword(__pos) |= _Base::_S_maskbit(__pos);
    return *this;
  }

  bitset<_Nb>& _Unchecked_set(size_t __pos, int __val) {
    if (__val)
      this->_M_getword(__pos) |= _Base::_S_maskbit(__pos);
    else
      this->_M_getword(__pos) &= ~_Base::_S_maskbit(__pos);

    return *this;
  }

  bitset<_Nb>& _Unchecked_reset(size_t __pos) {
    this->_M_getword(__pos) &= ~_Base::_S_maskbit(__pos);
    return *this;
  }

  bitset<_Nb>& _Unchecked_flip(size_t __pos) {
    this->_M_getword(__pos) ^= _Base::_S_maskbit(__pos);
    return *this;
  }

  bool _Unchecked_test(size_t __pos) const {
    return (this->_M_getword(__pos) & _Base::_S_maskbit(__pos))
      != static_cast<_WordT>(0);
  }

  // Set, reset, and flip.

  bitset<_Nb>& set() {
    this->_M_do_set();
    this->_M_do_sanitize();
    return *this;
  }

  bitset<_Nb>& set(size_t __pos, bool __val = true) {
    if (__pos >= _Nb)
      __STL_THROW(out_of_range("bitset"));

    return _Unchecked_set(__pos, __val);
  }

  bitset<_Nb>& reset() {
    this->_M_do_reset();
    return *this;
  }

  bitset<_Nb>& reset(size_t __pos) {
    if (__pos >= _Nb)
      __STL_THROW(out_of_range("bitset"));

    return _Unchecked_reset(__pos);
  }

  bitset<_Nb>& flip() {
    this->_M_do_flip();
    this->_M_do_sanitize();
    return *this;
  }

  bitset<_Nb>& flip(size_t __pos) {
    if (__pos >= _Nb)
      __STL_THROW(out_of_range("bitset"));

    return _Unchecked_flip(__pos);
  }

  bitset<_Nb> operator~() const { 
    return bitset<_Nb>(*this).flip();
  }

  // element access:
  //for b[i];
  reference operator[](size_t __pos) { return reference(*this,__pos); }
  bool operator[](size_t __pos) const { return _Unchecked_test(__pos); }

  unsigned long to_ulong() const { return this->_M_do_to_ulong(); }

  template <class _CharT, class _Traits, class _Alloc>
  basic_string<_CharT, _Traits, _Alloc> to_string() const {
    basic_string<_CharT, _Traits, _Alloc> __result;
    _M_copy_to_string(__result);
    return __result;
  }

  // Helper functions for string operations.
  template<class _CharT, class _Traits, class _Alloc>
  void _M_copy_from_string(const basic_string<_CharT,_Traits,_Alloc>& __s,
                          size_t,
                          size_t);

  template<class _CharT, class _Traits, class _Alloc>
  void _M_copy_to_string(basic_string<_CharT,_Traits,_Alloc>&) const;

  size_t count() const { return this->_M_do_count(); }

  size_t size() const { return _Nb; }

  bool operator==(const bitset<_Nb>& __rhs) const {
    return this->_M_is_equal(__rhs);
  }
  bool operator!=(const bitset<_Nb>& __rhs) const {
    return !this->_M_is_equal(__rhs);
  }

  bool test(size_t __pos) const {
    if (__pos >= _Nb)
      __STL_THROW(out_of_range("bitset"));

    return _Unchecked_test(__pos);
  }

  bool any() const { return this->_M_is_any(); }
  bool none() const { return !this->_M_is_any(); }

  bitset<_Nb> operator<<(size_t __pos) const
    { return bitset<_Nb>(*this) <<= __pos; }
  bitset<_Nb> operator>>(size_t __pos) const
    { return bitset<_Nb>(*this) >>= __pos; }

  //
  // EXTENSIONS: bit-find operations.  These operations are
  // experimental, and are subject to change or removal in future
  // versions.
  // 

  // find the index of the first "on" bit
  size_t _Find_first() const 
    { return this->_M_do_find_first(_Nb); }

  // find the index of the next "on" bit after prev
  size_t _Find_next( size_t __prev ) const 
    { return this->_M_do_find_next(__prev, _Nb); }

};

//
// Definitions of non-inline member functions.
//

template <size_t _Nb>
template<class _CharT, class _Traits, class _Alloc>
void bitset<_Nb>
  ::_M_copy_from_string(const basic_string<_CharT,_Traits,_Alloc>& __s,
                        size_t __pos,
                        size_t __n)
{
  reset();
  const size_t __nbits = min(_Nb, min(__n, __s.size() - __pos));
  for (size_t __i = 0; __i < __nbits; ++__i) {
    switch(__s[__pos + __nbits - __i - 1]) {
    case '0':
      break;
    case '1':
      set(__i);
      break;
    default:
      __STL_THROW(invalid_argument("bitset"));
    }
  }
}

template <size_t _Nb>
template <class _CharT, class _Traits, class _Alloc>
void bitset<_Nb>
  ::_M_copy_to_string(basic_string<_CharT, _Traits, _Alloc>& __s) const
{
  __s.assign(_Nb, '0');
  
  for (size_t __i = 0; __i < _Nb; ++__i) 
    if (_Unchecked_test(__i))
      __s[_Nb - 1 - __i] = '1';
}

// ------------------------------------------------------------

//
// 23.3.5.3 bitset operations:
//

template <size_t _Nb>
inline bitset<_Nb> operator&(const bitset<_Nb>& __x, const bitset<_Nb>& __y) {
  bitset<_Nb> __result(__x);
  __result &= __y;
  return __result;
}


template <size_t _Nb>
inline bitset<_Nb> operator|(const bitset<_Nb>& __x, const bitset<_Nb>& __y) {
  bitset<_Nb> __result(__x);
  __result |= __y;
  return __result;
}

template <size_t _Nb>
inline bitset<_Nb> operator^(const bitset<_Nb>& __x, const bitset<_Nb>& __y) {
  bitset<_Nb> __result(__x);
  __result ^= __y;
  return __result;
}

template <class _CharT, class _Traits, size_t _Nb>
basic_istream<_CharT, _Traits>&
operator>>(basic_istream<_CharT, _Traits>& __is, bitset<_Nb>& __x)
{
  typedef typename _Traits::char_type char_type;
  basic_string<_CharT, _Traits> __tmp;
  __tmp.reserve(_Nb);

  // Skip whitespace
  typename basic_istream<_CharT, _Traits>::sentry __sentry(__is);
  if (__sentry) {
    basic_streambuf<_CharT, _Traits>* __buf = __is.rdbuf();
    for (size_t __i = 0; __i < _Nb; ++__i) {
      static typename _Traits::int_type __eof = _Traits::eof();

      typename _Traits::int_type __c1 = __buf->sbumpc();
      if (_Traits::eq_int_type(__c1, __eof)) {
        __is.setstate(ios_base::eofbit);
        break;
      }
      else {
        char_type __c2 = _Traits::to_char_type(__c1);
        char_type __c  = __is.narrow(__c2, '*');

        if (__c == '0' || __c == '1')
          __tmp.push_back(__c);
        else if (_Traits::eq_int_type(__buf->sputbackc(__c2), __eof)) {
          __is.setstate(ios_base::failbit);
          break;
        }
      }
    }

    if (__tmp.empty())
      __is.setstate(ios_base::failbit);
    else
      __x._M_copy_from_string(__tmp, static_cast<size_t>(0), _Nb);
  }

  return __is;
}

template <class _CharT, class _Traits, size_t _Nb>
basic_ostream<_CharT, _Traits>&
operator<<(basic_ostream<_CharT, _Traits>& __os, const bitset<_Nb>& __x)
{
  basic_string<_CharT, _Traits> __tmp;
  __x._M_copy_to_string(__tmp);
  return __os << __tmp;
}

} // namespace std

#undef __BITSET_WORDS

#endif /* __SGI_STL_BITSET */


// Local Variables:
// mode:C++
// End:

