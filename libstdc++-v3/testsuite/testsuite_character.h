// -*- C++ -*-
// Testing character type and state type with char_traits and codecvt
// specializations for the C++ library testsuite.
//
// Copyright (C) 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.
//
// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _GLIBCXX_TESTSUITE_CHARACTER_H
#define _GLIBCXX_TESTSUITE_CHARACTER_H

#include <string> // for char_traits
#include <locale> // for codecvt
#include <climits>

namespace __gnu_test
{  
  // Character type
  struct character
  {
    unsigned char val;

    static character from_char(char c)
    {
      character ret;
      ret.val = c;
      return ret;
    }
  };

  inline bool
  operator==(const character& lhs, const character& rhs)
  { return lhs.val == rhs.val; }

  // State type.
  struct conversion_state
  {
    unsigned int state;
  };
}; // namespace __gnu_test

namespace std
{
  // char_traits specialization. Meets the additional requirements for
  // basic_filebuf.
  template<>
    struct char_traits<__gnu_test::character>
    {
      typedef __gnu_test::character char_type;
      typedef unsigned int int_type;
      typedef __gnu_test::conversion_state state_type;
      typedef streamoff off_type;
      typedef fpos<state_type> pos_type;

      static void
      assign(char_type& c1, const char_type& c2)
      { c1 = c2; }

      static bool
      eq(const char_type& c1, const char_type& c2)
      { return c1.val == c2.val; }

      static bool
      lt(const char_type& c1, const char_type& c2)
      { return c1.val < c2.val; }

      static int
      compare(const char_type* s1, const char_type* s2, size_t n)
      {
	for (size_t i = 0; i < n; ++i)
	  {
	    if (lt(s1[i], s2[i]))
	      return -1;
	    else if (lt(s2[i], s1[i]))
	      return 1;
	  }
	return 0;
      }

      static size_t
      length(const char_type* s)
      {
	size_t n = 0;
	while (!eq(s[n], char_type()))
	  ++n;
	return n;
      }

      static const char_type*
      find(const char_type* s, size_t n, const char_type& a)
      {
	for (size_t i = 0; i < n; ++i)
	  {
	    if (eq(s[i], a))
	      return s + i;
	  }
	return NULL;
      }

      static char_type*
      move(char_type* s1, const char_type* s2, size_t n)
      {
	if (s1 > s2)
	  {
	    for (size_t i = 0; i < n; ++i)
	      assign(s1[n - i - 1], s2[n - i - 1]);
	  }
	else
	  {
	    for (size_t i = 0; i < n; ++i)
	      assign(s1[i], s2[i]);
	  }
	return s1;
      }

      static char_type*
      copy(char_type* s1, const char_type* s2, size_t n)
      {
	for (size_t i = 0; i < n; ++i)
	  assign(s1[i], s2[i]);
	return s1;
      }

      static char_type*
      assign(char_type* s, size_t n, char_type a)
      {
	for (size_t i = 0; i < n; ++i)
	  assign(s[i], a);
	return s;
      }

      static int_type
      not_eof(const int_type& c)
      {
	if (eq_int_type(c, eof()))
	  return 0;
	return c;
      }

      // Note non-trivial conversion to maximize chance of catching bugs
      static char_type
      to_char_type(const int_type& c)
      {
	char_type ret;
	ret.val = (c >> 5);
	return ret;
      }

      static int_type
      to_int_type(const char_type& c)
      {
	return c.val << 5;
      }

      static bool
      eq_int_type(const int_type& c1, const int_type& c2)
      { return c1 == c2; }

      static int_type eof()
      { return 0xf; }
    };

  // codecvt specialization
  //
  // The conversion performed by the specialization is not supposed to
  // be useful, rather it has been designed to demonstrate the
  // essential features of stateful conversions:
  // * Number and value of bytes for each internal character depends on the
  //   state in addition to the character itself.
  // * Unshift produces an unshift sequence and resets the state. On input
  //   the unshift sequence causes the state to be reset.
  //
  // The conversion for output is as follows:
  // 1. Calculate the value tmp by xor-ing the state and the internal
  //    character
  // 2. Split tmp into either two or three bytes depending on the value of
  //    state. Output those bytes.
  // 3. tmp becomes the new value of state.
  template<>
    class codecvt<__gnu_test::character, char, __gnu_test::conversion_state>
      : public locale::facet, public codecvt_base
    {
    public:
      typedef __gnu_test::character intern_type;
      typedef char extern_type;
      typedef __gnu_test::conversion_state state_type;

      explicit codecvt(size_t refs = 0)
      : locale::facet(refs)
      { }

      result
      out(state_type& state, const intern_type* from,
	  const intern_type* from_end, const intern_type*& from_next,
	  extern_type* to, extern_type* to_limit, extern_type*& to_next) const
      {
	return do_out(state, from, from_end, from_next,
		      to, to_limit, to_next);
      }

      result
      unshift(state_type& state, extern_type* to, extern_type* to_limit,
	      extern_type*& to_next) const
      { return do_unshift(state, to, to_limit, to_next); }

      result
      in(state_type& state, const extern_type* from,
	 const extern_type* from_end, const extern_type*& from_next,
	 intern_type* to, intern_type* to_limit, intern_type*& to_next) const
      {
	return do_in(state, from, from_end, from_next,
		     to, to_limit, to_next);
      }

      int
      encoding() const throw()
      { return do_encoding(); }

      bool
      always_noconv() const throw()
      { return do_always_noconv(); }
      
      int
      length(state_type& state, const extern_type* from,
	     const extern_type* end, size_t max) const
      { return do_length(state, from, end, max); }
      
      int
      max_length() const throw()
      { return do_max_length(); }

      static locale::id id;

    protected:
      ~codecvt()
      { }

      virtual result
      do_out(state_type& state, const intern_type* from,
	     const intern_type* from_end, const intern_type*& from_next,
	     extern_type* to, extern_type* to_limit,
	     extern_type*& to_next) const
      {
	while (from < from_end && to < to_limit)
	  {
	    unsigned char tmp = (state.state ^ from->val);
	    if (state.state & 0x8)
	      {
		if (to >= to_limit - 2)
		  break;
		*to++ = (tmp & 0x7);
		*to++ = ((tmp >> 3) & 0x7);
		*to++ = ((tmp >> 6) & 0x3);
	      }
	    else
	      {
		if (to >= to_limit - 1)
		  break;
		*to++ = (tmp & 0xf);
		*to++ = ((tmp >> 4) & 0xf);
	      }
	    state.state = tmp;
	    ++from;
	  }

	from_next = from;
	to_next = to;
	return (from < from_end) ? partial : ok;
      }

      virtual result
      do_in(state_type& state, const extern_type* from,
	    const extern_type* from_end, const extern_type*& from_next,
	    intern_type* to, intern_type* to_limit,
	    intern_type*& to_next) const
      {
	while (from < from_end && to < to_limit)
	  {
	    unsigned char c = *from;
	    if (c & 0xc0)
	      {
		// Unshift sequence
		state.state &= c;
		++from;
		continue;
	      }

	    unsigned char tmp;
	    if (state.state & 0x8)
	      {
		if (from >= from_end - 2)
		  break;
		tmp = (*from++ & 0x7);
		tmp |= ((*from++ << 3) & 0x38);
		tmp |= ((*from++ << 6) & 0xc0);
	      }
	    else
	      {
		if (from >= from_end - 1)
		  break;
		tmp = (*from++ & 0xf);
		tmp |= ((*from++ << 4) & 0xf0);
	      }
	    to->val = (tmp ^ state.state);
	    state.state = tmp;
	    ++to;
	  }

	from_next = from;
	to_next = to;
	return (from < from_end) ? partial : ok;
      }

      virtual result
      do_unshift(state_type& state, extern_type* to, extern_type* to_limit,
		 extern_type*& to_next) const
      {
	for (unsigned int i = 0; i < CHAR_BIT; ++i)
	  {
	    unsigned int mask = (1 << i);
	    if (state.state & mask)
	      {
		if (to == to_limit)
		  {
		    to_next = to;
		    return partial;
		  }

		state.state &= ~mask;
		*to++ = static_cast<unsigned char>(~mask);
	      }
	  }

	to_next = to;
	return state.state == 0 ? ok : error;
      }

      virtual int
      do_encoding() const throw()
      { return -1; }

      virtual bool
      do_always_noconv() const throw()
      { return false; }

      virtual int
      do_length(state_type& state, const extern_type* from,
		const extern_type* end, size_t max) const
      {
	const extern_type* beg = from;
	while (from < end && max)
	  {
	    unsigned char c = *from;
	    if (c & 0xc0)
	      {
		// Unshift sequence
		state.state &= c;
		++from;
		continue;
	      }

	    unsigned char tmp;
	    if (state.state & 0x8)
	      {
		if (from >= end - 2)
		  break;
		tmp = (*from++ & 0x7);
		tmp |= ((*from++ << 3) & 0x38);
		tmp |= ((*from++ << 6) & 0xc0);
	      }
	    else
	      {
		if (from >= end - 1)
		  break;
		tmp = (*from++ & 0xf);
		tmp |= ((*from++ << 4) & 0xf0);
	      }
	    state.state = tmp;
	    --max;
	  }
	return from - beg;
      }

      // Maximum 8 bytes unshift sequence followed by max 3 bytes for
      // one character.
      virtual int
      do_max_length() const throw()
      { return 11; }
    };

  locale::id
  codecvt<__gnu_test::character, char, __gnu_test::conversion_state>::id;
} // namespace std

#endif // _GLIBCXX_TESTSUITE_CHARACTER_H

