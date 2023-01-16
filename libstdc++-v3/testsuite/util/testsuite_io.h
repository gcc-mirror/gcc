// -*- C++ -*-
// Testing streambuf/filebuf/stringbuf for the C++ library testsuite.
//
// Copyright (C) 2003-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#ifndef _GLIBCXX_TESTSUITE_IO_H
#define _GLIBCXX_TESTSUITE_IO_H

#include <ios>

namespace __gnu_test
{
  // Used to verify the constraints/requirements on get and put areas
  // as defined in
  // 27.5.1 - Stream buffer requirements: get and put areas
  // 27.8.1.1 - Template class basic_filebuf p 3
  //   If the file is not open (ios_base::in) -> input seq. cannot be read
  //   If the file is not open (ios_base::out) -> output seq. cannot be written
  //   Joint file position
  // 27.8.1.4 - Overridden virtual functions p9
  //   If unbuffered, pbase == pptr == NULL
  // 27.7.1.1 - Basic_stringbuf constructors p 1
  // 27.8.1.2 - Basic_filebuf constructors p 1
  //   ... , initializing the base class with basic_streambuf() 27.5.2.1
  template<typename T>
    class constraint_buf
    : public T
    {
    public:
      bool
      write_position()
      {
	bool one = this->pptr() != 0;
	bool two = this->pptr() < this->epptr();
	return one && two;
      }

      bool
      read_position()
      {
	bool one = this->gptr() != 0;
	bool two = this->gptr() < this->egptr();
	return one && two;
      }

      bool
      unbuffered()
      {
	bool one = this->pbase() == 0;
	bool two = this->pptr() == 0;
	return one && two;
      }

      bool
      check_pointers()
      {
	bool one   = this->eback() == 0;
	bool two   = this->gptr() == 0;
	bool three = this->egptr() == 0;

	bool four  = this->pbase() == 0;
	bool five  = this->pptr() == 0;
	bool six   = this->epptr() == 0;
	return one && two && three && four && five && six;
      }
    };

  typedef  constraint_buf<std::streambuf>   constraint_streambuf;
  typedef  constraint_buf<std::filebuf>     constraint_filebuf;
  typedef  constraint_buf<std::stringbuf>   constraint_stringbuf;
#ifdef _GLIBCXX_USE_WCHAR_T
  typedef  constraint_buf<std::wstreambuf>  constraint_wstreambuf;
  typedef  constraint_buf<std::wfilebuf>    constraint_wfilebuf;
  typedef  constraint_buf<std::wstringbuf>  constraint_wstringbuf;
#endif

  // Used to check if basic_streambuf::pubsync() has been called.
  // This is useful for checking if a function creates [io]stream::sentry
  // objects, since the sentry constructors call tie()->flush().
  template<typename T>
    class sync_buf
    : public T
    {
    private:
      bool m_sync_called;

    public:
      sync_buf()
      : m_sync_called(false)
      { }

      bool sync_called() const
      { return m_sync_called; }

    protected:
      int sync()
      {
	m_sync_called = true;
	return 0;
      }
    };

  typedef  sync_buf<std::streambuf>   sync_streambuf;
#ifdef _GLIBCXX_USE_WCHAR_T
  typedef  sync_buf<std::wstreambuf>  sync_wstreambuf;
#endif

  // Throws on all overflow and underflow calls.
  struct underflow_error: std::exception { };
  struct overflow_error: std::exception { };
  struct positioning_error: std::exception { };

  template<typename T>
    struct fail_buf
    : public T
    {
      typedef typename T::char_type   char_type;
      typedef typename T::int_type    int_type;
      typedef typename T::off_type    off_type;
      typedef typename T::pos_type    pos_type;

    private:
      char_type p[2];

    public:
      fail_buf()
      {
	p[0] = char_type('s');
	p[1] = char_type();
	this->setg(p, p, p + 1);
      }

      virtual int_type underflow()
      {
	throw underflow_error();
	return int_type();
      }

      virtual int_type uflow()
      {
	throw underflow_error();
	return int_type();
      }

      virtual int_type
      overflow(int_type)
      {
	throw overflow_error();
	return int_type();
      }

      virtual pos_type
      seekoff(off_type, std::ios_base::seekdir, std::ios_base::openmode)
      {
	throw positioning_error();
	return pos_type(off_type(-1));
      }

      virtual pos_type
      seekpos(pos_type, std::ios_base::openmode)
      {
	throw positioning_error();
	return pos_type(off_type(-1));
      }

      virtual int
      sync()
      {
	throw positioning_error();
	return 0;
      }
    };

  typedef  fail_buf<std::streambuf>   fail_streambuf;
#ifdef _GLIBCXX_USE_WCHAR_T
  typedef  fail_buf<std::wstreambuf>  fail_wstreambuf;
#endif

  // Facets that throw an exception for every virtual function.
  struct facet_error: std::exception { };

  template<typename T>
    class fail_num_get
    : public std::num_get<T>
    {
      typedef std::ios_base ios_base;
      typedef typename std::num_get<T>::iter_type iter_type;

    protected:
      iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, bool&) const
      { throw facet_error(); return iter_type(); }

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, long&) const
      { throw facet_error(); return iter_type(); }

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     unsigned short&) const
      { throw facet_error(); return iter_type(); }

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     unsigned int&) const
      { throw facet_error(); return iter_type(); }

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     unsigned long&) const
      { throw facet_error(); return iter_type(); }

#ifdef _GLIBCXX_USE_LONG_LONG
      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     long long&) const
      { throw facet_error(); return iter_type(); }

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     unsigned long long&) const
      { throw facet_error(); return iter_type(); }
#endif

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     float&) const
      { throw facet_error(); return iter_type(); }

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     double&) const
      { throw facet_error(); return iter_type(); }

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     long double&) const
      { throw facet_error(); return iter_type(); }

      virtual iter_type
      do_get(iter_type, iter_type, ios_base&, ios_base::iostate&,
	     void*&) const
      { throw facet_error(); return iter_type(); }
    };

  typedef  fail_num_get<char>     fail_num_get_char;
#ifdef _GLIBCXX_USE_WCHAR_T
  typedef  fail_num_get<wchar_t>  fail_num_get_wchar_t;
#endif

  template<typename T>
    class fail_num_put
    : public std::num_put<T>
    {
      typedef std::ios_base ios_base;
      typedef typename std::num_put<T>::iter_type iter_type;
      typedef typename std::num_put<T>::char_type char_type;

    protected:
      iter_type
      do_put(iter_type, ios_base&, char_type, bool) const
      { throw facet_error(); return iter_type(0); }

      virtual iter_type
      do_put(iter_type, ios_base&, char_type, long) const
      { throw facet_error(); return iter_type(0); }

      virtual iter_type
      do_put(iter_type, ios_base&, char_type, unsigned long) const
      { throw facet_error(); return iter_type(0); }

#ifdef _GLIBCXX_USE_LONG_LONG
      virtual iter_type
      do_put(iter_type, ios_base&, char_type, long long) const
      { throw facet_error(); return iter_type(0); }

      virtual iter_type
      do_put(iter_type, ios_base&, char_type, unsigned long long) const
      { throw facet_error(); return iter_type(0); }
#endif

      virtual iter_type
      do_put(iter_type, ios_base&, char_type, double) const
      { throw facet_error(); return iter_type(0); }

      virtual iter_type
      do_put(iter_type, ios_base&, char_type, long double) const
      { throw facet_error(); return iter_type(0); }

      virtual iter_type
      do_put(iter_type, ios_base&, char_type, const void*) const
      { throw facet_error(); return iter_type(0); }
    };

  typedef  fail_num_put<char>     fail_num_put_char;
#ifdef _GLIBCXX_USE_WCHAR_T
  typedef  fail_num_put<wchar_t>  fail_num_put_wchar_t;
#endif
} // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_IO_H

