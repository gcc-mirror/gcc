// -*- C++ -*-
// Testing streambuf/filebuf/stringbuf for the C++ library testsuite.
//
// Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_TESTSUITE_IO_H
#define _GLIBCXX_TESTSUITE_IO_H

#include <fstream>
#include <sstream>

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
	bool one = this->pptr() != NULL; 
	bool two = this->pptr() < this->epptr();
	return one && two;
      }
      
      bool
      read_position()
      { 
	bool one = this->gptr() != NULL; 
	bool two = this->gptr() < this->egptr();
	return one && two;
      }
      
      bool
      unbuffered() 
      { 
	bool one = this->pbase() == NULL; 
	bool two = this->pptr() == NULL; 
	return one && two;
      }
  
      bool
      check_pointers()
      {
	bool one   = this->eback() == NULL;
	bool two   = this->gptr() == NULL;
	bool three = this->egptr() == NULL;
	
	bool four  = this->pbase() == NULL;
	bool five  = this->pptr() == NULL;
	bool six   = this->epptr() == NULL;
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
  class sync_streambuf : public std::streambuf
  {
  private:
    bool m_sync_called;
    
  public:
    sync_streambuf()
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

  // Throws on all overflow and underflow calls.
  struct underflow_error: std::exception { };
  struct overflow_error: std::exception { };
  struct positioning_error: std::exception { };

  struct fail_streambuf : std::streambuf
  {
  private:
    char p[2];

  public:
    fail_streambuf()
    {
      p[0] = 's';
      p[1] = char();
      setg(p, p, p + 1); 
    }

    virtual int_type underflow() 
    {
      throw underflow_error();
      return -1;
    }
  
    virtual int_type uflow() 
    {
      throw underflow_error();
      return -1;
    }

    virtual int_type
    overflow(int_type)
    {
      throw overflow_error();
      return -1;
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

  // Facets that throw an exception for every virtual function.
  struct facet_error: std::exception { };

  class fail_num_get : public std::num_get<char>
  {
    typedef std::ios_base ios_base;

  protected:
    iter_type 
    do_get(iter_type a, iter_type, ios_base&, ios_base::iostate&, bool&) const
    { throw facet_error(); return iter_type(); }

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate&, long&) const
    { throw facet_error(); return iter_type(); }

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   unsigned short&) const
    { throw facet_error(); return iter_type(); }

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   unsigned int&) const
    { throw facet_error(); return iter_type(); }

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   unsigned long&) const
    { throw facet_error(); return iter_type(); }

#ifdef _GLIBCXX_USE_LONG_LONG 
    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   long long&) const
    { throw facet_error(); return iter_type(); }

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   unsigned long long&) const
    { throw facet_error(); return iter_type(); }
#endif

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   float&) const
    { throw facet_error(); return iter_type(); }

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   double&) const
    { throw facet_error(); return iter_type(); }

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   long double&) const
    { throw facet_error(); return iter_type(); }

    virtual iter_type 
    do_get(iter_type, iter_type, ios_base&, ios_base::iostate& __err, 
	   void*&) const
    { throw facet_error(); return iter_type(); }
  };

  class fail_num_put : public std::num_put<char>
  {
    typedef std::ios_base ios_base;

  protected:
    iter_type 
    do_put(iter_type, ios_base&, char_type __fill, bool __v) const
    { throw facet_error(); return iter_type(NULL); }

    virtual iter_type 
    do_put(iter_type, ios_base&, char_type __fill, long __v) const
    { throw facet_error(); return iter_type(NULL); }

    virtual iter_type 
    do_put(iter_type, ios_base&, char_type __fill, unsigned long) const
    { throw facet_error(); return iter_type(NULL); }

#ifdef _GLIBCXX_USE_LONG_LONG 
    virtual iter_type 
    do_put(iter_type, ios_base&, char_type __fill, long long __v) const
    { throw facet_error(); return iter_type(NULL); }

    virtual iter_type 
    do_put(iter_type, ios_base&, char_type __fill, unsigned long long) const
    { throw facet_error(); return iter_type(NULL); }
#endif

    virtual iter_type 
    do_put(iter_type, ios_base&, char_type __fill, double __v) const
    { throw facet_error(); return iter_type(NULL); }

    virtual iter_type 
    do_put(iter_type, ios_base&, char_type __fill, long double __v) const
    { throw facet_error(); return iter_type(NULL); }

    virtual iter_type 
    do_put(iter_type, ios_base&, char_type __fill, const void* __v) const
    { throw facet_error(); return iter_type(NULL); }
  };
}; // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_IO_H

