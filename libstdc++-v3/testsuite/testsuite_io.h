// -*- C++ -*-
// Testing filebuf for the C++ library testsuite.
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

#ifndef _GLIBCXX_TESTSUITE_IO_H
#define _GLIBCXX_TESTSUITE_IO_H

#include <fstream>

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
  class constraint_filebuf: public std::filebuf
  {
  public:
    bool
    write_position() 
    { 
      bool two = this->pptr() != NULL; 
      bool one = this->pptr() < this->epptr();
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
    
  };

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
}; // namespace __gnu_test

#endif // _GLIBCXX_TESTSUITE_IO_H

