
// Copyright (C) 1997-1999 Free Software Foundation, Inc.
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

#ifndef _CPP_EXCEPTION
#define _CPP_EXCEPTION 1

#ifdef __GNUG__
#include_next <exception>
#else

#include <bits/stl_config.h>

__STL_BEGIN_NAMESPACE

    // 18.6  Exception handling
    class exception;
    class bad_exception;
     
    typedef void (*unexpected_handler)();
    unexpected_handler set_unexpected(unexpected_handler) throw();
    void unexpected();
    typedef void (*terminate_handler)();
    terminate_handler set_terminate(terminate_handler) throw();
    void terminate();
    bool uncaught_exception();

    // 18.6.1  Class exception 
    class exception {
    public:
      exception() throw();
      exception(const exception&) throw();
      exception& operator=(const exception&) throw();
      virtual ~exception() throw();
      virtual const char* what() const throw();
    };

    // 18.6.2.1  Class bad_exception 
    class bad_exception : public exception {
    public:
      bad_exception() throw();
      bad_exception(const bad_exception&) throw();
      bad_exception& operator=(const bad_exception&) throw();
      virtual ~bad_exception() throw();
      virtual const char* what() const throw();
    };

__STL_END_NAMESPACE

#endif /* __GNUG__ */

#endif /* _CPP_EXCEPTION */

// Local Variables:
// mode:C++
// End:
