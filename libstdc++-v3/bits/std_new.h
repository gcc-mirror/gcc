
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

#ifndef _CPP_NEW
#define _CPP_NEW 1

#include <bits/c++config.h>

#ifdef __GNUG__
# include_next <new>
#else

#include <bits/std_exception.h>


// 18.4  Dynamic memory management

__STL_BEGIN_NAMESPACE

    class bad_alloc;
    struct nothrow_t {};
    extern const nothrow_t nothrow;
    typedef void (*new_handler)();
    new_handler set_new_handler(new_handler) throw();

    class bad_alloc : public exception {
    public:
      bad_alloc() throw();
      bad_alloc(const bad_alloc&) throw();
      bad_alloc& operator=(const bad_alloc&) throw();
      virtual ~bad_alloc() throw();
      virtual const char* what() const throw();
    };

__STL_END_NAMESPACE

    void* operator new(__STD::size_t) throw(__STD::bad_alloc);
    void* operator new(__STD::size_t, const __STD::nothrow_t&) throw();
    void  operator delete(void*) throw();
    void  operator delete(void*, const __STD::nothrow_t&) throw();
    void* operator new[](__STD::size_t) throw(__STD::bad_alloc);
    void* operator new[](__STD::size_t, const __STD::nothrow_t&) throw();
    void  operator delete[](void*) throw();
    void  operator delete[](void*, const __STD::nothrow_t&) throw();
    void* operator new  (__STD::size_t, void*) throw();
    void* operator new[](__STD::size_t, void*) throw();
    void  operator delete  (void*, void*) throw();
    void  operator delete[](void*, void*) throw();

#endif

#endif /* _CPP_NEW */

// Local Variables:
// mode:C++
// End:
