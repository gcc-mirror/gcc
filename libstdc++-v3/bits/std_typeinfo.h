
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


#ifndef _CPP_TYPEINFO
#define _CPP_TYPEINFO 1

#include <bits/c++config.h>
#include <bits/std_exception.h>

#ifdef __GNUG__
# include_next <typeinfo>
#else

__STL_BEGIN_NAMESPACE

    class type_info {
    public:
      virtual ~type_info();
      bool operator==(const type_info& rhs) const;
      bool operator!=(const type_info& rhs) const;
      bool before(const type_info& rhs) const;
      const char* name() const;
    private:
      type_info(const type_info& rhs);
      type_info& operator=(const type_info& rhs);
    };

    class bad_cast : public exception {
    public:
      bad_cast() throw();
      bad_cast(const bad_cast&) throw();
      bad_cast& operator=(const bad_cast&) throw();
      virtual ~bad_cast() throw();
      virtual const char* what() const throw();
    };

    class bad_typeid : public exception {
    public:
      bad_typeid() throw();
      bad_typeid(const bad_typeid&) throw();
      bad_typeid& operator=(const bad_typeid&) throw();
      virtual ~bad_typeid() throw();
      virtual const char* what() const throw();
    };

__STL_END_NAMESPACE

#endif

#endif /* _CPP_TYPEINFO */

// Local Variables:
// mode:C++
// End:
