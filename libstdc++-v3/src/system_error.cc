// <system_error> implementation file

// Copyright (C) 2007
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <cstring>
#include <system_error>
#include <bits/functexcept.h>
#include <limits>

namespace
{
  struct gnu_error_category : public std::error_category
  {
    virtual const std::string& 
    name() const 
    { 
      static const std::string category("GNU");
      return category;
    }

    virtual std::posix_error::posix_errno 
    posix(int __v) const
    {
#ifdef _GLIBCXX_HAVE_SYS_NERR
      const int last_errorno = sys_nerr;
#else
      const int last_errorno = std::numeric_limits<int>::max();
#endif
      if (__v > 0 && __v <= last_errorno)
	return std::posix_error::posix_errno(__v);
      else
	return std::posix_error::no_posix_equivalent;
    }
  };

  const gnu_error_category gnu_category;
}

_GLIBCXX_BEGIN_NAMESPACE(std)

  const error_category& system_category = gnu_category;

  system_error::~system_error() throw() { }

_GLIBCXX_END_NAMESPACE
