// std::time_get, std::time_put implementation, generic version -*- C++ -*-

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

//
// ISO C++ 14882: 22.2.5.1.2 - time_get virtual functions
// ISO C++ 14882: 22.2.5.3.2 - time_put virtual functions
//

// Written by Benjamin Kosnik <bkoz@redhat.com>

  template<typename _CharT>
    void
    __timepunct<_CharT>::
    _M_put_helper(char* __s, size_t __maxlen, const char* __format, 
		  const tm* __tm) const
    {
      setlocale(LC_ALL, _M_name_timepunct);
      strftime(__s, __maxlen, __format, __tm); 
    }

  template<typename _CharT>
    void
    __timepunct<_CharT>::
    _M_get_helper(const char*, const char*, tm*) const
    {
      setlocale(LC_ALL, _M_name_timepunct);
      // strptime(__s, __format, __tm);
    }
