// File descriptor layer for filebuf -*- C++ -*-

// Copyright (C) 2002 Free Software Foundation, Inc.
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

#include <fstream>

namespace __gnu_cxx
{
  template<typename _CharT, typename _Traits = std::char_traits<_CharT> >
    class stdio_filebuf : public std::basic_filebuf<_CharT, _Traits>
    {
    public:
      // Types:
      typedef _CharT                     	        char_type;
      typedef _Traits                    	        traits_type;
      typedef typename traits_type::int_type 		int_type;
      typedef typename traits_type::pos_type 		pos_type;
      typedef typename traits_type::off_type 		off_type;
      
    protected:
      // Stack-based buffer for unbuffered input.
      char_type			_M_unbuf[4];
      
    public:
      stdio_filebuf(int __fd, std::ios_base::openmode __mode, bool __del, 
		    int_type __size);

      stdio_filebuf(std::__c_file* __f, std::ios_base::openmode __mode, 
		    int_type __size = static_cast<int_type>(BUFSIZ));

      virtual
      ~stdio_filebuf();

      int
      fd()
      { return _M_file.fd(); }
    };

  template<typename _CharT, typename _Traits>
    stdio_filebuf<_CharT, _Traits>::~stdio_filebuf()
    { }

  template<typename _CharT, typename _Traits>
    stdio_filebuf<_CharT, _Traits>::
    stdio_filebuf(int __fd, std::ios_base::openmode __mode, bool __del, 
		  int_type __size)
    {
      _M_file.sys_open(__fd, __mode, __del);
      if (this->is_open())
	{
	  _M_mode = __mode;
	  _M_buf_size_opt = __size;
	  
	  if (__size > 0 && __size < 4)
	    {
	      _M_buf = _M_unbuf;
	      _M_buf_size = __size;
	    }
	  else
	    _M_allocate_internal_buffer();
	  
	  _M_set_indeterminate();
	}
    }

  template<typename _CharT, typename _Traits>
    stdio_filebuf<_CharT, _Traits>::
    stdio_filebuf(std::__c_file* __f, std::ios_base::openmode __mode, 
		  int_type __size)
    {
      _M_file.sys_open(__f, __mode);
      if (this->is_open())
	{
	  _M_mode = __mode;
	  _M_buf_size_opt = __size;
	  
	  if (__size > 0 && __size < 4)
	    {
	      _M_buf = _M_unbuf;
	      _M_buf_size = __size;
	    }
	  else
	    _M_allocate_internal_buffer();
	  
	  _M_set_indeterminate();
	}
    }
} // namespace __gnu_cxx
