// Wrapper of C-language FILE struct -*- C++ -*-

// Copyright (C) 2000, 2001 Free Software Foundation, Inc.
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
// ISO C++ 14882: 27.8  File-based streams
//

#include <libioP.h>

namespace std 
{
  // __basic_file<char> specializations
  template<>
    __basic_file<char>::__basic_file(__c_lock* __lock);

  template<>
    int 
    __basic_file<char>::overflow(int __c);

  template<>
    int 
    __basic_file<char>::underflow();

  template<>
    int 
    __basic_file<char>::uflow();

  template<>
    int 
    __basic_file<char>::pbackfail(int __c);

  template<>
    streamsize 
    __basic_file<char>::xsputn(const char* __s, streamsize __n);

  template<>
    streamoff
    __basic_file<char>::seekoff(streamoff __off, ios_base::seekdir __way, 
				ios_base::openmode __mode);

  template<>
    streamoff
    __basic_file<char>::seekpos(streamoff __pos, ios_base::openmode __mode);

  template<>
    streambuf* 
    __basic_file<char>::setbuf(char* __b, int __len);

  template<>
    int 
    __basic_file<char>::sync();

  template<>
    int 
    __basic_file<char>::doallocate();

  // __basic_file<wchar_t> specializations
#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    __basic_file<wchar_t>::__basic_file(__c_lock* __lock);

  template<>
    int 
    __basic_file<wchar_t>::overflow(int __c);

  template<>
    int 
    __basic_file<wchar_t>::underflow();

  template<>
    int 
    __basic_file<wchar_t>::uflow();

  template<>
    int 
    __basic_file<wchar_t>::pbackfail(int __c);

  template<>
    streamsize 
    __basic_file<wchar_t>::xsputn(const wchar_t* __s, streamsize __n);

  template<>
    streamoff
    __basic_file<wchar_t>::seekoff(streamoff __off, ios_base::seekdir __way, 
				ios_base::openmode __mode);

  template<>
    streamoff
    __basic_file<wchar_t>::seekpos(streamoff __pos, ios_base::openmode __mode);

  template<>
    streambuf* 
    __basic_file<wchar_t>::setbuf(wchar_t* __b, int __len);

  template<>
    int 
    __basic_file<wchar_t>::sync();

  template<>
    int 
    __basic_file<wchar_t>::doallocate();
#endif

  template<typename _CharT>
    __basic_file<_CharT>::~__basic_file()
    { _IO_file_finish(this, 0); }
      
  template<typename _CharT>
    void 
    __basic_file<_CharT>::_M_open_mode(ios_base::openmode __mode, 
				       int& __p_mode, int& __rw_mode, 
				       char* /*__c_mode*/)
    {  
#ifdef O_BINARY
      bool __testb = __mode & ios_base::binary;
#endif
      bool __testi = __mode & ios_base::in;
      bool __testo = __mode & ios_base::out;
      bool __testt = __mode & ios_base::trunc;
      bool __testa = __mode & ios_base::app;
      
      if (!__testi && __testo && !__testt && !__testa)
	{
	  __p_mode = O_WRONLY | O_TRUNC | O_CREAT;
	  __rw_mode = _IO_NO_READS;
	}
      if (!__testi && __testo && !__testt && __testa)
	{
	  __p_mode = O_WRONLY | O_APPEND | O_CREAT;
	  __rw_mode = _IO_NO_READS | _IO_IS_APPENDING;
	}
      if (!__testi && __testo && __testt && !__testa)
	{
	  __p_mode = O_WRONLY | O_TRUNC | O_CREAT;
	  __rw_mode = _IO_NO_READS;
	}
      if (__testi && !__testo && !__testt && !__testa)
	{
	  __p_mode = O_RDONLY;
	  __rw_mode = _IO_NO_WRITES;
	}
      if (__testi && __testo && !__testt && !__testa)
	{
	  __p_mode = O_RDWR;
	  __rw_mode = 0;
	}
      if (__testi && __testo && __testt && !__testa)
	{
	  __p_mode = O_RDWR | O_TRUNC | O_CREAT;
	  __rw_mode = 0;
	}
#ifdef O_BINARY
      if (__testb)
	__p_mode |= O_BINARY;
#endif	   
    }
  
  template<typename _CharT>
    __basic_file<_CharT>*
    __basic_file<_CharT>::sys_open(__c_file_type* __f, 
				   ios_base::openmode __mode) 
    {
      __basic_file* __ret = NULL;
      int __fd = fileno(__f);
      int __p_mode = 0;
      int __rw_mode = _IO_NO_READS + _IO_NO_WRITES; 
      char __c_mode[4];
      
      _M_open_mode(__mode, __p_mode, __rw_mode, __c_mode);

      if (!_IO_file_is_open(this))
	{
	  _fileno = __fd;
	  _flags &= ~(_IO_NO_READS + _IO_NO_WRITES);
	  _flags |= _IO_DELETE_DONT_CLOSE;
	  _offset = _IO_pos_BAD;
	  int __mask = _IO_NO_READS + _IO_NO_WRITES + _IO_IS_APPENDING;
	  _IO_mask_flags(this, __rw_mode, __mask);
	}

      return __ret;
    }
  
  template<typename _CharT>
    __basic_file<_CharT>* 
    __basic_file<_CharT>::open(const char* __name, ios_base::openmode __mode, 
			       int __prot)
    {
      __basic_file* __ret = NULL;
      int __p_mode = 0;
      int __rw_mode = _IO_NO_READS + _IO_NO_WRITES; 
      char __c_mode[4];

      _M_open_mode(__mode, __p_mode, __rw_mode, __c_mode);
      if (!_IO_file_is_open(this))
	{
	  __c_file_type* __f;
	  __f = _IO_file_open(this, __name, __p_mode, __prot, __rw_mode, 0);
	  __ret = __f ? this: NULL;
	}
      return __ret;
    }
  
  template<typename _CharT>
    bool 
    __basic_file<_CharT>::is_open() { return _fileno >= 0; }
  
  template<typename _CharT>
    __basic_file<_CharT>* 
    __basic_file<_CharT>::close()
    { 
      return _IO_file_close_it(this) ? static_cast<__basic_file*>(NULL) : this;
    }
 
  template<typename _CharT>
    streamsize 
    __basic_file<_CharT>::xsgetn(_CharT* __s, streamsize __n)
    { return _IO_file_xsgetn(this, __s, __n); }

  // NB: Unused.
  template<typename _CharT>
    streamsize 
    __basic_file<_CharT>::sys_read(_CharT* __s, streamsize __n) 
    { return _IO_file_read(this, __s, __n); }

  // NB: Unused.    
  template<typename _CharT>
    streamsize 
    __basic_file<_CharT>::sys_write(const _CharT* __s, streamsize __n) 
    { return _IO_file_write(this, __s, __n); }

  // NB: Unused.
  template<typename _CharT>
    streamoff
    __basic_file<_CharT>::sys_seek(streamoff __pos, ios_base::seekdir __way)
    { return _IO_file_seek(this, __pos, __way); }
  
  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::sys_close() 
    { return _IO_file_close(this); }

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::sys_stat(void* __v) 
    { return _IO_file_stat(this, __v); }

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::showmanyc() { return EOF; }

  // NB: Unused.
  template<typename _CharT>
    void 
    __basic_file<_CharT>::imbue(void* /*__v*/) { }
}  // namespace std
