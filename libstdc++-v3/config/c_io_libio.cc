// Wrapper of C-language FILE struct -*- C++ -*-

// Copyright (C) 2000 Free Software Foundation, Inc.
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

#include <bits/basic_file.h>
#include <libioP.h>
#include <fcntl.h> 		// Solaris needs for O_* macros

namespace std {
  
  __basic_file::__basic_file(__c_lock* __lock)
  {
    _lock = __lock;
    _IO_init(this, 0);     
    _IO_file_init((_IO_FILE_plus*) this); 
    _IO_file_attach(this, -1);
  }

  int 
  __basic_file::get_fileno(void)
  { return _fileno; }
 
  __basic_file::~__basic_file()
  {
    if (this->is_open())
      {
	_IO_do_flush(this);
	if (!(_flags & _IO_DELETE_DONT_CLOSE))
	  _IO_SYSCLOSE((_IO_FILE*)this);
      }
    _IO_default_finish(this, 0);
  }
      
  void 
  __basic_file::_M_open_mode(ios_base::openmode __mode, int& __p_mode, 
			     int& __rw_mode)
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

  __basic_file*
  __basic_file::sys_open(int __fd, ios_base::openmode __mode) 
  {
    __basic_file* __retval = NULL;
    int __p_mode = 0;
    int __rw_mode = _IO_NO_READS + _IO_NO_WRITES; 

    _M_open_mode(__mode, __p_mode, __rw_mode);
    if (__fd >= 0)
      {
	_fileno = __fd;
	int __mask = _IO_NO_READS + _IO_NO_WRITES + _IO_IS_APPENDING;
	_flags = (_flags & ~__mask) | (__rw_mode & __mask);
	_IO_link_in((_IO_FILE_plus*) this); 
	__retval = this;
      }
    return __retval;
  }

  __basic_file* 
  __basic_file::open(const char* __name, ios_base::openmode __mode, 
		     int __prot = 0664)
  {
    __basic_file* __retval = NULL;
    int __p_mode = 0;
    int __rw_mode = _IO_NO_READS + _IO_NO_WRITES; 

    _M_open_mode(__mode, __p_mode, __rw_mode);
    if (!_IO_file_is_open(this))
      {
#if _G_HAVE_IO_FILE_OPEN
	__c_file_type* __f;
	__f = _IO_file_open(this, __name, __p_mode, __prot, __rw_mode, 0);
	_flags &= ~_IO_DELETE_DONT_CLOSE;
	__retval = __f ? this: NULL;
#else
	int __fd = ::open(__name, __p_mode, __prot);
	if (__fd >= 0)
	  {
	    _fileno = __fd;	   
	    int __mask = _IO_NO_READS + _IO_NO_WRITES + _IO_IS_APPENDING;
	    _flags = (_flags & ~__mask) | (__rw_mode & __mask);
	    _IO_link_in(this);
	    __retval = this;
	  }
#endif      
      }
    return __retval;
  }
  
  bool 
  __basic_file::is_open() { return _fileno >= 0; }
  
  __basic_file* 
  __basic_file::close()
  { return _IO_file_close_it(this) ? static_cast<__basic_file*>(NULL) : this; }

  // NB: Unused.
  int 
  __basic_file::overflow(int __c) { return _IO_file_overflow(this, __c); }

  // NB: Unused.
  int 
  __basic_file::underflow()  { return _IO_file_underflow(this); }

  // NB: Unused.
  int 
  __basic_file::uflow()  { return _IO_default_uflow(this); }
  
  // NB: Unused.
  int 
  __basic_file::pbackfail(int __c) 
  { return _IO_default_pbackfail(this, __c); }
  
  streamsize 
  __basic_file::xsputn(const char* __s, streamsize __n)
  { return _IO_file_xsputn(this, __s, __n); }
  
  streamsize 
  __basic_file::xsgetn(char* __s, streamsize __n)
  { return _IO_default_xsgetn(this, __s, __n); }

  streamoff
  __basic_file::seekoff(streamoff __off, ios_base::seekdir __way, 
			ios_base::openmode __mode)
  { return _IO_file_seekoff(this, __off, __way, __mode); }

  streamoff
  __basic_file::seekpos(streamoff __pos, ios_base::openmode __mode)
  { return _IO_file_seekoff(this, __pos, ios_base::beg, __mode); }

  // NB: Unused.
  streambuf* 
  __basic_file::setbuf(char* __b, int __len)
  { return (streambuf*) _IO_file_setbuf(this,__b, __len); }

  int 
  __basic_file::sync()
  { return _IO_file_sync(this); }

  // NB: Unused.
  int 
  __basic_file::doallocate() 
  { return _IO_file_doallocate(this); }

  // NB: Unused.
  streamsize 
  __basic_file::sys_read(char* __s, streamsize __n) 
  { return _IO_file_read(this, __s, __n); }

  // NB: Unused.    
  streamsize 
  __basic_file::sys_write(const char* __s, streamsize __n) 
  { return _IO_file_write(this, __s, __n); }

  // NB: Unused.
  streamoff
  __basic_file::sys_seek(streamoff __pos, ios_base::seekdir __way)
  { return _IO_file_seek(this, __pos, __way); }
  
  // NB: Unused.
  int 
  __basic_file::sys_close() { return _IO_file_close(this); }

  // NB: Unused.
  int 
  __basic_file::sys_stat(void* __v) { return _IO_file_stat(this, __v); }

  // NB: Unused.
  int 
  __basic_file::showmanyc() { return EOF; }

  // NB: Unused.
  void 
  __basic_file::imbue(void* /*__v*/) { }

}  // namespace std







