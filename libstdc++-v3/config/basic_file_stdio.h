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

#include <unistd.h>

namespace std 
{
  // Generic definitions for __basic_file
  template<typename _CharT>
    __basic_file<_CharT>::__basic_file(__c_lock* /*__lock*/) 
    : _M_cfile(NULL), _M_cfile_created(false) { }

  template<typename _CharT>
    __basic_file<_CharT>::~__basic_file()
    {
      if (this->is_open())
	{
	  fflush(_M_cfile);
	  this->close();
	}
    }
      
  template<typename _CharT>
    void 
    __basic_file<_CharT>::_M_open_mode(ios_base::openmode __mode, 
				       int& /*__p_mode*/, int& /*__rw_mode*/, 
				       char* __c_mode)
    {  
      bool __testb = __mode & ios_base::binary;
      bool __testi = __mode & ios_base::in;
      bool __testo = __mode & ios_base::out;
      bool __testt = __mode & ios_base::trunc;
      bool __testa = __mode & ios_base::app;
      
      if (!__testi && __testo && !__testt && !__testa)
	strcpy(__c_mode, "w");
      if (!__testi && __testo && !__testt && __testa)
	strcpy(__c_mode, "a");
      if (!__testi && __testo && __testt && !__testa)
	strcpy(__c_mode, "w");
      if (__testi && !__testo && !__testt && !__testa)
	strcpy(__c_mode, "r");
      if (__testi && __testo && !__testt && !__testa)
	strcpy(__c_mode, "r+");
      if (__testi && __testo && __testt && !__testa)
	strcpy(__c_mode, "w+");
      if (__testb)
	strcat(__c_mode, "b");
    }
  
  template<typename _CharT>
    __basic_file<_CharT>*
    __basic_file<_CharT>::sys_open(__c_file_type* __file, ios_base::openmode) 
    {
      __basic_file* __ret = NULL;

      if (!this->is_open() && __file)
	{
	  _M_cfile = __file;
	  _M_cfile_created = false;
	  __ret = this;
	}

      return __ret;
    }
  
  template<typename _CharT>
    __basic_file<_CharT>* 
    __basic_file<_CharT>::open(const char* __name, ios_base::openmode __mode, 
			       int /*__prot*/)
    {
      __basic_file* __ret = NULL;
      int __p_mode = 0;
      int __rw_mode = 0;
      char __c_mode[4];
      
      _M_open_mode(__mode, __p_mode, __rw_mode, __c_mode);

      if (!this->is_open())
	{
	  if ((_M_cfile = fopen(__name, __c_mode)))
	    {
	      _M_cfile_created = true;
	      __ret = this;
	    }
	}
      return __ret;
    }
  
  template<typename _CharT>
    bool 
    __basic_file<_CharT>::is_open() { return _M_cfile != 0; }
  
  template<typename _CharT>
    __basic_file<_CharT>* 
    __basic_file<_CharT>::close()
    { 
      __basic_file* __retval = static_cast<__basic_file*>(NULL);
      if (_M_cfile_created && fclose(_M_cfile))
	__retval = this;
      return __retval;
    }
 
  template<typename _CharT>
    streamsize 
    __basic_file<_CharT>::xsgetn(_CharT* __s, streamsize __n)
    { return fread(__s, 1, __n, _M_cfile); }

  template<typename _CharT>
    streamsize 
    __basic_file<_CharT>::xsputn(const _CharT* __s, streamsize __n)
    { return fwrite(__s, 1, __n, _M_cfile); }
 
  template<typename _CharT>
    streamoff
    __basic_file<_CharT>::seekoff(streamoff __off, ios_base::seekdir __way, 
				  ios_base::openmode /*__mode*/)
    { fseek(_M_cfile, __off, __way); return ftell(_M_cfile); }

  template<typename _CharT>
    streamoff
    __basic_file<_CharT>::seekpos(streamoff __pos, 
				  ios_base::openmode /*__mode*/)
    { fseek(_M_cfile, __pos, ios_base::beg); return ftell(_M_cfile); }

  template<typename _CharT>
    int 
    __basic_file<_CharT>::sync()
    { return fflush(_M_cfile); }

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::overflow(int /*__c*/) 
    { return EOF; }

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::underflow()  
    { return EOF; } 

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::uflow()  
    { return EOF; }

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::pbackfail(int /*__c*/) 
    { return EOF; } 
 
 // NB: Unused.
  template<typename _CharT>
    streambuf* 
    __basic_file<_CharT>::setbuf(_CharT* /*__b*/, int /*__len*/)
    { return reinterpret_cast<streambuf*>(this); }

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::doallocate() 
    { return EOF; }

  // NB: Unused.
  template<typename _CharT>
    streamsize 
    __basic_file<_CharT>::sys_read(_CharT* __s, streamsize __n) 
    { return fread(__s, 1, __n, _M_cfile); }

  // NB: Unused.    
  template<typename _CharT>
    streamsize 
    __basic_file<_CharT>::sys_write(const _CharT* __s, streamsize __n) 
    { return fwrite(__s, 1, __n, _M_cfile); }

  // NB: Unused.
  template<typename _CharT>
    streamoff
    __basic_file<_CharT>::sys_seek(streamoff __pos, ios_base::seekdir __way)
    { 
      fseek(_M_cfile, __pos, __way); 
      return ftell(_M_cfile); 
    }
  
  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::sys_close() 
    { return fclose(_M_cfile); }

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::sys_stat(void* /*__v*/) 
    { return EOF; }

  // NB: Unused.
  template<typename _CharT>
    int 
    __basic_file<_CharT>::showmanyc() 
    { return EOF; }

  // NB: Unused.
  template<typename _CharT>
    void 
    __basic_file<_CharT>::imbue(void* /*__v*/) { }
}  // namespace std
