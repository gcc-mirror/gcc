// Wrapper of C-language FILE struct -*- C++ -*-

// Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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
#include <fcntl.h>
#include <unistd.h>

#ifdef _GLIBCPP_HAVE_SYS_IOCTL_H
#define BSD_COMP /* Get FIONREAD on Solaris2. */
#include <sys/ioctl.h>
#endif

// Pick up FIONREAD on Solaris 2.5.
#ifdef _GLIBCPP_HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#ifdef _GLIBCPP_HAVE_POLL
#include <poll.h>
#endif

#if defined(_GLIBCPP_HAVE_S_ISREG) || defined(_GLIBCPP_HAVE_S_IFREG)
# include <sys/stat.h>
# ifdef _GLIBCPP_HAVE_S_ISREG
#  define _GLIBCPP_ISREG(x) S_ISREG(x)
# else
#  define _GLIBCPP_ISREG(x) (((x) & S_IFMT) == S_IFREG)
# endif
#endif

namespace std 
{
  // Definitions for __basic_file<char>.
  __basic_file<char>::__basic_file(__c_lock* /*__lock*/) 
  : _M_cfile(NULL), _M_cfile_created(false) { }

  __basic_file<char>::~__basic_file()
  { this->close(); }
      
  void 
  __basic_file<char>::_M_open_mode(ios_base::openmode __mode, int& __p_mode, 
				   int&, char* __c_mode)
  {  
    bool __testb = __mode & ios_base::binary;
    bool __testi = __mode & ios_base::in;
    bool __testo = __mode & ios_base::out;
    bool __testt = __mode & ios_base::trunc;
    bool __testa = __mode & ios_base::app;
      
    // Set __c_mode for use in fopen.
    // Set __p_mode for use in open.
    if (!__testi && __testo && !__testt && !__testa)
      {
	strcpy(__c_mode, "w");
	__p_mode = (O_WRONLY | O_CREAT);
      }
    if (!__testi && __testo && !__testt && __testa)
      {
	strcpy(__c_mode, "a");
	__p_mode |=  O_WRONLY | O_CREAT | O_APPEND;
      }
    if (!__testi && __testo && __testt && !__testa)
      {
	strcpy(__c_mode, "w");
	__p_mode |=  O_WRONLY | O_CREAT | O_TRUNC;
      }

    if (__testi && !__testo && !__testt && !__testa)
      {
	strcpy(__c_mode, "r");
	__p_mode |=  O_RDONLY;
      }
    if (__testi && __testo && !__testt && !__testa)
      {
	strcpy(__c_mode, "r+");
	__p_mode |=  O_RDWR | O_CREAT;
      }
    if (__testi && __testo && __testt && !__testa)
      {
	strcpy(__c_mode, "w+");
	__p_mode |=  O_RDWR | O_CREAT | O_TRUNC;
      }
    if (__testb)
      strcat(__c_mode, "b");
  }
  
  __basic_file<char>*
  __basic_file<char>::sys_open(__c_file* __file, ios_base::openmode) 
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
  
  __basic_file<char>*
  __basic_file<char>::sys_open(int __fd, ios_base::openmode __mode, 
			       bool __del) 
  {
    __basic_file* __ret = NULL;
    int __p_mode = 0;
    int __rw_mode = 0;
    char __c_mode[4];
    
    _M_open_mode(__mode, __p_mode, __rw_mode, __c_mode);
    if (!this->is_open() && (_M_cfile = fdopen(__fd, __c_mode)))
      {
	// Iff __del is true, then close will fclose the fd.
	_M_cfile_created = __del;

	if (__fd == 0)
	  setvbuf(_M_cfile, reinterpret_cast<char*>(NULL), _IONBF, 0);

	__ret = this;
      }
    return __ret;
  }

  int
  __basic_file<char>::sys_getc() 
  { return getc(_M_cfile); }

  int
  __basic_file<char>::sys_ungetc(int __c) 
  { return ungetc(__c, _M_cfile); }
  
  __basic_file<char>* 
  __basic_file<char>::open(const char* __name, ios_base::openmode __mode, 
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
  
  bool 
  __basic_file<char>::is_open() const 
  { return _M_cfile != 0; }
  
  int 
  __basic_file<char>::fd() 
  { return fileno(_M_cfile) ; }
  
  __basic_file<char>* 
  __basic_file<char>::close()
  { 
    __basic_file* __retval = static_cast<__basic_file*>(NULL);
    if (this->is_open())
      {
	if (_M_cfile_created)
	  fclose(_M_cfile);
	else
	  fflush(_M_cfile);
	_M_cfile = 0;
	__retval = this;
      }
    return __retval;
  }
 
  streamsize 
  __basic_file<char>::xsgetn(char* __s, streamsize __n)
  { return fread(__s, 1, __n, _M_cfile); }
  
  streamsize 
  __basic_file<char>::xsputn(const char* __s, streamsize __n)
  { return fwrite(__s, 1, __n, _M_cfile); }
  
  streamoff
  __basic_file<char>::seekoff(streamoff __off, ios_base::seekdir __way, 
			      ios_base::openmode /*__mode*/)
  { 
    if (!fseek(_M_cfile, __off, __way))
      return ftell(_M_cfile); 
    else
      // Fseek failed.
      return -1L;
  }

  streamoff
  __basic_file<char>::seekpos(streamoff __pos, ios_base::openmode /*__mode*/)
  { 
    if (!fseek(_M_cfile, __pos, ios_base::beg))
      return ftell(_M_cfile);
    else
      // Fseek failed.
      return -1L;
  }
  
  int 
  __basic_file<char>::sync() 
  { return fflush(_M_cfile); }

  streamsize
  __basic_file<char>::showmanyc_helper()
  {
#ifdef FIONREAD
    // Pipes and sockets.    
    int __num = 0;
    int __r = ioctl(this->fd(), FIONREAD, &__num);
    if (!__r && __num >= 0)
      return __num; 
#endif    

#ifdef _GLIBCPP_HAVE_POLL
    // Cheap test.
    struct pollfd __pfd[1];
    __pfd[0].fd = this->fd();
    __pfd[0].events = POLLIN;
    if (poll(__pfd, 1, 0) <= 0)
      return 0;
#endif   

#if defined(_GLIBCPP_HAVE_S_ISREG) || defined(_GLIBCPP_HAVE_S_IFREG)
    // Regular files.
    struct stat __buffer;
    int __ret = fstat(this->fd(), &__buffer);
    if (!__ret && _GLIBCPP_ISREG(__buffer.st_mode))
      return __buffer.st_size - ftell(_M_cfile);
#endif
    return 0;
  }
}  // namespace std
