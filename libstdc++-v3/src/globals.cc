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

#include "bits/c++config.h"
#include "bits/gthr.h"
#include <fstream>
#include <istream>
#include <ostream>

// On AIX, and perhaps other systems, library initialization order is
// not guaranteed.  For example, the static initializers for the main
// program might run before the static initializers for this library.
// That means that we cannot rely on static initialization in the
// library; there is no guarantee that things will get initialized in
// time.  This file contains definitions of all global variables that
// require initialization as arrays of characters.

// Because <iostream> declares the standard streams to be [io]stream
// types instead of say [io]fstream types, it is also necessary to
// allocate the actual file buffers in this file.
namespace std 
{
  // Standard "C" locale.
  typedef char fake_locale_Impl[sizeof(locale::_Impl)]
  __attribute__ ((aligned(__alignof__(locale::_Impl))));
  fake_locale_Impl locale_impl_c;

  typedef char fake_locale[sizeof(locale)]
  __attribute__ ((aligned(__alignof__(locale))));
  fake_locale locale_c;
  

  // Standard stream objects.
  typedef char fake_istream[sizeof(istream)]
  __attribute__ ((aligned(__alignof__(istream))));
  typedef char fake_ostream[sizeof(ostream)] 
  __attribute__ ((aligned(__alignof__(ostream))));
  fake_istream cin;
  fake_ostream cout;
  fake_ostream cerr;
  fake_ostream clog;

  typedef char fake_filebuf[sizeof(filebuf)]
  __attribute__ ((aligned(__alignof__(filebuf))));
  fake_filebuf buf_cout;
  fake_filebuf buf_cin;
  fake_filebuf buf_cerr;

#ifdef _GLIBCPP_USE_WCHAR_T
  typedef char fake_wistream[sizeof(wistream)] 
  __attribute__ ((aligned(__alignof__(wistream))));
  typedef char fake_wostream[sizeof(wostream)] 
  __attribute__ ((aligned(__alignof__(wostream))));
  fake_wistream wcin;
  fake_wostream wcout;
  fake_wostream wcerr;
  fake_wostream wclog;

  typedef char fake_wfilebuf[sizeof(wfilebuf)]
  __attribute__ ((aligned(__alignof__(wfilebuf))));
  fake_wfilebuf buf_wcout;
  fake_wfilebuf buf_wcin;
  fake_wfilebuf buf_wcerr;
#endif


  // Globals for once-only runtime initialization of mutex objects.  This
  // allows static initialization of these objects on systems that need a
  // function call to initialize a mutex.  For example, see stl_threads.h.
#if __GTHREADS
#ifdef __GTHREAD_MUTEX_INIT
  // This path is not needed since static initialization of mutexs works
  // on this platform.
#elif defined(__GTHREAD_MUTEX_INIT_FUNCTION)
  __gthread_once_t _GLIBCPP_once = __GTHREAD_ONCE_INIT;
  __gthread_mutex_t _GLIBCPP_mutex;
  __gthread_mutex_t *_GLIBCPP_mutex_address;
  
  // Once-only initializer function for _GLIBCPP_mutex.  
  void
  _GLIBCPP_mutex_init ()
  { __GTHREAD_MUTEX_INIT_FUNCTION (&_GLIBCPP_mutex); }

  // Once-only initializer function for _GLIBCPP_mutex_address.  
  void
  _GLIBCPP_mutex_address_init ()
  { __GTHREAD_MUTEX_INIT_FUNCTION (_GLIBCPP_mutex_address); }
#endif
#endif // __GTHREADS
}
