// Iostreams base classes -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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
// ISO C++ 14882: 27.4  Iostreams base classes
//

#include <bits/std_ios.h>
#include <bits/std_ostream.h>
#include <bits/std_istream.h>
#include <bits/std_fstream.h>

namespace std 
{
  // Extern declarations for global objects in src/globals.cc.
  extern istream cin;
  extern ostream cout;
  extern ostream cerr;
  extern ostream clog;
  extern filebuf buf_cout;
  extern filebuf buf_cin;
  extern filebuf buf_cerr;

#ifdef _GLIBCPP_USE_WCHAR_T
  extern wistream wcin;
  extern wostream wcout;
  extern wostream wcerr;
  extern wostream wclog;
  extern wfilebuf buf_wcout;
  extern wfilebuf buf_wcin;
  extern wfilebuf buf_wcerr;
#endif

  // Definitions for static const data members of __ios_flags.
  const __ios_flags::__int_type __ios_flags::_S_boolalpha;
  const __ios_flags::__int_type __ios_flags::_S_dec;
  const __ios_flags::__int_type __ios_flags::_S_fixed;
  const __ios_flags::__int_type __ios_flags::_S_hex;
  const __ios_flags::__int_type __ios_flags::_S_internal;
  const __ios_flags::__int_type __ios_flags::_S_left;
  const __ios_flags::__int_type __ios_flags::_S_oct;
  const __ios_flags::__int_type __ios_flags::_S_right;
  const __ios_flags::__int_type __ios_flags::_S_scientific;
  const __ios_flags::__int_type __ios_flags::_S_showbase;
  const __ios_flags::__int_type __ios_flags::_S_showpoint;
  const __ios_flags::__int_type __ios_flags::_S_showpos;
  const __ios_flags::__int_type __ios_flags::_S_skipws;
  const __ios_flags::__int_type __ios_flags::_S_unitbuf;
  const __ios_flags::__int_type __ios_flags::_S_uppercase;
  const __ios_flags::__int_type __ios_flags::_S_adjustfield;
  const __ios_flags::__int_type __ios_flags::_S_basefield;
  const __ios_flags::__int_type __ios_flags::_S_floatfield;

  const __ios_flags::__int_type __ios_flags::_S_badbit;
  const __ios_flags::__int_type __ios_flags::_S_eofbit;
  const __ios_flags::__int_type __ios_flags::_S_failbit;

  const __ios_flags::__int_type __ios_flags::_S_app;
  const __ios_flags::__int_type __ios_flags::_S_ate;
  const __ios_flags::__int_type __ios_flags::_S_bin;
  const __ios_flags::__int_type __ios_flags::_S_in;
  const __ios_flags::__int_type __ios_flags::_S_out;
  const __ios_flags::__int_type __ios_flags::_S_trunc;

  // Definitions for static const members of ios_base.
  const ios_base::fmtflags ios_base::boolalpha;
  const ios_base::fmtflags ios_base::dec;
  const ios_base::fmtflags ios_base::fixed;
  const ios_base::fmtflags ios_base::hex;
  const ios_base::fmtflags ios_base::internal;
  const ios_base::fmtflags ios_base::left;
  const ios_base::fmtflags ios_base::oct;
  const ios_base::fmtflags ios_base::right;
  const ios_base::fmtflags ios_base::scientific;
  const ios_base::fmtflags ios_base::showbase;
  const ios_base::fmtflags ios_base::showpoint;
  const ios_base::fmtflags ios_base::showpos;
  const ios_base::fmtflags ios_base::skipws;
  const ios_base::fmtflags ios_base::unitbuf;
  const ios_base::fmtflags ios_base::uppercase;
  const ios_base::fmtflags ios_base::adjustfield;
  const ios_base::fmtflags ios_base::basefield;
  const ios_base::fmtflags ios_base::floatfield;

  const ios_base::iostate ios_base::badbit;
  const ios_base::iostate ios_base::eofbit;
  const ios_base::iostate ios_base::failbit;
  const ios_base::iostate ios_base::goodbit;

  const ios_base::openmode ios_base::app;
  const ios_base::openmode ios_base::ate;
  const ios_base::openmode ios_base::binary;
  const ios_base::openmode ios_base::in;
  const ios_base::openmode ios_base::out;
  const ios_base::openmode ios_base::trunc;

  const ios_base::seekdir ios_base::beg;
  const ios_base::seekdir ios_base::cur;
  const ios_base::seekdir ios_base::end;

  const int ios_base::_S_local_words;
  int ios_base::Init::_S_ios_base_init = 0;
  bool ios_base::Init::_S_synced_with_stdio = true;

  ios_base::failure::failure(const string& __str) throw()
  {
    strncpy(_M_name, __str.c_str(), _M_bufsize);
    _M_name[_M_bufsize - 1] = '\0';
  }

  ios_base::failure::~failure() throw()
  { }

  const char*
  ios_base::failure::what() const throw()
  { return _M_name; }

  void
  ios_base::Init::_S_ios_create(bool __sync)
  {
    int __out_bufsize = __sync ? 0 : static_cast<int>(BUFSIZ);
    int __in_bufsize = __sync ? 1 : static_cast<int>(BUFSIZ);

#if _GLIBCPP_AVOID_FSEEK
    // Platforms that prefer to avoid fseek() calls on streams only
    // get their desire when the C++-layer input buffer size is 1.
    // This hack hurts performance but keeps correctness across
    // all types of streams that might be attached to (e.g.) cin.
    __in_bufsize = 1;
#endif

    // NB: The file globals.cc creates the four standard files
    // with NULL buffers. At this point, we swap out the dummy NULL
    // [io]stream objects and buffers with the real deal.
    new (&buf_cout) filebuf(stdout, ios_base::out, __out_bufsize);
    new (&buf_cin) filebuf(stdin, ios_base::in, __in_bufsize);
    new (&buf_cerr) filebuf(stderr, ios_base::out, __out_bufsize);
    new (&cout) ostream(&buf_cout);
    new (&cin) istream(&buf_cin);
    new (&cerr) ostream(&buf_cerr);
    new (&clog) ostream(&buf_cerr);
    cin.tie(&cout);
    cerr.flags(ios_base::unitbuf);
    
#ifdef _GLIBCPP_USE_WCHAR_T
    new (&buf_wcout) wfilebuf(stdout, ios_base::out, __out_bufsize);
    new (&buf_wcin) wfilebuf(stdin, ios_base::in, __in_bufsize);
    new (&buf_wcerr) wfilebuf(stderr, ios_base::out, __out_bufsize);
    new (&wcout) wostream(&buf_wcout);
    new (&wcin) wistream(&buf_wcin);
    new (&wcerr) wostream(&buf_wcerr);
    new (&wclog) wostream(&buf_wcerr);
    wcin.tie(&wcout);
    wcerr.flags(ios_base::unitbuf);
#endif
  }

  void
  ios_base::Init::_S_ios_destroy()
  {
    // Explicitly call dtors to free any memory that is dynamically
    // allocated by filebuf ctor or member functions, but don't
    // deallocate all memory by calling operator delete.
    cout.flush();
    cerr.flush();
    clog.flush();
    buf_cout.~filebuf();
    buf_cin.~filebuf();
    buf_cerr.~filebuf();
#ifdef _GLIBCPP_USE_WCHAR_T
    wcout.flush();
    wcerr.flush();
    wclog.flush();
    buf_wcout.~wfilebuf();
    buf_wcin.~wfilebuf();
    buf_wcerr.~wfilebuf();
#endif
  }

  ios_base::Init::Init()
  {
    if (_S_ios_base_init == 0)
      {
	// Standard streams default to synced with "C" operations.
	ios_base::Init::_S_synced_with_stdio = true;
	_S_ios_create(ios_base::Init::_S_synced_with_stdio);
      }
    ++_S_ios_base_init;
  }

  ios_base::Init::~Init()
  {
    if (--_S_ios_base_init == 0)
      _S_ios_destroy();
  } 

  // 27.4.2.5  ios_base storage functions
  int 
  ios_base::xalloc() throw()
  {
    // XXX MT
    // XXX should be a symbol. (Reserve 0..3 for builtins.)
    static int top = 4; 
    return top++;
  }

  // 27.4.2.5  iword/pword storage
  ios_base::_Words&
  ios_base::_M_grow_words(int ix)
  {
    // Precondition: _M_word_limit <= ix
    _Words zero = { 0, 0 };
    int newlimit = _S_local_words;
    _Words* words = _M_word_array;
    int i = 0;
    if (_S_local_words <= ix)
      {
	newlimit = ix+1;
	try
	  { words = new _Words[ix+1]; }
	catch (...)
	  {
	    _M_dummy = zero;  // XXX MT? Not on "normal" machines.
	    // XXX now in basic_ios
	    // _M_clear(_M_rdstate() | badbit);  // may throw
	    return _M_dummy;
	  }
	for (; i < _M_word_limit; i++) 
	  words[i] = _M_words[i];
	if (_M_words != _M_word_array) 
	  delete [] _M_words;
      }
    
    do { words[i] = zero; } while (++i < newlimit);
    _M_words = words;
    _M_word_limit = newlimit;
    return words[ix];
  }
  
  // Called only by basic_ios<>::init.
  void 
  ios_base::_M_init()   
  {
    // NB: May be called more than once
    _M_precision = 6;
    _M_width = 0;
    _M_flags = skipws | dec;
    _M_callbacks = 0;
    _M_words = 0;
    _M_word_limit = 0;
    _M_ios_locale = locale();
    // No init needed for _M_word_array or _M_dummy.
  }  
  
  // 27.4.2.3  ios_base locale functions
  locale
  ios_base::imbue(const locale& __loc)
  {
    locale __old = _M_ios_locale;
    _M_ios_locale = __loc;
    // Make sure there's a callback for the format caches so they will be
    // marked dirty.
    _Format_cache<char>::_S_get(*this);
#ifdef _GLIBCPP_USE_WCHAR_T
    _Format_cache<wchar_t>::_S_get(*this);
#endif
    _M_call_callbacks(imbue_event);
    return __old;
  }

  ios_base::ios_base()
  {
    // Do nothing; init() does it.  Static init to 0 makes everything sane.
  }
  
  // 27.4.2.7  ios_base constructors/destructors
  ios_base::~ios_base()
  {
    _M_call_callbacks(erase_event);
    _M_dispose_callbacks();
    if (_M_words != _M_word_array) 
      delete [] _M_words;
    // XXX done?
  }

  void 
  ios_base::register_callback(event_callback __fn, int __index)
  { _M_callbacks = new _Callback_list(__fn, __index, _M_callbacks); }

  void 
  ios_base::_M_call_callbacks(event __e) throw()
  {
    for (_Callback_list* __p = _M_callbacks; __p; __p = __p->_M_next)
      {
	try { 
	  (*__p->_M_fn) (__e, *this, __p->_M_index); 
	} 
	catch (...) { 
	}
      }
  }

  void 
  ios_base::_M_dispose_callbacks(void)
  {
    _Callback_list* __p = _M_callbacks;
    while (__p && __p->_M_remove_reference() == 0)
      {
	_Callback_list* __next = __p->_M_next;
	delete __p;
	__p = __next;
      }
    _M_callbacks = 0;
  }

  bool 
  ios_base::sync_with_stdio(bool __sync)
  { 
#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
    // 49.  Underspecification of ios_base::sync_with_stdio
    bool __ret = ios_base::Init::_S_synced_with_stdio;
#endif

    // Turn off sync with C FILE* for cin, cout, cerr, clog iff
    // currently synchronized.
    if (!__sync && __ret)
      {
	ios_base::Init::_S_synced_with_stdio = false;
	ios_base::Init::_S_ios_destroy();
	ios_base::Init::_S_ios_create(ios_base::Init::_S_synced_with_stdio);
      }
    return __ret; 
  }
}  // namespace std

