// Iostreams base classes -*- C++ -*-

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

//
// ISO C++ 14882: 27.4  Iostreams base classes
//

#include <bits/std_ios.h>
#include <bits/std_iostream.h>
#include <bits/std_fstream.h>

namespace std {

  // Out-of-line definitions for static const ios_base members.
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

  ios_base::failure::failure(const string& __str)
  {
    strncpy(_M_name, __str.c_str(), _M_bufsize);
    _M_name[_M_bufsize - 1] = '\0';
  }

  int ios_base::Init::_M_ios_base_init = 0;

  ios_base::Init::Init()
  {
    if (++_M_ios_base_init == 1)
      {
	// NB: std_iostream.h creates the four standard files with
	// default buffers. At this point, we swap out the default
	// buffers for the properly-constructed ones, and dispose of
	// the default buffers.
	streambuf* __old;
       	_M_cout = new filebuf(1, "stdout", ios_base::out);
	_M_cin = new filebuf(0, "stdin", ios_base::in);
	_M_cerr = new filebuf(2, "stderr", ios_base::out);
	__old = cout.rdbuf(_M_cout);
	__old->~streambuf();
	__old = cin.rdbuf(_M_cin);
	__old->~streambuf();
	cin.tie(&cout);
	__old = cerr.rdbuf(_M_cerr);
	__old->~streambuf();
	cerr.flags(ios_base::unitbuf);
	__old = clog.rdbuf(_M_cerr);
	__old->~streambuf();
#ifdef _GLIBCPP_USE_WCHAR_T
	wstreambuf* __wold;
	_M_wcout = new wfilebuf(1, "stdout", ios_base::out);
	_M_wcin = new wfilebuf(0, "stdin", ios_base::in);
	_M_wcerr = new wfilebuf(2, "stderr", ios_base::out);
	__wold = wcout.rdbuf(_M_wcout);
	__wold->~wstreambuf();
	__wold = wcin.rdbuf(_M_wcin);
	__wold->~wstreambuf();
	wcin.tie(&wcout);
	__wold = wcerr.rdbuf(_M_wcerr);
	__wold->~wstreambuf();
	wcerr.flags(ios_base::unitbuf);
	__wold = wclog.rdbuf(_M_wcerr);
	__wold->~wstreambuf();
#endif
      }
  }

  ios_base::Init::~Init()
  {
    if (--_M_ios_base_init == 0)
      {
	cout.flush();
	cerr.flush();
	clog.flush();
	delete _M_cout;
	delete _M_cin;
	delete _M_cerr;
	_M_cout = NULL;
	_M_cin = NULL;
	_M_cerr = NULL;
#ifdef _GLIBCPP_USE_WCHAR_T
	wcout.flush();
	wcerr.flush();
	wclog.flush();
	delete _M_wcout;
	delete _M_wcin;
	delete _M_wcerr;
	_M_wcout = NULL;
	_M_wcin = NULL;
	_M_wcerr = NULL;
#endif
      }
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
	do { words[i] = _M_words[i]; } while (++i < _M_word_limit);
	if (_M_words != _M_word_array) delete [] _M_words;
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
    _M_flags = skipws | dec;
    _M_width = 0;
    _M_precision = 6;
    _M_callbacks = 0;
    _M_words = 0;
    _M_word_limit = 0;
    _M_locale_ios = locale();
    // No init needed for _M_word_array or _M_dummy.
  }  
  
  // 27.4.2.3  ios_base locale functions
  locale
  ios_base::imbue(const locale& __loc)
  {
    locale __old = _M_locale_ios;
    _M_locale_ios = __loc;
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
    bool __retval = __ioinit._M_cin->_M_file->get_fileno() == 0;
    
    // Turn off sync with C FILE* for cin, cout, cerr, clog.
    if (!__sync && __retval)
      {
	__ioinit._M_cout = new filebuf();
	__ioinit._M_cout->open("stdout", ios_base::out);
	__ioinit._M_cin = new filebuf();
	__ioinit._M_cin->open("stdin", ios_base::in);
	__ioinit._M_cerr = new filebuf();
	__ioinit._M_cerr->open("stderr", ios_base::out);
	cout.rdbuf(__ioinit._M_cout);
	cin.rdbuf(__ioinit._M_cin);
	cerr.rdbuf(__ioinit._M_cerr);
	cerr.flags(ios_base::unitbuf);
	clog.rdbuf(__ioinit._M_cerr);
#ifdef _GLIBCPP_USE_WCHAR_T
	__ioinit._M_wcout = new wfilebuf();
	__ioinit._M_wcout->open("stdout", ios_base::out);
	__ioinit._M_wcin = new wfilebuf();
	__ioinit._M_wcin->open("stdin", ios_base::in);
	__ioinit._M_wcerr = new wfilebuf();
	__ioinit._M_wcerr->open("stderr", ios_base::out);
	wcout.rdbuf(__ioinit._M_wcout);
	wcin.rdbuf(__ioinit._M_wcin);
	wcerr.rdbuf(__ioinit._M_wcerr);
	wcerr.flags(ios_base::unitbuf);
	wclog.rdbuf(__ioinit._M_wcerr);
#endif
      }
    
    return __retval; 
#endif
  }

}  // namespace std












