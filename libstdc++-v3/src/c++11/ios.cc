// Iostreams base classes -*- C++ -*-

// Copyright (C) 1997-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

//
// ISO C++ 14882: 27.4  Iostreams base classes
//

// Determines the version of ios_base::failure thrown by __throw_ios_failure.
// If !_GLIBCXX_USE_DUAL_ABI this will get undefined automatically.
#define _GLIBCXX_USE_CXX11_ABI 0

#include <ios>
#include <limits>
#include <bits/functexcept.h>

#ifdef _GLIBCXX_USE_NLS
# include <libintl.h>
# define _(msgid)   gettext (msgid)
#else
# define _(msgid)   (msgid)
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  void
  __throw_ios_failure(const char* __s __attribute__((unused)))
  { _GLIBCXX_THROW_OR_ABORT(ios_base::failure(_(__s))); }

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

  _Atomic_word ios_base::Init::_S_refcount;

  bool ios_base::Init::_S_synced_with_stdio = true;

  ios_base::ios_base() throw()
  : _M_precision(), _M_width(), _M_flags(), _M_exception(), 
  _M_streambuf_state(), _M_callbacks(0), _M_word_zero(), 
  _M_word_size(_S_local_word_size), _M_word(_M_local_word), _M_ios_locale()
  {
    // Do nothing: basic_ios::init() does it.  
    // NB: _M_callbacks and _M_word must be zero for non-initialized
    // ios_base to go through ~ios_base gracefully.
  }
  
  // 27.4.2.7  ios_base constructors/destructors
  ios_base::~ios_base()
  {
    _M_call_callbacks(erase_event);
    _M_dispose_callbacks();
    if (_M_word != _M_local_word) 
      {
	delete [] _M_word;
	_M_word = 0;
      }
  }

  // 27.4.2.5  ios_base storage functions
  int 
  ios_base::xalloc() throw()
  {
    // Implementation note: Initialize top to zero to ensure that
    // initialization occurs before main() is started.
    static _Atomic_word _S_top = 0; 
    return __gnu_cxx::__exchange_and_add_dispatch(&_S_top, 1) + 4;
  }

  void 
  ios_base::register_callback(event_callback __fn, int __index)
  { _M_callbacks = new _Callback_list(__fn, __index, _M_callbacks); }

  // 27.4.2.5  iword/pword storage
  ios_base::_Words&
  ios_base::_M_grow_words(int __ix, bool __iword)
  {
    // Precondition: _M_word_size <= __ix
    int __newsize = _S_local_word_size;
    _Words* __words = _M_local_word;
    if (__ix > _S_local_word_size - 1)
      {
	if (__ix < numeric_limits<int>::max())
	  {
	    __newsize = __ix + 1;
	    /* We still need to catch bad_alloc even though we use
	       a nothrow new, because the new-expression can throw
	       a bad_array_new_length.  */
	    __try
	      { __words = new (std::nothrow) _Words[__newsize]; }
	    __catch(const std::bad_alloc&)
	      { __words = nullptr; }
	    if (!__words)
	      {
		_M_streambuf_state |= badbit;
		if (_M_streambuf_state & _M_exception)
		  __throw_ios_failure(__N("ios_base::_M_grow_words "
					  "allocation failed"));
		if (__iword)
		  _M_word_zero._M_iword = 0;
		else
		  _M_word_zero._M_pword = 0;
		return _M_word_zero;
	      }
	    for (int __i = 0; __i < _M_word_size; __i++) 
	      __words[__i] = _M_word[__i];
	    if (_M_word && _M_word != _M_local_word) 
	      {
		delete [] _M_word;
		_M_word = 0;
	      }
	  }
	else
	  {
	    _M_streambuf_state |= badbit;
	    if (_M_streambuf_state & _M_exception)
	      __throw_ios_failure(__N("ios_base::_M_grow_words is not valid"));
	    if (__iword)
	      _M_word_zero._M_iword = 0;
	    else
	      _M_word_zero._M_pword = 0;
	    return _M_word_zero;
	  }
      }
    _M_word = __words;
    _M_word_size = __newsize;
    return _M_word[__ix];
  }

  void 
  ios_base::_M_call_callbacks(event __e) throw()
  {
    _Callback_list* __p = _M_callbacks;
    while (__p)
      {
	__try 
	  { (*__p->_M_fn) (__e, *this, __p->_M_index); } 
	__catch(...) 
	  { }
	__p = __p->_M_next;
      }
  }

  void 
  ios_base::_M_dispose_callbacks(void) throw()
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

  void
  ios_base::_M_move(ios_base& __rhs) noexcept
  {
    _M_precision = __rhs._M_precision;
    _M_width = __rhs._M_width;
    _M_flags = __rhs._M_flags;
    _M_exception = __rhs._M_exception;
    _M_streambuf_state = __rhs._M_streambuf_state;
    _M_callbacks = std::__exchange(__rhs._M_callbacks, nullptr);
    if (_M_word != _M_local_word)
       delete[] _M_word;
    if (__rhs._M_word == __rhs._M_local_word)
     {
       _M_word = _M_local_word;
       _M_word_size = _S_local_word_size;
       for (int __i = 0; __i < _S_local_word_size; __i++)
	 _M_word[__i] = std::__exchange(__rhs._M_word[__i], {});
     }
    else
     {
       _M_word = std::__exchange(__rhs._M_word, __rhs._M_local_word);
       _M_word_size
	 = std::__exchange(__rhs._M_word_size, _S_local_word_size);
     }
    _M_ios_locale = __rhs._M_ios_locale;
  }

  void
  ios_base::_M_swap(ios_base& __rhs) noexcept
  {
    std::swap(_M_precision, __rhs._M_precision);
    std::swap(_M_width, __rhs._M_width);
    std::swap(_M_flags, __rhs._M_flags);
    std::swap(_M_exception, __rhs._M_exception);
    std::swap(_M_streambuf_state, __rhs._M_streambuf_state);
    std::swap(_M_callbacks, __rhs._M_callbacks);
    const bool __lhs_local = _M_word == _M_local_word;
    const bool __rhs_local = __rhs._M_word == __rhs._M_local_word;
    if (__lhs_local && __rhs_local)
     std::swap(_M_local_word, __rhs._M_local_word); // array swap
    else
     {
       if (!__lhs_local && !__rhs_local)
	 std::swap(_M_word, __rhs._M_word);
       else
	 {
	   ios_base* __local;
	   ios_base* __allocated;
	   if (__lhs_local)
	     {
	       __local = this;
	       __allocated = &__rhs;
	     }
	   else
	     {
	       __local = &__rhs;
	       __allocated= this;
	     }
	   for (int __i = 0; __i < _S_local_word_size; __i++)
	     __allocated->_M_local_word[__i] = __local->_M_local_word[__i];
	   __local->_M_word = __allocated->_M_word;
	   __allocated->_M_word = __allocated->_M_local_word;
	 }
       std::swap(_M_word_size, __rhs._M_word_size);
     }
    std::swap(_M_ios_locale, __rhs._M_ios_locale);
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
