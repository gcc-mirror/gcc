// Iostreams base classes -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
// Free Software Foundation, Inc.
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

/** @file ios_base.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _CPP_BITS_IOSBASE_H
#define _CPP_BITS_IOSBASE_H 1

#pragma GCC system_header

#include <bits/atomicity.h>

namespace std
{
  // The following definitions of bitmask types are enums, not ints,
  // as permitted (but not required) in the standard, in order to provide
  // better type safety in iostream calls.  A side effect is that
  // expressions involving them are no longer compile-time constants.
  enum _Ios_Fmtflags { _M_ios_fmtflags_end = 1L << 16 };

  inline _Ios_Fmtflags 
  operator&(_Ios_Fmtflags __a, _Ios_Fmtflags __b)
  { return _Ios_Fmtflags(static_cast<int>(__a) & static_cast<int>(__b)); }

  inline _Ios_Fmtflags 
  operator|(_Ios_Fmtflags __a, _Ios_Fmtflags __b)
  { return _Ios_Fmtflags(static_cast<int>(__a) | static_cast<int>(__b)); }

  inline _Ios_Fmtflags 
  operator^(_Ios_Fmtflags __a, _Ios_Fmtflags __b)
  { return _Ios_Fmtflags(static_cast<int>(__a) ^ static_cast<int>(__b)); }

  inline _Ios_Fmtflags 
  operator|=(_Ios_Fmtflags& __a, _Ios_Fmtflags __b)
  { return __a = __a | __b; }

  inline _Ios_Fmtflags 
  operator&=(_Ios_Fmtflags& __a, _Ios_Fmtflags __b)
  { return __a = __a & __b; }

  inline _Ios_Fmtflags 
  operator^=(_Ios_Fmtflags& __a, _Ios_Fmtflags __b)
  { return __a = __a ^ __b; }

  inline _Ios_Fmtflags 
  operator~(_Ios_Fmtflags __a)
  { return _Ios_Fmtflags(~static_cast<int>(__a)); }


  enum _Ios_Openmode { _M_ios_openmode_end = 1L << 16 };

  inline _Ios_Openmode 
  operator&(_Ios_Openmode __a, _Ios_Openmode __b)
  { return _Ios_Openmode(static_cast<int>(__a) & static_cast<int>(__b)); }

  inline _Ios_Openmode 
  operator|(_Ios_Openmode __a, _Ios_Openmode __b)
  { return _Ios_Openmode(static_cast<int>(__a) | static_cast<int>(__b)); }

  inline _Ios_Openmode 
  operator^(_Ios_Openmode __a, _Ios_Openmode __b)
  { return _Ios_Openmode(static_cast<int>(__a) ^ static_cast<int>(__b)); }

  inline _Ios_Openmode 
  operator|=(_Ios_Openmode& __a, _Ios_Openmode __b)
  { return __a = __a | __b; }

  inline _Ios_Openmode 
  operator&=(_Ios_Openmode& __a, _Ios_Openmode __b)
  { return __a = __a & __b; }

  inline _Ios_Openmode 
  operator^=(_Ios_Openmode& __a, _Ios_Openmode __b)
  { return __a = __a ^ __b; }

  inline _Ios_Openmode 
  operator~(_Ios_Openmode __a)
  { return _Ios_Openmode(~static_cast<int>(__a)); }


  enum _Ios_Iostate { _M_ios_iostate_end = 1L << 16 };

  inline _Ios_Iostate 
  operator&(_Ios_Iostate __a, _Ios_Iostate __b)
  { return _Ios_Iostate(static_cast<int>(__a) & static_cast<int>(__b)); }

  inline _Ios_Iostate 
  operator|(_Ios_Iostate __a, _Ios_Iostate __b)
  { return _Ios_Iostate(static_cast<int>(__a) | static_cast<int>(__b)); }

  inline _Ios_Iostate 
  operator^(_Ios_Iostate __a, _Ios_Iostate __b)
  { return _Ios_Iostate(static_cast<int>(__a) ^ static_cast<int>(__b)); }

  inline _Ios_Iostate 
  operator|=(_Ios_Iostate& __a, _Ios_Iostate __b)
  { return __a = __a | __b; }

  inline _Ios_Iostate 
  operator&=(_Ios_Iostate& __a, _Ios_Iostate __b)
  { return __a = __a & __b; }

  inline _Ios_Iostate 
  operator^=(_Ios_Iostate& __a, _Ios_Iostate __b)
  { return __a = __a ^ __b; }

  inline _Ios_Iostate 
  operator~(_Ios_Iostate __a)
  { return _Ios_Iostate(~static_cast<int>(__a)); }

  enum _Ios_Seekdir { _M_ios_seekdir_end = 1L << 16 };

  // 27.4.2  Class ios_base
  class ios_base
  {
  public:
    
    // 27.4.2.1.1  Class ios_base::failure
    class failure : public exception
    {
    public:
#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
      //48.  Use of non-existent exception constructor
      explicit 
      failure(const string& __str) throw();

      // This declaration is not useless:
      // http://gcc.gnu.org/onlinedocs/gcc-3.0.2/gcc_6.html#SEC118
      virtual 
      ~failure() throw();

      virtual const char*
      what() const throw();
      
    private:
      enum { _M_bufsize = 256 };
      char _M_name[_M_bufsize];
#endif
    };

    // 27.4.2.1.2  Type ios_base::fmtflags
    typedef _Ios_Fmtflags fmtflags;
    // 27.4.2.1.2  Type fmtflags
    static const fmtflags boolalpha =   fmtflags(__ios_flags::_S_boolalpha);
    static const fmtflags dec =         fmtflags(__ios_flags::_S_dec);
    static const fmtflags fixed =       fmtflags(__ios_flags::_S_fixed);
    static const fmtflags hex =         fmtflags(__ios_flags::_S_hex);
    static const fmtflags internal =    fmtflags(__ios_flags::_S_internal);
    static const fmtflags left =        fmtflags(__ios_flags::_S_left);
    static const fmtflags oct =         fmtflags(__ios_flags::_S_oct);
    static const fmtflags right =       fmtflags(__ios_flags::_S_right);
    static const fmtflags scientific =  fmtflags(__ios_flags::_S_scientific);
    static const fmtflags showbase =    fmtflags(__ios_flags::_S_showbase);
    static const fmtflags showpoint =   fmtflags(__ios_flags::_S_showpoint);
    static const fmtflags showpos =     fmtflags(__ios_flags::_S_showpos);
    static const fmtflags skipws =      fmtflags(__ios_flags::_S_skipws);
    static const fmtflags unitbuf =     fmtflags(__ios_flags::_S_unitbuf);
    static const fmtflags uppercase =   fmtflags(__ios_flags::_S_uppercase);
    static const fmtflags adjustfield = fmtflags(__ios_flags::_S_adjustfield);
    static const fmtflags basefield =   fmtflags(__ios_flags::_S_basefield);
    static const fmtflags floatfield =  fmtflags(__ios_flags::_S_floatfield);

    // 27.4.2.1.3  Type ios_base::iostate
    typedef _Ios_Iostate iostate;
    static const iostate badbit =  	iostate(__ios_flags::_S_badbit);
    static const iostate eofbit =  	iostate(__ios_flags::_S_eofbit);
    static const iostate failbit = 	iostate(__ios_flags::_S_failbit);
    static const iostate goodbit = 	iostate(0);

    // 27.4.2.1.4  Type openmode
    typedef _Ios_Openmode openmode;
    static const openmode app =    	openmode(__ios_flags::_S_app);
    static const openmode ate =    	openmode(__ios_flags::_S_ate);
    static const openmode binary = 	openmode(__ios_flags::_S_bin);
    static const openmode in =     	openmode(__ios_flags::_S_in);
    static const openmode out =    	openmode(__ios_flags::_S_out);
    static const openmode trunc =  	openmode(__ios_flags::_S_trunc);

    // 27.4.2.1.5  Type seekdir
    typedef _Ios_Seekdir seekdir;
    static const seekdir beg = 		seekdir(0);
    static const seekdir cur = 		seekdir(SEEK_CUR);
    static const seekdir end = 		seekdir(SEEK_END);

#ifdef _GLIBCPP_DEPRECATED
    typedef int io_state;
    typedef int open_mode;
    typedef int seek_dir;
    
    typedef std::streampos streampos;
    typedef std::streamoff streamoff;
#endif

    // Callbacks;
    enum event
    {
      erase_event,
      imbue_event,
      copyfmt_event
    };

    typedef void (*event_callback) (event, ios_base&, int);

    void 
    register_callback(event_callback __fn, int __index);

  protected:
    // Data Members
    streamsize 		_M_precision;
    streamsize 		_M_width;
    fmtflags 		_M_flags;
    iostate 		_M_exception;
    iostate 	       	_M_streambuf_state;

    // 27.4.2.6  Members for callbacks
    // 27.4.2.6  ios_base callbacks
    struct _Callback_list
    {
      // Data Members
      _Callback_list* 		_M_next;
      ios_base::event_callback 	_M_fn;
      int 			_M_index;
      _Atomic_word		_M_refcount;  // 0 means one reference.
    
      _Callback_list(ios_base::event_callback __fn, int __index, 
		     _Callback_list* __cb)
      : _M_next(__cb), _M_fn(__fn), _M_index(__index), _M_refcount(0) { }
      
      void 
      _M_add_reference() { __atomic_add(&_M_refcount, 1); }

      // 0 => OK to delete.
      int 
      _M_remove_reference() { return __exchange_and_add(&_M_refcount, -1); }
    };

     _Callback_list*  	_M_callbacks;

    void 
    _M_call_callbacks(event __ev) throw();

    void 
    _M_dispose_callbacks(void);

    // 27.4.2.5  Members for iword/pword storage
    struct _Words 
    { 
      void* 	_M_pword; 
      long 	_M_iword; 
      _Words() : _M_pword(0), _M_iword(0) { }
    };

    // Only for failed iword/pword calls.
    _Words  		_M_word_zero;    

    // Guaranteed storage.
    static const int 	_S_local_word_size = 8;
    _Words  		_M_local_word[_S_local_word_size];  

    // Allocated storage.
    int     		_M_word_size;
    _Words* 		_M_word;
 
    _Words& 
    _M_grow_words(int __index);

    // Members for locale and locale caching.
    locale 		_M_ios_locale;

    void 
    _M_init();

  public:

    // 27.4.2.1.6  Class ios_base::Init
    // Used to initialize standard streams. In theory, g++ could use
    // -finit-priority to order this stuff correctly without going
    // through these machinations. 
    class Init 
    {
      friend class ios_base;
    public:
      Init();
      ~Init();
      
      static void
      _S_ios_create(bool __sync);
      
      static void
      _S_ios_destroy();

    private:
      static int 	_S_ios_base_init;
      static bool	_S_synced_with_stdio;
    };

    // Fmtflags state:
    inline fmtflags 
    flags() const { return _M_flags; }

    inline fmtflags 
    flags(fmtflags __fmtfl)
    { 
      fmtflags __old = _M_flags; 
      _M_flags = __fmtfl; 
      return __old; 
    }

    inline fmtflags 
    setf(fmtflags __fmtfl)
    { 
      fmtflags __old = _M_flags; 
      _M_flags |= __fmtfl; 
      return __old; 
    }

    inline fmtflags 
    setf(fmtflags __fmtfl, fmtflags __mask)
    {
      fmtflags __old = _M_flags;
      _M_flags &= ~__mask;
      _M_flags |= (__fmtfl & __mask);
      return __old;
    }

    inline void 
    unsetf(fmtflags __mask) { _M_flags &= ~__mask; }

    inline streamsize 
    precision() const { return _M_precision; }

    inline streamsize 
    precision(streamsize __prec)
    { 
      streamsize __old = _M_precision; 
      _M_precision = __prec; 
      return __old; 
    }

    inline streamsize 
    width() const { return _M_width; }

    inline streamsize 
    width(streamsize __wide)
    { 
      streamsize __old = _M_width; 
      _M_width = __wide; 
      return __old; 
    }

    static bool 
    sync_with_stdio(bool __sync = true);

    // Locales:
    locale 
    imbue(const locale& __loc);

    inline locale 
    getloc() const { return _M_ios_locale; }

    // Storage:
    static int 
    xalloc() throw();

    inline long& 
    iword(int __ix)
    {
      _Words& __word = (__ix < _M_word_size) 
			? _M_word[__ix] : _M_grow_words(__ix);
      return __word._M_iword;
    }

    inline void*& 
    pword(int __ix)
    {
      _Words& __word = (__ix < _M_word_size) 
			? _M_word[__ix] : _M_grow_words(__ix);
      return __word._M_pword;
    }

    // Destructor
    ~ios_base();

  protected:
    ios_base();

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
  //50.  Copy constructor and assignment operator of ios_base
  private:
    ios_base(const ios_base&);

    ios_base& 
    operator=(const ios_base&);
#endif
  };
 
  // 27.4.5.1 fmtflags manipulators:
  inline ios_base& 
  boolalpha(ios_base& __base)
  {
    __base.setf(ios_base::boolalpha);
    return __base;
  }

  inline ios_base& 
  noboolalpha(ios_base& __base)
  {
    __base.unsetf(ios_base::boolalpha);
    return __base;
  }

  inline ios_base& 
  showbase(ios_base& __base)
  {
    __base.setf(ios_base::showbase);
    return __base;
  }

  inline ios_base& 
  noshowbase(ios_base& __base)
  {
    __base.unsetf(ios_base::showbase);
    return __base;
  }

  inline ios_base& 
  showpoint(ios_base& __base)
  {
    __base.setf(ios_base::showpoint);
    return __base;
  }

  inline ios_base& 
  noshowpoint(ios_base& __base)
  {
    __base.unsetf(ios_base::showpoint);
    return __base;
  }

  inline ios_base& 
  showpos(ios_base& __base)
  {
    __base.setf(ios_base::showpos);
    return __base;
  }

  inline ios_base& 
  noshowpos(ios_base& __base)
  {
    __base.unsetf(ios_base::showpos);
    return __base;
  }

  inline ios_base& 
  skipws(ios_base& __base)
  {
    __base.setf(ios_base::skipws);
    return __base;
  }
  
  inline ios_base& 
  noskipws(ios_base& __base)
  {
    __base.unsetf(ios_base::skipws);
    return __base;
  }

  inline ios_base& 
  uppercase(ios_base& __base)
  {
    __base.setf(ios_base::uppercase);
    return __base;
  }

  inline ios_base& 
  nouppercase(ios_base& __base)
  {
    __base.unsetf(ios_base::uppercase);
    return __base;
  }

  inline ios_base& 
  unitbuf(ios_base& __base)
  {
     __base.setf(ios_base::unitbuf);      
     return __base;
  }

  inline ios_base& 
  nounitbuf(ios_base& __base)
  {
     __base.unsetf(ios_base::unitbuf);
     return __base;    
  }

  // 27.4.5.2 adjustfield anipulators:
  inline ios_base& 
  internal(ios_base& __base)
  {
     __base.setf(ios_base::internal, ios_base::adjustfield);
     return __base;    
  }

  inline ios_base& 
  left(ios_base& __base)
  {
    __base.setf(ios_base::left, ios_base::adjustfield);
    return __base;
  }
  
  inline ios_base& 
  right(ios_base& __base)
  {
    __base.setf(ios_base::right, ios_base::adjustfield);
    return __base;
  }
  
  // 27.4.5.3 basefield anipulators:
  inline ios_base& 
  dec(ios_base& __base)
  {
    __base.setf(ios_base::dec, ios_base::basefield);
    return __base;
  }
  
  inline ios_base& 
  hex(ios_base& __base)
  {
    __base.setf(ios_base::hex, ios_base::basefield);
    return __base;
  }

  inline ios_base& 
  oct(ios_base& __base)
  {
    __base.setf(ios_base::oct, ios_base::basefield);
    return __base;
  }
  
  // 27.4.5.4 floatfield anipulators:
  inline ios_base& 
  fixed(ios_base& __base)
  {
    __base.setf(ios_base::fixed, ios_base::floatfield);
    return __base;
  }

  inline ios_base& 
  scientific(ios_base& __base)
  {
    __base.setf(ios_base::scientific, ios_base::floatfield);
    return __base;
  }

} // namespace std

#endif /* _CPP_BITS_IOSBASE_H */

