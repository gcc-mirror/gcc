// -*- C++ -*- header wrapper.

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
// ISO C++ 14882: 20.4.6  C library
//

#ifndef _CPP_CSIGNAL
#define _CPP_CSIGNAL 1

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <signal.h>
      typedef void (*_CPP_CSIGFUN_capture)(int);  // a C function pointer
      typedef sig_atomic_t _CPP_sig_atomic_t_capture; 
      const _CPP_CSIGFUN_capture _CPP_SIG_DFL_capture = SIG_DFL;
      const _CPP_CSIGFUN_capture _CPP_SIG_ERR_capture = SIG_ERR;
      const _CPP_CSIGFUN_capture _CPP_SIG_IGN_capture = SIG_IGN;
    }
    const int _CPP_SIGABRT_capture = SIGABRT;
    const int _CPP_SIGFPE_capture  = SIGFPE;
    const int _CPP_SIGILL_capture  = SIGILL;
    const int _CPP_SIGINT_capture  = SIGINT;
    const int _CPP_SIGSEGV_capture = SIGSEGV;
    const int _CPP_SIGTERM_capture = SIGTERM;

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

#  undef sig_atomic_t
#  undef raise
#  undef signal
#  undef SIG_DFL
#  define SIG_DFL \
     reinterpret_cast<void (*)(int)>(::_C_Swamp::_CPP_SIG_DFL_capture)
#  undef SIG_ERR
#  define SIG_ERR \
     reinterpret_cast<void (*)(int)>(::_C_Swamp::_CPP_SIG_ERR_capture)
#  undef SIG_IGN
#  define SIG_IGN \
     reinterpret_cast<void (*)(int)>(::_C_Swamp::_CPP_SIG_IGN_capture)
#  undef SIGABRT
#  define SIGABRT ::_C_Swamp::_CPP_SIGABRT_capture
#  undef SIGFPE
#  define SIGFPE  ::_C_Swamp::_CPP_SIGFPE_capture 
#  undef SIGILL
#  define SIGILL  ::_C_Swamp::_CPP_SIGILL_capture
#  undef SIGINT
#  define SIGINT  ::_C_Swamp::_CPP_SIGINT_capture
#  undef SIGSEGV
#  define SIGSEGV ::_C_Swamp::_CPP_SIGSEGV_capture
#  undef SIGTERM
#  define SIGTERM ::_C_Swamp::_CPP_SIGTERM_capture

  namespace _C_Swamp {
    namespace _C_Shadow {
      typedef ::_C_Swamp::_CPP_sig_atomic_t_capture sig_atomic_t;
    }
  }
  namespace std {

    // Adopt C names into std::
    using ::_C_Swamp::_C_Shadow::sig_atomic_t;
    using ::_C_Swamp::raise;

    inline void (*signal(int __sig, void (* __fun)(int)))(int)
      { return reinterpret_cast<void (*)(int)>(
          ::_C_Swamp::signal(__sig,
	    reinterpret_cast< ::_C_Swamp::_CPP_CSIGFUN_capture>(__fun)));
      }

  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
      using ::std::signal;
    }
  }

# undef _IN_C_SWAMP_

#endif

