// -*- C++ -*- header wrapper.

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


#ifndef  _INCLUDED_CPP_PTHREAD_H_
# define _INCLUDED_CPP_PTHREAD_H_ 1

# ifdef _IN_C_LEGACY_  /* sub-included by a C header */
      // get out of the "legacy"
    } // close extern "C"
  }   // close namespace _C_legacy::
#  undef _IN_C_LEGACY_
#  define _PTHREAD_NEED_C_LEGACY_
# endif

# include <bits/wrap_pthread.h>

  // Expose global C names, including non-standard ones, but shadow
  // some names and types with the std:: C++ version.
  using _C_legacy::__sched_param;

  using _C_legacy::pthread_attr_t;
  using _C_legacy::pthread_cond_t;
  using _C_legacy::pthread_condattr_t;
  using _C_legacy::pthread_key_t;
  using _C_legacy::pthread_mutex_t;
  using _C_legacy::pthread_mutexattr_t;
  using _C_legacy::pthread_once_t;
  using _C_legacy::pthread_rwlock_t;
  using _C_legacy::pthread_rwlockattr_t;
  using _C_legacy::pthread_t;

  using _C_legacy::pthread_mutex_init;
  using _C_legacy::pthread_mutex_destroy;
  using _C_legacy::pthread_mutex_lock;
  using _C_legacy::pthread_mutex_trylock;
  using _C_legacy::pthread_mutex_unlock;
  using _C_legacy::pthread_mutexattr_init;
  using _C_legacy::pthread_mutexattr_destroy;
  using _C_legacy::pthread_mutexattr_settype;
  using _C_legacy::pthread_mutexattr_gettype;
  using _C_legacy::pthread_key_create;
  using _C_legacy::pthread_key_delete;
  using _C_legacy::pthread_setspecific;
  using _C_legacy::pthread_getspecific;
  using _C_legacy::pthread_once;
  using _C_legacy::pthread_atfork;

# ifdef _PTHREAD_NEED_C_LEGACY_
  // dive back into the "swamp"
  namespace _C_legacy {
    extern "C" {
#  define _IN_C_LEGACY_
#  undef _PTHREAD_NEED_C_LEGACY_
# endif /* _PTHREAD_NEED_C_LEGACY_ */
#endif /* _INCLUDED_CPP_PTHREAD_H_ */




