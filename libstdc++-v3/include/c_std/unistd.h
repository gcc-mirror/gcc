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


#ifndef  _INCLUDED_CPP_UNISTD_H_
# define _INCLUDED_CPP_UNISTD_H_ 1

# ifdef _IN_C_LEGACY_  /* sub-included by a C header */
      // get out of the "legacy"
    } // close extern "C"
  }   // close namespace _C_legacy::
#  undef _IN_C_LEGACY_
#  define _TIME_NEED_C_LEGACY_
# endif

# include <bits/wrap_unistd.h>
 
  // Expose global C names, including non-standard ones, but shadow
  // some names and types with the std:: C++ version.
#ifdef __gid_t_defined
  using _C_legacy::gid_t;
#endif
#ifdef __uid_t_defined
  using _C_legacy::uid_t;
#endif
#ifdef __off_t_defined
  using _C_legacy::off_t;
#endif
#ifdef __off64_t_defined
  using _C_legacy::off64_t;
#endif
#ifdef __useconds_t_defined
  using _C_legacy::useconds_t;
#endif
#ifdef __pid_t_defined
  using _C_legacy::pid_t;
#endif
  using _C_legacy::intptr_t;
#ifdef __socklen_t_defined
  using _C_legacy::socklen_t;
#endif

  using _C_legacy::access;
  using _C_legacy::euidaccess;
  using _C_legacy::lseek;
  using _C_legacy::close;
  using _C_legacy::read;
  using _C_legacy::write;
  using _C_legacy::pread;
  using _C_legacy::pwrite;
  using _C_legacy::pipe;
  using _C_legacy::alarm;
  using _C_legacy::sleep;
  using _C_legacy::ualarm;
  using _C_legacy::usleep;
  using _C_legacy::pause;
  using _C_legacy::chown;
  using _C_legacy::fchown;
  using _C_legacy::lchown;
  using _C_legacy::chdir;
  using _C_legacy::fchdir;
  using _C_legacy::getcwd;
  using _C_legacy::get_current_dir_name;
  using _C_legacy::getwd;
  using _C_legacy::dup;
  using _C_legacy::dup2;

# ifdef _TIME_NEED_C_LEGACY_
  // dive back into the "swamp"
  namespace _C_legacy {
    extern "C" {
#  define _IN_C_LEGACY_
#  undef _TIME_NEED_C_LEGACY_
# endif /* _TIME_NEED_C_LEGACY_ */
#endif /* _INCLUDED_CPP_UNISTD_H_ */
