// generic C header shadow file -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

// This file is included by all the standard C <foo.h> headers
// after defining _SHADOW_NAME.

#ifdef _IN_C_LEGACY_  /* sub-included by a C header */

      // Get out of the "swamp."
    } // Close extern "C"
  }   // Close namespace _C_legacy::

# undef _IN_C_LEGACY_
# include _SHADOW_NAME

  // Dive back into the "swamp."
  namespace _C_legacy {
    extern "C" {
#     define _IN_C_LEGACY_
  
#else /* not _IN_C_LEGACY_:  directly included by user program */
  
# include _SHADOW_NAME
  
  // Expose global C names, including non-standard ones, but shadow
  // some names and types with the std:: C++ version.

  using namespace ::_C_legacy::_C_shadow;
  
#endif /* _IN_C_LEGACY_ */
    


