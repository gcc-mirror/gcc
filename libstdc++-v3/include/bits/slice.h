// The template and inlines for the -*- C++ -*- slice class.

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

// Written by Gabriel Dos Reis <Gabriel.Dos-Reis@DPTMaths.ENS-Cachan.Fr>

#ifndef _CPP_BITS_SLICE_H
#define _CPP_BITS_SLICE_H

namespace std {

class slice
{
public:
    slice ();
    slice (size_t, size_t, size_t);

    size_t start () const;
    size_t size () const;
    size_t stride () const;

private:
    size_t _M_off;                      // offset
    size_t _M_sz;			// size
    size_t _M_st;			// stride unit
};

inline slice::slice () {}

inline slice::slice (size_t __o, size_t __d, size_t __s)
        : _M_off (__o), _M_sz (__d), _M_st (__s) {}

inline size_t
slice::start () const
  { return _M_off; }

inline size_t
slice::size () const
  { return _M_sz; }

inline size_t
slice::stride () const
  { return _M_st; }

} // std::


#endif /* _CPP_BITS_SLICE_H */

// Local Variables:
// mode:c++
// End:
