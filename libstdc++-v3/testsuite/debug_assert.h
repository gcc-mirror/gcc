// 20000810  Brent Verner <brent@rcfile.org>
//
// Copyright (C) 2000 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.
//
//
// Purpose:
//   This file is included in the various testsuite programs to provide
//   #define(able) assert() behavior for debugging/testing. It may be
//   a suitable location for other furry woodland creatures as well.
//
// Notes:
//   If you find yourself compiling small test progs as much as I
//   do, you can move this file to a location your compiler(s)
//   will find, and possibly add more cheap debugging stuff...
//

#ifndef _CPP_DEBUG_ASSERT_H
#define _CPP_DEBUG_ASSERT_H

#ifdef DEBUG_ASSERT
# include <cassert>
# define VERIFY(fn) assert(fn)

#else
# define VERIFY(fn) test &= (fn)
// should we define this here to make sure no 'unexpected' failures
// happen, or do we require that it be defined in any scope where
// the VERIFY macro is used???
//
// static bool test = true;
#endif


#endif // _CPP_DEBUG_ASSERT_H

