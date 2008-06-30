// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#ifndef _BACKWARD_BACKWARD_WARNING_H
#define _BACKWARD_BACKWARD_WARNING_H 1

#ifdef __DEPRECATED
#warning \
  This file includes at least one deprecated or antiquated header which \
  may be removed without further notice at a future date. Please use a \
  non-deprecated interface with equivalent functionality instead. For a \
  listing of replacement headers and interfaces, consult the file \
  backward_warning.h. To disable this warning use -Wno-deprecated.

/*
  A list of valid replacements is as follows:

  Use:					Instead of:
  <sstream>, basic_stringbuf	   	<strstream>, strstreambuf
  <sstream>, basic_istringstream	<strstream>, istrstream
  <sstream>, basic_ostringstream	<strstream>, ostrstream
  <sstream>, basic_stringstream		<strstream>, strstream
  <unordered_set>, unordered_set     	<ext/hash_set>, hash_set
  <unordered_set>, unordered_multiset	<ext/hash_set>, hash_multiset
  <unordered_map>, unordered_map	<ext/hash_map>, hash_map
  <unordered_map>, unordered_multimap	<ext/hash_map>, hash_multimap
  <functional>, bind			<functional>, binder1st
  <functional>, bind			<functional>, binder2nd
  <functional>, bind			<functional>, bind1st
  <functional>, bind			<functional>, bind2nd
  <memory>, unique_ptr       		<memory>, auto_ptr
*/

#endif

#endif
