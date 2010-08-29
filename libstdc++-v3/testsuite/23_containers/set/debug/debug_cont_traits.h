// Copyright (C) 2010 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

#include <debug/set>

template<typename Type>
  struct cont_traits
  {
    typedef __gnu_debug::set<Type> cont_type;
    typedef Type val_type;

    static val_type
    make_val(Type val)
    { return val; }

    template<typename Iter>
      static void
      insert(cont_type& cont, Iter first, Iter last)
      { cont.insert(first, last); }
  };
