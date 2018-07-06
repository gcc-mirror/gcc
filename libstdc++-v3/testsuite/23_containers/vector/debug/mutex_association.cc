// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

#include <set>
#include <debug/vector>

#include <testsuite_hooks.h>

class container : public __gnu_debug::_Safe_sequence<container>
{
public:
  __gnu_cxx::__mutex&
  get_mutex()
  { return this->_M_get_mutex(); }
};

int
main()
{
  std::set<__gnu_cxx::__mutex*> mutexes;
  container conts[17];

  for (int i = 0; i != 16; ++i)
    VERIFY( mutexes.insert(&conts[i].get_mutex()).second );

  VERIFY( !mutexes.insert(&conts[16].get_mutex()).second );
}
