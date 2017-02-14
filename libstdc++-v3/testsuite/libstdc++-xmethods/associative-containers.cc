// { dg-do run { target c++11 } }
// { dg-options "-g -O0" }

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <set>
#include <map>
#include <unordered_set>
#include <unordered_map>

int
main ()
{
  std::set<int> s, s1;
  std::multiset<int> ms, ms1;
  std::unordered_set<int> us, us1;
  std::unordered_multiset<int> ums, ums1;
  std::map<char, int> m, m1;
  std::multimap<char, int> mm, mm1;
  std::unordered_map<char, int> um, um1;
  std::unordered_multimap<char, int> umm, umm1;

  for (int i = 0; i < 100; i++)
    {
      s.insert (i % 5);
      ms.insert (i % 5);
      us.insert (i % 7);
      ums.insert (i % 7);

      m.insert(std::pair<char, int> ('a' + i % 5, i));
      mm.insert(std::pair<char, int> ('a' + i % 5, i));
      um.insert(std::pair<char, int> ('a' + i % 7, i));
      umm.insert(std::pair<char, int> ('a' + i % 7, i));
    }

// { dg-final { note-test s.size() 5 } }
// { dg-final { note-test s.empty() false } }
// { dg-final { note-test s1.empty() true } }
// { dg-final { note-test ms.size() 100 } }
// { dg-final { note-test ms.empty() false } }
// { dg-final { note-test ms1.empty() true } }
// { dg-final { note-test us.size() 7 } }
// { dg-final { note-test us.empty() false } }
// { dg-final { note-test us1.empty() true } }
// { dg-final { note-test ums.size() 100 } }
// { dg-final { note-test ums.empty() false } }
// { dg-final { note-test ums1.empty() true } }
// { dg-final { note-test m.size() 5 } }
// { dg-final { note-test m.empty() false } }
// { dg-final { note-test m1.empty() true } }
// { dg-final { note-test mm.size() 100 } }
// { dg-final { note-test mm.empty() false } }
// { dg-final { note-test mm1.empty() true } }
// { dg-final { note-test um.size() 7 } }
// { dg-final { note-test um.empty() false } }
// { dg-final { note-test um1.empty() true } }
// { dg-final { note-test umm.size() 100 } }
// { dg-final { note-test umm.empty() false } }
// { dg-final { note-test umm1.empty() true } }

// { dg-final { whatis-test s.size() std::size_t } }
// { dg-final { whatis-test s.empty() bool } }

  return 0;  // Mark SPOT
}

// { dg-final { gdb-test SPOT {} 1 } }
