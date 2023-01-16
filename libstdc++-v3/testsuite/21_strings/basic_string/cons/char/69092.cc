// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

// PR libstdc++/69092

#include <string>
#include <iterator>

struct hate_T_iterator {

    typedef std::forward_iterator_tag iterator_category;
    typedef char value_type;
    typedef std::ptrdiff_t difference_type;
    typedef char* pointer;
    typedef char& reference;

    explicit hate_T_iterator(char* p) : p(p) {}
    char* p;

    hate_T_iterator& operator++() { ++p; return *this; }

    hate_T_iterator operator++(int)
    {
      hate_T_iterator r = *this;
      ++*this; return r;
    }

    char& operator*() const
    {
      if (*p == 'T')
        throw 1;
      return *p;
    }

    char* operator->() const { return p; }

    bool operator== (hate_T_iterator other) const { return p == other.p;}
    bool operator!= (hate_T_iterator other) const { return p != other.p;}
};

int main()
{
  char test_str[4] = "ATA";
  try {
    std::string s(hate_T_iterator(test_str), hate_T_iterator(test_str+3));
  }
  catch(int) {
  }
}
