// { dg-options "-O0" }
// { dg-additional-options "-D_GLIBCXX_PROFILE_NO_THREADS" { target { ! tls_native } } }
// { dg-do compile { target c++11 } }
// { dg-require-profile-mode "" }

// -*- C++ -*-

// Copyright (C) 2006-2016 Free Software Foundation, Inc.
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

#include <map>
#include <vector>
#include <unordered_map>

using std::map;
using std::vector;
using std::unordered_map;

struct dumb_hash {
  size_t operator()(int x) const {return 0;}
  size_t operator()(int x, int y) const {return x == y;}
};

int main() {
  map<int, int> m_to_umap;
  vector<int> v_to_list;
  unordered_map<int, int> um_too_small;
  unordered_map<int, int> um_too_large(1000000);
  unordered_map<int, int, dumb_hash, dumb_hash> um_dumb_hash;

  for (int i = 0; i < 10000; ++i) {
    m_to_umap[i] = i;
    v_to_list.insert(v_to_list.begin(), i);
    um_too_small[i] = i;
    um_too_small[i] = i;
    um_dumb_hash[i] = i;
  }
}
