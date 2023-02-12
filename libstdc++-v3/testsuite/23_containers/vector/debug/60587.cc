// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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
// { dg-require-debug-mode "" }

// PR libstdc++/60587

#include <vector>

int main() {
    std::vector<int> a, b;
    a.push_back(1);
    a.insert(a.end(), b.begin(), b.end());
    b.push_back(1L);
    a.insert(a.end(), b.begin(), b.end());

    std::vector<long> c;
    a.insert(a.end(), c.begin(), c.end());
    c.push_back(1L);
    a.insert(a.end(), c.begin(), c.end());
}
