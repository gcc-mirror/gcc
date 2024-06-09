// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-options "-O1" }
// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// PR middle-end/89303

#include <memory>

class blob final: public std::enable_shared_from_this<blob>
{
  int* data;

public:
  blob() { data = new int; }
  ~blob() { delete data; }
};

int
main()
{
  std::shared_ptr<blob> tg = std::make_shared<blob>();
  return tg->shared_from_this().use_count() - 2;
}
