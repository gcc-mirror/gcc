// { dg-require-atomic-builtins "" }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

#include <atomic>

enum class tacos : int
{
  cancun = 4,
  el_loco = 5,
  sabor = 6,
  papalote = 9,
  licious = 44,
  jarritos = 55
};

// should minimally compile and link
int main()
{
  std::atomic<float> af(0.0f);
  float non_af __attribute__((unused)) = af;
  
  std::atomic<tacos> ae(tacos::sabor);
  tacos non_ae __attribute__((unused)) = ae;

  return 0;
}
