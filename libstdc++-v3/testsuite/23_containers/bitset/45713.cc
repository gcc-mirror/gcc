// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

// The testcase requires bitsizetype to be wider than sizetype,
// otherwise types/vars with (e.g. for 32-bit sizetype) 0x20000000
// bytes or larger can't be used.  See http://gcc.gnu.org/PR54897
// { dg-do compile { target { ! { avr*-*-* cris*-*-* h8300*-*-* hppa*64*-*-* mcore*-*-* moxie*-*-* mmix-*-* } } } }

#include <bitset>

// libstdc++/45713
int test[sizeof(std::bitset<__SIZE_MAX__>) != 1 ? 1 : -1];
