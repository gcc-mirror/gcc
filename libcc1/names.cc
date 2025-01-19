/* String definitions.
   Copyright (C) 2014-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <cc1plugin-config.h>
#include "names.hh"

#define GCC_METHOD0(R, N) \
  const char *cc1_plugin::LANG::N = # N;
#define GCC_METHOD1(R, N, A) \
  const char *cc1_plugin::LANG::N = # N;
#define GCC_METHOD2(R, N, A, B) \
  const char *cc1_plugin::LANG::N = # N;
#define GCC_METHOD3(R, N, A, B, C) \
  const char *cc1_plugin::LANG::N = # N;
#define GCC_METHOD4(R, N, A, B, C, D) \
  const char *cc1_plugin::LANG::N = # N;
#define GCC_METHOD5(R, N, A, B, C, D, E) \
  const char *cc1_plugin::LANG::N = # N;
#define GCC_METHOD7(R, N, A, B, C, D, E, F, G) \
  const char *cc1_plugin::LANG::N = # N;

#define LANG c
#include "gcc-c-fe.def"
#undef LANG

#define LANG cp
#include "gcc-cp-fe.def"
#undef LANG

#undef GCC_METHOD0
#undef GCC_METHOD1
#undef GCC_METHOD2
#undef GCC_METHOD3
#undef GCC_METHOD4
#undef GCC_METHOD5
#undef GCC_METHOD7
