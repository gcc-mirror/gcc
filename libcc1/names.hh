/* String declarations.
   Copyright (C) 2014-2016 Free Software Foundation, Inc.

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

#ifndef CC1_PLUGIN_NAMES_HH
#define CC1_PLUGIN_NAMES_HH

namespace cc1_plugin
{
  // This code defines global string constants, one for each method in
  // gcc-c-fe.def.  This is needed so that they can be used as
  // template arguments elsewhere.

#define GCC_METHOD0(R, N) \
  extern const char *N;
#define GCC_METHOD1(R, N, A) \
  extern const char *N;
#define GCC_METHOD2(R, N, A, B) \
  extern const char *N;
#define GCC_METHOD3(R, N, A, B, C) \
  extern const char *N;
#define GCC_METHOD4(R, N, A, B, C, D) \
  extern const char *N;
#define GCC_METHOD5(R, N, A, B, C, D, E) \
  extern const char *N;
#define GCC_METHOD7(R, N, A, B, C, D, E, F, G) \
  extern const char *N;

#include "gcc-c-fe.def"

#undef GCC_METHOD0
#undef GCC_METHOD1
#undef GCC_METHOD2
#undef GCC_METHOD3
#undef GCC_METHOD4
#undef GCC_METHOD5
#undef GCC_METHOD7
};

#endif // CC1_PLUGIN_NAMES_HH
