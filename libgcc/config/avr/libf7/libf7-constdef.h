/* Copyright (C) 2019-2023 Free Software Foundation, Inc.

   This file is part of LIBF7, which is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#if !defined STATIC
#define STATIC /* empty */
#endif

#if USE_LPM
#define F7_CONST_DEF(NAME, FLAGS, M6, M5, M4, M3, M2, M1, M0, EXPO) \
  STATIC const __attribute__((__progmem__))			    \
  f7_t F7_(const_ ## NAME ## _P) =				    \
  { .flags = FLAGS, .mant = { M0, M1, M2, M3, M4, M5, M6 }, .expo = EXPO };
  #include "libf7-const.def"
#undef F7_CONST_DEF
#else
#define F7_CONST_DEF(NAME, FLAGS, M6, M5, M4, M3, M2, M1, M0, EXPO) \
  STATIC const f7_t F7_(const_ ## NAME) =			    \
  { .flags = FLAGS, .mant = { M0, M1, M2, M3, M4, M5, M6 }, .expo = EXPO };
  #include "libf7-const.def"
#undef F7_CONST_DEF
#endif // USE_LPM

#undef STATIC
