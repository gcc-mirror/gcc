/* Copyright (C) 2012-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Atomic built-in C functions for link compatibility with older code that
   was compiled to emit function calls for atomic built-ins.
   Notice that if no atomic model has been selected the functions in this
   file must not be generated, or else they will result in infinite no-op
   loops.
   Notice also, that all the generated functions below take three parameters,
   which is not actually true for some of the built-in functions.  However,
   on SH this does not matter, since the first four parameters are always
   passed in call clobbered registers.
   The return type for the sync_bool_compare_and_swap functions is also
   actually supposed to be a bool, but this also doesn't matter since any
   int return type <= 32 bit is returned in R0 on SH.  */

#if !__SH_ATOMIC_MODEL_NONE__

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;

#define uint8_t_sz 1
#define uint16_t_sz 2
#define uint32_t_sz 4

#define typesz(x) x##_sz

#define concat(x,y) __ ## x ## _ ## y
#define eval(x,y) concat (x,y)
#define genname(f,t) eval(f, typesz (t))

#define func1(name, type) \
  type __attribute__((visibility("hidden"))) \
  genname (name, type) (type* x, type y, type z) \
  { \
    return __##name (x, y, z); \
  }

#define genfuncs(name) \
  func1 (name, uint8_t) \
  func1 (name, uint16_t) \
  func1 (name, uint32_t)

genfuncs (sync_lock_test_and_set)
genfuncs (sync_val_compare_and_swap)
genfuncs (sync_bool_compare_and_swap)
genfuncs (sync_fetch_and_add)
genfuncs (sync_fetch_and_or)
genfuncs (sync_fetch_and_and)
genfuncs (sync_fetch_and_xor)
genfuncs (sync_fetch_and_sub)
genfuncs (sync_fetch_and_nand)
genfuncs (sync_add_and_fetch)
genfuncs (sync_or_and_fetch)
genfuncs (sync_and_and_fetch)
genfuncs (sync_xor_and_fetch)
genfuncs (sync_sub_and_fetch)
genfuncs (sync_nand_and_fetch)

#endif
