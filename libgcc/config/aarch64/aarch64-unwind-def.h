/* Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by Arm Ltd.

This file is part of GCC.

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

#if !defined (AARCH64_UNWIND_DEF_H) && !defined (__ILP32__)
#define AARCH64_UNWIND_DEF_H

/* The key used to sign a function's return address.  */
typedef enum {
  AARCH64_PAUTH_KEY_A,
  AARCH64_PAUTH_KEY_B,
} __attribute__((packed)) aarch64_pointer_auth_key;

typedef struct
{
  aarch64_pointer_auth_key signing_key;
} _AArch64Ext_Unwind_FrameState;

#define MD_ARCH_FRAME_STATE_T _AArch64Ext_Unwind_FrameState

#endif /* defined AARCH64_UNWIND_DEF_H && defined __ILP32__ */
