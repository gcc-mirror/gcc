/* Copyright (C) 2007-2012 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

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

#ifndef MACHINE_GMON_H
#define MACHINE_GMON_H

/* We can't fake out own <sys/types.h> header because the newlib / uclibc
   headers in GCC_FOR_TARGET take precedence.  */

#define __BEGIN_DECLS
#define __END_DECLS

#define __THROW

extern int __dcache_linesz (void);

#define _MCOUNT_DECL(countp, selfpc) \
  static inline void _mcount_internal (void *countp, u_long selfpc)

extern void _mcount (void);
extern void _mcount_call (void);

/* N.B.: the calling point might be a sibcall, thus blink does not necessarily
  hold the caller's address.  r8 doesn't hold the caller's address, either,
  but rather a pointer to the counter data structure associated with the
  caller.
  This function must be compiled with optimization turned on in order to
  enable a sibcall for the final call to selfpc; this is important when trying
  to profile a program with deep tail-recursion that would get a stack
  overflow otherwise.  */
#define MCOUNT \
void \
_mcount_call (void) \
{ \
  register void *countp __asm("r8"); \
  register u_long selfpc __asm("r9"); \
  _mcount_internal (countp, selfpc); \
  ((void (*)(void)) selfpc) (); \
}

extern int __profil (u_short *,size_t, size_t, u_int);

#endif /* MACHINE_GMON_H */
