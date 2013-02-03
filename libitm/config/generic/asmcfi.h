/* Copyright (C) 2011-2013 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"

#ifdef HAVE_AS_CFI_PSEUDO_OP

#define cfi_startproc			.cfi_startproc
#define cfi_endproc			.cfi_endproc
#define cfi_adjust_cfa_offset(n)	.cfi_adjust_cfa_offset n
#define cfi_def_cfa_offset(n)		.cfi_def_cfa_offset n
#define cfi_def_cfa(r,n)		.cfi_def_cfa r, n
#define cfi_rel_offset(r,o)		.cfi_rel_offset r, o
#define cfi_register(o,n)		.cfi_register o, n
#define cfi_offset(r,o)			.cfi_offset r, o
#define cfi_restore(r)			.cfi_restore r
#define cfi_undefined(r)		.cfi_undefined r

#else

#define cfi_startproc
#define cfi_endproc
#define cfi_adjust_cfa_offset(n)
#define cfi_def_cfa_offset(n)
#define cfi_def_cfa(r,n)
#define cfi_rel_offset(r,o)
#define cfi_register(o,n)
#define cfi_offset(r,o)
#define cfi_restore(r)
#define cfi_undefined(r)

#endif /* HAVE_AS_CFI_PSEUDO_OP */
