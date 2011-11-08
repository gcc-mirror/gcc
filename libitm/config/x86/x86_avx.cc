/* Copyright (C) 2009, 2011 Free Software Foundation, Inc.
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

#include "libitm_i.h"
#include "dispatch.h"

// ??? Use memcpy for now, until we have figured out how to best instantiate
// these loads/stores.
CREATE_DISPATCH_FUNCTIONS_T_MEMCPY(M256, GTM::abi_disp()->, )

void ITM_REGPARM
_ITM_LM256 (const _ITM_TYPE_M256 *ptr)
{
  GTM::GTM_LB (ptr, sizeof (*ptr));
}

// Helpers for re-aligning two 128-bit values.
#ifdef __XOP__
const __v16qi GTM::GTM_vpperm_shift[16] =
{
  {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 },
  {  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16 },
  {  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17 },
  {  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18 },
  {  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 },
  {  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 },
  {  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21 },
  {  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 },
  {  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 },
  {  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 },
  { 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 },
  { 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26 },
  { 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27 },
  { 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28 },
  { 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29 },
  { 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 },
};
#else
# define INSN0		"movdqa  %xmm1, %xmm0"
# define INSN(N)	"vpalignr $" #N ", %xmm0, %xmm1, %xmm0"
# define TABLE_ENT_0	INSN0 "\n\tret\n\t"
# define TABLE_ENT(N)	".balign 8\n\t" INSN(N) "\n\tret\n\t"

asm(".pushsection .text\n\
	.balign 16\n\
	.globl	GTM_vpalignr_table\n\
	.hidden	GTM_vpalignr_table\n\
	.type	GTM_vpalignr_table, @function\n\
GTM_vpalignr_table:\n\t"
	TABLE_ENT_0
	TABLE_ENT(1)
	TABLE_ENT(2)
	TABLE_ENT(3)
	TABLE_ENT(4)
	TABLE_ENT(5)
	TABLE_ENT(6)
	TABLE_ENT(7)
	TABLE_ENT(8)
	TABLE_ENT(9)
	TABLE_ENT(10)
	TABLE_ENT(11)
	TABLE_ENT(12)
	TABLE_ENT(13)
	TABLE_ENT(14)
	TABLE_ENT(15)
	".balign 8\n\
	.size	GTM_vpalignr_table, .-GTM_vpalignr_table\n\
	.popsection");

# undef INSN0
# undef INSN
# undef TABLE_ENT_0
# undef TABLE_ENT
#endif
