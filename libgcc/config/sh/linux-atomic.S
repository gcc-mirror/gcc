/* Copyright (C) 2006, 2008, 2009 Free Software Foundation, Inc.

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


!! Linux specific atomic routines for the Renesas / SuperH SH CPUs.
!! Linux kernel for SH3/4 has implemented the support for software
!! atomic sequences.

#define FUNC(X)		.type X,@function
#define HIDDEN_FUNC(X)	FUNC(X); .hidden X
#define ENDFUNC0(X)	.Lfe_##X: .size X,.Lfe_##X-X
#define ENDFUNC(X)	ENDFUNC0(X)

#if ! __SH5__

#define ATOMIC_TEST_AND_SET(N,T,EXT) \
	.global	__sync_lock_test_and_set_##N; \
	HIDDEN_FUNC(__sync_lock_test_and_set_##N); \
	.align	2; \
__sync_lock_test_and_set_##N:; \
	mova	1f, r0; \
	nop; \
	mov	r15, r1; \
	mov	#(0f-1f), r15; \
0:	mov.##T	@r4, r2; \
	mov.##T	r5, @r4; \
1:	mov	r1, r15; \
	rts; \
	 EXT	r2, r0; \
	ENDFUNC(__sync_lock_test_and_set_##N)

ATOMIC_TEST_AND_SET (1,b,extu.b)
ATOMIC_TEST_AND_SET (2,w,extu.w)
ATOMIC_TEST_AND_SET (4,l,mov)

#define ATOMIC_COMPARE_AND_SWAP(N,T,EXTS,EXT) \
	.global	__sync_val_compare_and_swap_##N; \
	HIDDEN_FUNC(__sync_val_compare_and_swap_##N); \
	.align	2; \
__sync_val_compare_and_swap_##N:; \
	mova	1f, r0; \
	EXTS	r5, r5; \
	mov	r15, r1; \
	mov	#(0f-1f), r15; \
0:	mov.##T	@r4, r2; \
	cmp/eq	r2, r5; \
	bf	1f; \
	mov.##T	r6, @r4; \
1:	mov	r1, r15; \
	rts; \
	 EXT	r2, r0; \
	ENDFUNC(__sync_val_compare_and_swap_##N)

ATOMIC_COMPARE_AND_SWAP (1,b,exts.b,extu.b)
ATOMIC_COMPARE_AND_SWAP (2,w,exts.w,extu.w)
ATOMIC_COMPARE_AND_SWAP (4,l,mov,mov)

#define ATOMIC_BOOL_COMPARE_AND_SWAP(N,T,EXTS) \
	.global	__sync_bool_compare_and_swap_##N; \
	HIDDEN_FUNC(__sync_bool_compare_and_swap_##N); \
	.align	2; \
__sync_bool_compare_and_swap_##N:; \
	mova	1f, r0; \
	EXTS	r5, r5; \
	mov	r15, r1; \
	mov	#(0f-1f), r15; \
0:	mov.##T	@r4, r2; \
	cmp/eq	r2, r5; \
	bf	1f; \
	mov.##T	r6, @r4; \
1:	mov	r1, r15; \
	rts; \
	 movt	r0; \
	ENDFUNC(__sync_bool_compare_and_swap_##N)

ATOMIC_BOOL_COMPARE_AND_SWAP (1,b,exts.b)
ATOMIC_BOOL_COMPARE_AND_SWAP (2,w,exts.w)
ATOMIC_BOOL_COMPARE_AND_SWAP (4,l,mov)

#define ATOMIC_FETCH_AND_OP(OP,N,T,EXT) \
	.global	__sync_fetch_and_##OP##_##N; \
	HIDDEN_FUNC(__sync_fetch_and_##OP##_##N); \
	.align	2; \
__sync_fetch_and_##OP##_##N:; \
	mova	1f, r0; \
	nop; \
	mov	r15, r1; \
	mov	#(0f-1f), r15; \
0:	mov.##T	@r4, r2; \
	mov	r5, r3; \
	OP	r2, r3; \
	mov.##T	r3, @r4; \
1:	mov	r1, r15; \
	rts; \
	 EXT	r2, r0; \
	ENDFUNC(__sync_fetch_and_##OP##_##N)

ATOMIC_FETCH_AND_OP(add,1,b,extu.b)
ATOMIC_FETCH_AND_OP(add,2,w,extu.w)
ATOMIC_FETCH_AND_OP(add,4,l,mov)

ATOMIC_FETCH_AND_OP(or,1,b,extu.b)
ATOMIC_FETCH_AND_OP(or,2,w,extu.w)
ATOMIC_FETCH_AND_OP(or,4,l,mov)

ATOMIC_FETCH_AND_OP(and,1,b,extu.b)
ATOMIC_FETCH_AND_OP(and,2,w,extu.w)
ATOMIC_FETCH_AND_OP(and,4,l,mov)

ATOMIC_FETCH_AND_OP(xor,1,b,extu.b)
ATOMIC_FETCH_AND_OP(xor,2,w,extu.w)
ATOMIC_FETCH_AND_OP(xor,4,l,mov)

#define ATOMIC_FETCH_AND_COMBOP(OP,OP0,OP1,N,T,EXT) \
	.global	__sync_fetch_and_##OP##_##N; \
	HIDDEN_FUNC(__sync_fetch_and_##OP##_##N); \
	.align	2; \
__sync_fetch_and_##OP##_##N:; \
	mova	1f, r0; \
	mov	r15, r1; \
	mov	#(0f-1f), r15; \
0:	mov.##T	@r4, r2; \
	mov	r5, r3; \
	OP0	r2, r3; \
	OP1	r3, r3; \
	mov.##T	r3, @r4; \
1:	mov	r1, r15; \
	rts; \
	 EXT	r2, r0; \
	ENDFUNC(__sync_fetch_and_##OP##_##N)

ATOMIC_FETCH_AND_COMBOP(sub,sub,neg,1,b,extu.b)
ATOMIC_FETCH_AND_COMBOP(sub,sub,neg,2,w,extu.w)
ATOMIC_FETCH_AND_COMBOP(sub,sub,neg,4,l,mov)

ATOMIC_FETCH_AND_COMBOP(nand,and,not,1,b,extu.b)
ATOMIC_FETCH_AND_COMBOP(nand,and,not,2,w,extu.w)
ATOMIC_FETCH_AND_COMBOP(nand,and,not,4,l,mov)

#define ATOMIC_OP_AND_FETCH(OP,N,T,EXT) \
	.global	__sync_##OP##_and_fetch_##N; \
	HIDDEN_FUNC(__sync_##OP##_and_fetch_##N); \
	.align	2; \
__sync_##OP##_and_fetch_##N:; \
	mova	1f, r0; \
	nop; \
	mov	r15, r1; \
	mov	#(0f-1f), r15; \
0:	mov.##T	@r4, r2; \
	mov	r5, r3; \
	OP	r2, r3; \
	mov.##T	r3, @r4; \
1:	mov	r1, r15; \
	rts; \
	 EXT	r3, r0; \
	ENDFUNC(__sync_##OP##_and_fetch_##N)

ATOMIC_OP_AND_FETCH(add,1,b,extu.b)
ATOMIC_OP_AND_FETCH(add,2,w,extu.w)
ATOMIC_OP_AND_FETCH(add,4,l,mov)

ATOMIC_OP_AND_FETCH(or,1,b,extu.b)
ATOMIC_OP_AND_FETCH(or,2,w,extu.w)
ATOMIC_OP_AND_FETCH(or,4,l,mov)

ATOMIC_OP_AND_FETCH(and,1,b,extu.b)
ATOMIC_OP_AND_FETCH(and,2,w,extu.w)
ATOMIC_OP_AND_FETCH(and,4,l,mov)

ATOMIC_OP_AND_FETCH(xor,1,b,extu.b)
ATOMIC_OP_AND_FETCH(xor,2,w,extu.w)
ATOMIC_OP_AND_FETCH(xor,4,l,mov)

#define ATOMIC_COMBOP_AND_FETCH(OP,OP0,OP1,N,T,EXT) \
	.global	__sync_##OP##_and_fetch_##N; \
	HIDDEN_FUNC(__sync_##OP##_and_fetch_##N); \
	.align	2; \
__sync_##OP##_and_fetch_##N:; \
	mova	1f, r0; \
	mov	r15, r1; \
	mov	#(0f-1f), r15; \
0:	mov.##T	@r4, r2; \
	mov	r5, r3; \
	OP0	r2, r3; \
	OP1	r3, r3; \
	mov.##T	r3, @r4; \
1:	mov	r1, r15; \
	rts; \
	 EXT	r3, r0; \
	ENDFUNC(__sync_##OP##_and_fetch_##N)

ATOMIC_COMBOP_AND_FETCH(sub,sub,neg,1,b,extu.b)
ATOMIC_COMBOP_AND_FETCH(sub,sub,neg,2,w,extu.w)
ATOMIC_COMBOP_AND_FETCH(sub,sub,neg,4,l,mov)

ATOMIC_COMBOP_AND_FETCH(nand,and,not,1,b,extu.b)
ATOMIC_COMBOP_AND_FETCH(nand,and,not,2,w,extu.w)
ATOMIC_COMBOP_AND_FETCH(nand,and,not,4,l,mov)

.section .note.GNU-stack,"",%progbits
.previous

#endif /* ! __SH5__ */
