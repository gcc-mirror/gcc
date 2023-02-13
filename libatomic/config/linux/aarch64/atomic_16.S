/* Copyright (C) 2022-2023 Free Software Foundation, Inc.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
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


	.arch	armv8-a+lse

#define ENTRY(name)		\
	.global name;		\
	.hidden name;		\
	.type name,%function;	\
	.p2align 4;		\
name:				\
	.cfi_startproc;		\
	hint	34	// bti c

#define END(name)		\
	.cfi_endproc;		\
	.size name, .-name;

#define res0 x0
#define res1 x1
#define in0  x2
#define in1  x3
#define tmp0 x6
#define tmp1 x7
#define exp0 x8
#define exp1 x9

#ifdef __AARCH64EB__
# define reslo x1
# define reshi x0
# define inlo  x3
# define inhi  x2
# define tmplo x7
# define tmphi x6
#else
# define reslo x0
# define reshi x1
# define inlo  x2
# define inhi  x3
# define tmplo x6
# define tmphi x7
#endif

#define RELAXED 0
#define CONSUME 1
#define ACQUIRE 2
#define RELEASE 3
#define ACQ_REL 4
#define SEQ_CST 5


ENTRY (libat_load_16_i1)
	cbnz	w1, 1f
	ldp	res0, res1, [x0]
	ret
1:
	cmp	w1, ACQUIRE
	b.hi	2f
	ldp	res0, res1, [x0]
	dmb	ishld
	ret
2:
	ldp	res0, res1, [x0]
	dmb	ish
	ret
END (libat_load_16_i1)


ENTRY (libat_store_16_i1)
	cbnz	w4, 1f
	stp	in0, in1, [x0]
	ret
1:
	dmb	ish
	stp	in0, in1, [x0]
	cmp	w4, SEQ_CST
	beq	2f
	ret
2:
	dmb	ish
	ret
END (libat_store_16_i1)


ENTRY (libat_exchange_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	stxp	w4, in0, in1, [x5]
	cbnz	w4, 1b
	ret
2:
	cmp	w4, ACQUIRE
	b.hi	4f
3:
	ldaxp	res0, res1, [x5]
	stxp	w4, in0, in1, [x5]
	cbnz	w4, 3b
	ret
4:
	cmp	w4, RELEASE
	b.ne	6f
5:
	ldxp	res0, res1, [x5]
	stlxp	w4, in0, in1, [x5]
	cbnz	w4, 5b
	ret
6:
	ldaxp	res0, res1, [x5]
	stlxp	w4, in0, in1, [x5]
	cbnz	w4, 6b
	ret
END (libat_exchange_16_i1)


ENTRY (libat_compare_exchange_16_i1)
	ldp	exp0, exp1, [x1]
	mov	tmp0, exp0
	mov	tmp1, exp1
	cbz	w4, 2f
	cmp	w4, RELEASE
	b.hs	3f
	caspa	exp0, exp1, in0, in1, [x0]
0:
	cmp	exp0, tmp0
	ccmp	exp1, tmp1, 0, eq
	bne	1f
	mov	x0, 1
	ret
1:
	stp	exp0, exp1, [x1]
	mov	x0, 0
	ret
2:
	casp	exp0, exp1, in0, in1, [x0]
	b	0b
3:
	b.hi	4f
	caspl	exp0, exp1, in0, in1, [x0]
	b	0b
4:
	caspal	exp0, exp1, in0, in1, [x0]
	b	0b
END (libat_compare_exchange_16_i1)


ENTRY (libat_fetch_add_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	adds	tmplo, reslo, inlo
	adc	tmphi, reshi, inhi
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	adds	tmplo, reslo, inlo
	adc	tmphi, reshi, inhi
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (libat_fetch_add_16_i1)


ENTRY (libat_add_fetch_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	adds	reslo, reslo, inlo
	adc	reshi, reshi, inhi
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	adds	reslo, reslo, inlo
	adc	reshi, reshi, inhi
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (libat_add_fetch_16_i1)


ENTRY (libat_fetch_sub_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	subs	tmplo, reslo, inlo
	sbc	tmphi, reshi, inhi
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	subs	tmplo, reslo, inlo
	sbc	tmphi, reshi, inhi
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (libat_fetch_sub_16_i1)


ENTRY (libat_sub_fetch_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	subs	reslo, reslo, inlo
	sbc	reshi, reshi, inhi
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	subs	reslo, reslo, inlo
	sbc	reshi, reshi, inhi
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (libat_sub_fetch_16_i1)


ENTRY (libat_fetch_or_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	orr	tmp0, res0, in0
	orr	tmp1, res1, in1
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	orr	tmp0, res0, in0
	orr	tmp1, res1, in1
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (libat_fetch_or_16_i1)


ENTRY (libat_or_fetch_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	orr	res0, res0, in0
	orr	res1, res1, in1
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	orr	res0, res0, in0
	orr	res1, res1, in1
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (libat_or_fetch_16_i1)


ENTRY (libat_fetch_and_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	and	tmp0, res0, in0
	and	tmp1, res1, in1
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	and	tmp0, res0, in0
	and	tmp1, res1, in1
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (libat_fetch_and_16_i1)


ENTRY (libat_and_fetch_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	and	res0, res0, in0
	and	res1, res1, in1
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	and	res0, res0, in0
	and	res1, res1, in1
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (libat_and_fetch_16_i1)


ENTRY (libat_fetch_xor_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	eor	tmp0, res0, in0
	eor	tmp1, res1, in1
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	eor	tmp0, res0, in0
	eor	tmp1, res1, in1
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (libat_fetch_xor_16_i1)


ENTRY (libat_xor_fetch_16_i1)
	mov	x5, x0
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	eor	res0, res0, in0
	eor	res1, res1, in1
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	eor	res0, res0, in0
	eor	res1, res1, in1
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (libat_xor_fetch_16_i1)


ENTRY (libat_fetch_nand_16_i1)
	mov	x5, x0
	mvn	in0, in0
	mvn	in1, in1
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	orn	tmp0, in0, res0
	orn	tmp1, in1, res1
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	orn	tmp0, in0, res0
	orn	tmp1, in1, res1
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (libat_fetch_nand_16_i1)


ENTRY (libat_nand_fetch_16_i1)
	mov	x5, x0
	mvn	in0, in0
	mvn	in1, in1
	cbnz	w4, 2f
1:
	ldxp	res0, res1, [x5]
	orn	res0, in0, res0
	orn	res1, in1, res1
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret
2:
	ldaxp	res0, res1, [x5]
	orn	res0, in0, res0
	orn	res1, in1, res1
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (libat_nand_fetch_16_i1)


ENTRY (libat_test_and_set_16_i1)
	mov	w2, 1
	cbnz	w1, 2f
	swpb	w0, w2, [x0]
	ret

2:	swpalb	w0, w2, [x0]
	ret
END (libat_test_and_set_16_i1)


/* GNU_PROPERTY_AARCH64_* macros from elf.h for use in asm code.  */
#define FEATURE_1_AND 0xc0000000
#define FEATURE_1_BTI 1
#define FEATURE_1_PAC 2

/* Supported features based on the code generation options.  */
#if defined(__ARM_FEATURE_BTI_DEFAULT)
# define BTI_FLAG FEATURE_1_BTI
#else
# define BTI_FLAG 0
#endif

#if __ARM_FEATURE_PAC_DEFAULT & 3
# define PAC_FLAG FEATURE_1_PAC
#else
# define PAC_FLAG 0
#endif

/* Add a NT_GNU_PROPERTY_TYPE_0 note.  */
#define GNU_PROPERTY(type, value)	\
  .section .note.gnu.property, "a";     \
  .p2align 3;				\
  .word 4;				\
  .word 16;				\
  .word 5;				\
  .asciz "GNU";				\
  .word type;				\
  .word 4;				\
  .word value;				\
  .word 0;

#if defined(__linux__) || defined(__FreeBSD__)
.section .note.GNU-stack, "", %progbits

/* Add GNU property note if built with branch protection.  */
# if (BTI_FLAG|PAC_FLAG) != 0
GNU_PROPERTY (FEATURE_1_AND, BTI_FLAG|PAC_FLAG)
# endif
#endif
