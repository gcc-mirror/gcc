/* Copyright (C) 2022-2025 Free Software Foundation, Inc.

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


/* AArch64 128-bit lock-free atomic implementation.

   128-bit atomics are now lock-free for all AArch64 architecture versions.
   This is backwards compatible with existing binaries (as we swap all uses
   of 128-bit atomics via an ifunc) and gives better performance than locking
   atomics.

   128-bit atomic loads use a exclusive loop if LSE2 is not supported.
   This results in an implicit store which is invisible to software as long
   as the given address is writeable.  Since all other atomics have explicit
   writes, this will be true when using atomics in actual code.

   The libat_<op>_16 entry points are ARMv8.0.
   The libat_<op>_16_i1 entry points are used when LSE128 or LRCPC3 is available.
   The libat_<op>_16_i2 entry points are used when LSE2 is available.  */

#include "auto-config.h"

	.arch	armv8-a+lse

/* There is overlap in atomic instructions implemented in RCPC3 and LSE2.
   Consequently, both _i1 and _i2 suffixes are needed for functions using these.
   Elsewhere, all extension-specific implementations are mapped to _i1.  */

#define LRCPC3(NAME)	libat_##NAME##_i1
#define LSE128(NAME)	libat_##NAME##_i1
#define LSE(NAME)	libat_##NAME##_i1
#define LSE2(NAME)	libat_##NAME##_i2
#define CORE(NAME)	libat_##NAME
#define ATOMIC(NAME)	__atomic_##NAME

/* Emit __atomic_* entrypoints if no ifuncs.  */
#define ENTRY_ALIASED(NAME)	ENTRY2 (CORE (NAME), ALIAS (NAME, ATOMIC, CORE))

#if HAVE_IFUNC
# define ENTRY(NAME)		ENTRY2 (CORE (NAME), )
# define ENTRY_FEAT(NAME, FEAT) ENTRY2 (FEAT (NAME), )
# define END_FEAT(NAME, FEAT)	END2 (FEAT (NAME))
#else
# define ENTRY(NAME)	ENTRY_ALIASED (NAME)
#endif

#define END(NAME)		END2 (CORE (NAME))

#define ENTRY2(NAME, ALIASES)	\
	.global NAME;		\
	.hidden NAME;		\
	.type NAME,%function;	\
	.p2align 4;		\
	ALIASES;		\
NAME:				\
	.cfi_startproc;		\
	hint	34;	// bti c

#define END2(NAME)		\
	.cfi_endproc;		\
	.size NAME, .-NAME;

#define ALIAS(NAME, FROM, TO)	ALIAS1 (FROM (NAME), TO (NAME))

#define ALIAS1(ALIAS, NAME)	\
	.global ALIAS;		\
	.set ALIAS, NAME;

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


/* Core implementations: Not dependent on the presence of further architectural
   extensions.  */

ENTRY (load_16)
	mov	x5, x0
	cbnz	w1, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (load_16)


ENTRY (store_16)
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	xzr, tmp0, [x0]
	stxp	w4, in0, in1, [x0]
	cbnz	w4, 1b
	ret

	/* RELEASE/SEQ_CST.  */
2:	ldxp	xzr, tmp0, [x0]
	stlxp	w4, in0, in1, [x0]
	cbnz	w4, 2b
	ret
END (store_16)


ENTRY (exchange_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	stxp	w4, in0, in1, [x5]
	cbnz	w4, 1b
	ret
2:
	cmp	w4, ACQUIRE
	b.hi	4f

	/* ACQUIRE/CONSUME.  */
3:	ldaxp	res0, res1, [x5]
	stxp	w4, in0, in1, [x5]
	cbnz	w4, 3b
	ret

	/* RELEASE/ACQ_REL/SEQ_CST.  */
4:	ldaxp	res0, res1, [x5]
	stlxp	w4, in0, in1, [x5]
	cbnz	w4, 4b
	ret
END (exchange_16)


ENTRY (compare_exchange_16)
	ldp	exp0, exp1, [x1]
	cbz	w4, 3f
	cmp	w4, RELEASE
	b.hs	5f

	/* ACQUIRE/CONSUME.  */
1:	ldaxp	tmp0, tmp1, [x0]
	cmp	tmp0, exp0
	ccmp	tmp1, exp1, 0, eq
	csel	tmp0, in0, tmp0, eq
	csel	tmp1, in1, tmp1, eq
	stxp	w4, tmp0, tmp1, [x0]
	cbnz	w4, 1b
	beq	2f
	stp     tmp0, tmp1, [x1]
2:	cset	x0, eq
	ret

	/* RELAXED.  */
3:	ldxp	tmp0, tmp1, [x0]
	cmp	tmp0, exp0
	ccmp	tmp1, exp1, 0, eq
	csel	tmp0, in0, tmp0, eq
	csel	tmp1, in1, tmp1, eq
	stxp	w4, tmp0, tmp1, [x0]
	cbnz	w4, 3b
	beq	4f
	stp	tmp0, tmp1, [x1]
4:	cset	x0, eq
	ret

	/* RELEASE/ACQ_REL/SEQ_CST.  */
5:	ldaxp	tmp0, tmp1, [x0]
	cmp	tmp0, exp0
	ccmp	tmp1, exp1, 0, eq
	csel	tmp0, in0, tmp0, eq
	csel	tmp1, in1, tmp1, eq
	stlxp	w4, tmp0, tmp1, [x0]
	cbnz	w4, 5b
	beq	6f
	stp	tmp0, tmp1, [x1]
6:	cset	x0, eq
	ret
END (compare_exchange_16)




ENTRY_ALIASED (fetch_add_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	adds	tmplo, reslo, inlo
	adc	tmphi, reshi, inhi
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	adds	tmplo, reslo, inlo
	adc	tmphi, reshi, inhi
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (fetch_add_16)


ENTRY_ALIASED (add_fetch_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	adds	reslo, reslo, inlo
	adc	reshi, reshi, inhi
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	adds	reslo, reslo, inlo
	adc	reshi, reshi, inhi
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (add_fetch_16)


ENTRY_ALIASED (fetch_sub_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	subs	tmplo, reslo, inlo
	sbc	tmphi, reshi, inhi
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	subs	tmplo, reslo, inlo
	sbc	tmphi, reshi, inhi
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (fetch_sub_16)


ENTRY_ALIASED (sub_fetch_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	subs	reslo, reslo, inlo
	sbc	reshi, reshi, inhi
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	subs	reslo, reslo, inlo
	sbc	reshi, reshi, inhi
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (sub_fetch_16)


ENTRY (fetch_or_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	orr	tmp0, res0, in0
	orr	tmp1, res1, in1
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	orr	tmp0, res0, in0
	orr	tmp1, res1, in1
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (fetch_or_16)


ENTRY (or_fetch_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	orr	res0, res0, in0
	orr	res1, res1, in1
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	orr	res0, res0, in0
	orr	res1, res1, in1
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (or_fetch_16)


ENTRY (fetch_and_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	and	tmp0, res0, in0
	and	tmp1, res1, in1
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	and	tmp0, res0, in0
	and	tmp1, res1, in1
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (fetch_and_16)


ENTRY (and_fetch_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	and	res0, res0, in0
	and	res1, res1, in1
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	and	res0, res0, in0
	and	res1, res1, in1
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (and_fetch_16)


ENTRY_ALIASED (fetch_xor_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	eor	tmp0, res0, in0
	eor	tmp1, res1, in1
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	eor	tmp0, res0, in0
	eor	tmp1, res1, in1
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (fetch_xor_16)


ENTRY_ALIASED (xor_fetch_16)
	mov	x5, x0
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	eor	res0, res0, in0
	eor	res1, res1, in1
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	eor	res0, res0, in0
	eor	res1, res1, in1
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (xor_fetch_16)


ENTRY_ALIASED (fetch_nand_16)
	mov	x5, x0
	mvn	in0, in0
	mvn	in1, in1
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	orn	tmp0, in0, res0
	orn	tmp1, in1, res1
	stxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	orn	tmp0, in0, res0
	orn	tmp1, in1, res1
	stlxp	w4, tmp0, tmp1, [x5]
	cbnz	w4, 2b
	ret
END (fetch_nand_16)


ENTRY_ALIASED (nand_fetch_16)
	mov	x5, x0
	mvn	in0, in0
	mvn	in1, in1
	cbnz	w4, 2f

	/* RELAXED.  */
1:	ldxp	res0, res1, [x5]
	orn	res0, in0, res0
	orn	res1, in1, res1
	stxp	w4, res0, res1, [x5]
	cbnz	w4, 1b
	ret

	/* ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
2:	ldaxp	res0, res1, [x5]
	orn	res0, in0, res0
	orn	res1, in1, res1
	stlxp	w4, res0, res1, [x5]
	cbnz	w4, 2b
	ret
END (nand_fetch_16)


/* __atomic_test_and_set is always inlined, so this entry is unused and
   only required for completeness.  */
ENTRY_ALIASED (test_and_set_16)

	/* RELAXED/ACQUIRE/CONSUME/RELEASE/ACQ_REL/SEQ_CST.  */
	mov	x5, x0
1:	ldaxrb	w0, [x5]
	stlxrb	w4, w2, [x5]
	cbnz	w4, 1b
	ret
END (test_and_set_16)


#if HAVE_IFUNC
/* ifunc implementations: Carries run-time dependence on the presence of further
   architectural extensions.  */

ENTRY_FEAT (load_16, LRCPC3)
	cbnz	w1, 1f

	/* RELAXED.  */
	ldp	res0, res1, [x0]
	ret
1:
	cmp	w1, SEQ_CST
	b.eq	2f

	/* ACQUIRE/CONSUME (Load-AcquirePC semantics).  */
	/* ldiapp res0, res1, [x0]  */
	.inst	0xd9411800
	ret

	/* SEQ_CST.  */
2:	ldar	tmp0, [x0]	/* Block reordering with Store-Release instr.  */
	/* ldiapp res0, res1, [x0]  */
	.inst	0xd9411800
	ret
END_FEAT (load_16, LRCPC3)


ENTRY_FEAT (store_16, LRCPC3)
	cbnz	w4, 1f

	/* RELAXED.  */
	stp	in0, in1, [x0]
	ret

	/* RELEASE/SEQ_CST.  */
1:	/* stilp in0, in1, [x0]  */
	.inst	0xd9031802
	ret
END_FEAT (store_16, LRCPC3)


ENTRY_FEAT (exchange_16, LSE128)
	mov	tmp0, x0
	mov	res0, in0
	mov	res1, in1
	cbnz	w4, 1f

	/* RELAXED.  */
	/* swpp res0, res1, [tmp0]  */
	.inst	0x192180c0
	ret
1:
	cmp	w4, ACQUIRE
	b.hi	2f

	/* ACQUIRE/CONSUME.  */
	/* swppa res0, res1, [tmp0]  */
	.inst	0x19a180c0
	ret

	/* RELEASE/ACQ_REL/SEQ_CST.  */
2:	/* swppal res0, res1, [tmp0]  */
	.inst	0x19e180c0
	ret
END_FEAT (exchange_16, LSE128)


ENTRY_FEAT (fetch_or_16, LSE128)
	mov	tmp0, x0
	mov	res0, in0
	mov	res1, in1
	cbnz	w4, 1f

	/* RELAXED.  */
	/* ldsetp res0, res1, [tmp0]  */
	.inst	0x192130c0
	ret
1:
	cmp	w4, ACQUIRE
	b.hi	2f

	/* ACQUIRE/CONSUME.  */
	/* ldsetpa res0, res1, [tmp0]  */
	.inst	0x19a130c0
	ret

	/* RELEASE/ACQ_REL/SEQ_CST.  */
2:	/* ldsetpal res0, res1, [tmp0]  */
	.inst	0x19e130c0
	ret
END_FEAT (fetch_or_16, LSE128)


ENTRY_FEAT (or_fetch_16, LSE128)
	cbnz	w4, 1f
	mov	tmp0, in0
	mov	tmp1, in1

	/* RELAXED.  */
	/* ldsetp in0, in1, [x0]  */
	.inst	0x19233002
	orr	res0, in0, tmp0
	orr	res1, in1, tmp1
	ret
1:
	cmp	w4, ACQUIRE
	b.hi	2f

	/* ACQUIRE/CONSUME.  */
	/* ldsetpa in0, in1, [x0]  */
	.inst	0x19a33002
	orr	res0, in0, tmp0
	orr	res1, in1, tmp1
	ret

	/* RELEASE/ACQ_REL/SEQ_CST.  */
2:	/* ldsetpal in0, in1, [x0]  */
	.inst	0x19e33002
	orr	res0, in0, tmp0
	orr	res1, in1, tmp1
	ret
END_FEAT (or_fetch_16, LSE128)


ENTRY_FEAT (fetch_and_16, LSE128)
	mov	tmp0, x0
	mvn	res0, in0
	mvn	res1, in1
	cbnz	w4, 1f

	/* RELAXED.  */
	/* ldclrp res0, res1, [tmp0]  */
	.inst	0x192110c0
	ret

1:
	cmp	w4, ACQUIRE
	b.hi	2f

	/* ACQUIRE/CONSUME.  */
	/* ldclrpa res0, res1, [tmp0]  */
	.inst	0x19a110c0
	ret

	/* RELEASE/ACQ_REL/SEQ_CST.  */
2:	/* ldclrpal res0, res1, [tmp0]  */
	.inst	0x19e110c0
	ret
END_FEAT (fetch_and_16, LSE128)


ENTRY_FEAT (and_fetch_16, LSE128)
	mvn	tmp0, in0
	mvn	tmp0, in1
	cbnz	w4, 1f

	/* RELAXED.  */
	/* ldclrp tmp0, tmp1, [x0]  */
	.inst	0x19271006
	and	res0, tmp0, in0
	and	res1, tmp1, in1
	ret

1:
	cmp	w4, ACQUIRE
	b.hi	2f

	/* ACQUIRE/CONSUME.  */
	/* ldclrpa tmp0, tmp1, [x0]  */
	.inst	0x19a71006
	and	res0, tmp0, in0
	and	res1, tmp1, in1
	ret

	/* RELEASE/ACQ_REL/SEQ_CST.  */
2:	/* ldclrpal	tmp0, tmp1, [x5]  */
	.inst	0x19e710a6
	and	res0, tmp0, in0
	and	res1, tmp1, in1
	ret
END_FEAT (and_fetch_16, LSE128)


ENTRY_FEAT (load_16, LSE2)
	cbnz	w1, 1f

	/* RELAXED.  */
	ldp	res0, res1, [x0]
	ret
1:
	cmp	w1, SEQ_CST
	b.eq	2f

	/* ACQUIRE/CONSUME (Load-AcquirePC semantics).  */
	ldp	res0, res1, [x0]
	dmb	ishld
	ret

	/* SEQ_CST.  */
2:	ldar	tmp0, [x0]	/* Block reordering with Store-Release instr.  */
	ldp	res0, res1, [x0]
	dmb	ishld
	ret
END_FEAT (load_16, LSE2)


ENTRY_FEAT (store_16, LSE2)
	cbnz	w4, 1f

	/* RELAXED.  */
	stp	in0, in1, [x0]
	ret

	/* RELEASE/SEQ_CST.  */
1:	ldxp	xzr, tmp0, [x0]
	stlxp	w4, in0, in1, [x0]
	cbnz	w4, 1b
	ret
END_FEAT (store_16, LSE2)


ENTRY_FEAT (compare_exchange_16, LSE)
	ldp	exp0, exp1, [x1]
	mov	tmp0, exp0
	mov	tmp1, exp1
	cbz	w4, 2f
	cmp	w4, RELEASE
	b.hs	3f

	/* ACQUIRE/CONSUME.  */
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

	/* RELAXED.  */
2:	casp	exp0, exp1, in0, in1, [x0]
	b	0b

	/* RELEASE.  */
3:	b.hi	4f
	caspl	exp0, exp1, in0, in1, [x0]
	b	0b

	/* ACQ_REL/SEQ_CST.  */
4:	caspal	exp0, exp1, in0, in1, [x0]
	b	0b
END_FEAT (compare_exchange_16, LSE)
#endif

/* GNU_PROPERTY_AARCH64_* macros from elf.h for use in asm code.  */
#define FEATURE_1_AND 0xc0000000
#define FEATURE_1_BTI 1
#define FEATURE_1_PAC 2
#define FEATURE_1_GCS 4

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

#if __ARM_FEATURE_GCS_DEFAULT
# define GCS_FLAG FEATURE_1_GCS
#else
# define GCS_FLAG 0
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
# if (BTI_FLAG|PAC_FLAG|GCS_FLAG) != 0
GNU_PROPERTY (FEATURE_1_AND, BTI_FLAG|PAC_FLAG|GCS_FLAG)
# endif
#endif
