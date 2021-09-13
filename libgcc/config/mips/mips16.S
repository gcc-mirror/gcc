/* mips16 floating point support code
   Copyright (C) 1996-2021 Free Software Foundation, Inc.
   Contributed by Cygnus Support

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* An executable stack is *not* required for these functions.  */
#include "gnustack.h"

#include "auto-host.h"

#if defined(__mips_micromips) || defined(__mips_soft_float) \
    || __mips_isa_rev >= 6
  /* Do nothing because this code is only needed when linking
     against mips16 hard-float objects.  Neither micromips code
     nor soft-float nor MIPS R6 code can be linked against mips16
     hard-float objects so we do not need these routines when
     building libgcc for those cases.  */
#else

#if defined(HAVE_AS_MODULE)
#if __mips_fpr == 32
	.module fp=32
#elif __mips_fpr == 0
	.module fp=xx
#elif __mips_fpr == 64
	.module fp=64
#endif
#endif

/* This file contains mips16 floating point support functions.  These
   functions are called by mips16 code to handle floating point when
   -msoft-float is not used.  They accept the arguments and return
   values using the soft-float calling convention, but do the actual
   operation using the hard floating point instructions.  */

#if defined _MIPS_SIM && (_MIPS_SIM == _ABIO32 || _MIPS_SIM == _ABIO64)

/* This file contains 32-bit assembly code.  */
	.set nomips16

/* Start a function.  */

#define STARTFN(NAME) .globl NAME; .ent NAME; NAME:

/* Finish a function.  */

#define ENDFN(NAME) .end NAME

/* ARG1
	The FPR that holds the first floating-point argument.

   ARG2
	The FPR that holds the second floating-point argument.

   RET
	The FPR that holds a floating-point return value.  */

#define RET $f0
#define ARG1 $f12
#ifdef __mips64
#define ARG2 $f13
#else
#define ARG2 $f14
#endif

/* Set 64-bit register GPR so that its high 32 bits contain HIGH_FPR
   and so that its low 32 bits contain LOW_FPR.  */
#define MERGE_GPRf(GPR, HIGH_FPR, LOW_FPR)	\
	.set	noat;				\
	mfc1	$1, LOW_FPR;			\
	mfc1	GPR, HIGH_FPR;			\
	dsll	$1, $1, 32;			\
	dsll	GPR, GPR, 32;			\
	dsrl	$1, $1, 32;			\
	or	GPR, GPR, $1;			\
	.set	at

/* Move the high 32 bits of GPR to HIGH_FPR and the low 32 bits of
   GPR to LOW_FPR.  */
#define MERGE_GPRt(GPR, HIGH_FPR, LOW_FPR)	\
	.set	noat;				\
	dsrl	$1, GPR, 32;			\
	mtc1	GPR, LOW_FPR;			\
	mtc1	$1, HIGH_FPR;			\
	.set	at

/* Jump to T, and use "OPCODE, OP2" to implement a delayed move.  */
#define DELAYt(T, OPCODE, OP2)			\
	.set	noreorder;			\
	jr	T;				\
	OPCODE, OP2;				\
	.set	reorder

#if __mips >= 4
/* Coprocessor moves are interlocked from the MIPS IV ISA up.  */
#define DELAYf(T, OPCODE, OP2) DELAYt (T, OPCODE, OP2)
#else
/* Use "OPCODE. OP2" and jump to T.  */
#define DELAYf(T, OPCODE, OP2) OPCODE, OP2; jr T
#endif

/* MOVE_SF_BYTE0(D)
	Move the first single-precision floating-point argument between
	GPRs and FPRs.

   MOVE_SI_BYTE0(D)
	Likewise the first single-precision integer argument.

   MOVE_SF_BYTE4(D)
	Move the second single-precision floating-point argument between
	GPRs and FPRs, given that the first argument occupies 4 bytes.

   MOVE_SF_BYTE8(D)
	Move the second single-precision floating-point argument between
	GPRs and FPRs, given that the first argument occupies 8 bytes.

   MOVE_DF_BYTE0(D)
	Move the first double-precision floating-point argument between
	GPRs and FPRs.

   MOVE_DF_BYTE8(D)
	Likewise the second double-precision floating-point argument.

   MOVE_SF_RET(D, T)
	Likewise a single-precision floating-point return value,
	then jump to T.

   MOVE_SC_RET(D, T)
	Likewise a complex single-precision floating-point return value.

   MOVE_DF_RET(D, T)
	Likewise a double-precision floating-point return value.

   MOVE_DC_RET(D, T)
	Likewise a complex double-precision floating-point return value.

   MOVE_SI_RET(D, T)
	Likewise a single-precision integer return value.

   The D argument is "t" to move to FPRs and "f" to move from FPRs.
   The return macros may assume that the target of the jump does not
   use a floating-point register.  */

#define MOVE_SF_RET(D, T) DELAY##D (T, m##D##c1 $2,$f0)
#define MOVE_SI_RET(D, T) DELAY##D (T, m##D##c1 $2,$f0)

#if defined(__mips64) && defined(__MIPSEB__)
#define MOVE_SC_RET(D, T) MERGE_GPR##D ($2, $f0, $f1); jr T
#elif defined(__mips64)
/* The high 32 bits of $2 correspond to the second word in memory;
   i.e. the imaginary part.  */
#define MOVE_SC_RET(D, T) MERGE_GPR##D ($2, $f1, $f0); jr T
#else
#define MOVE_SC_RET(D, T) m##D##c1 $2,$f0; DELAY##D (T, m##D##c1 $3,$f2)
#endif

#if defined(__mips64)
#define MOVE_SF_BYTE0(D) m##D##c1 $4,$f12
#define MOVE_SF_BYTE4(D) m##D##c1 $5,$f13
#define MOVE_SF_BYTE8(D) m##D##c1 $5,$f13
#else
#define MOVE_SF_BYTE0(D) m##D##c1 $4,$f12
#define MOVE_SF_BYTE4(D) m##D##c1 $5,$f14
#define MOVE_SF_BYTE8(D) m##D##c1 $6,$f14
#endif
#define MOVE_SI_BYTE0(D) MOVE_SF_BYTE0(D)

#if defined(__mips64)
#define MOVE_DF_BYTE0(D) dm##D##c1 $4,$f12
#define MOVE_DF_BYTE8(D) dm##D##c1 $5,$f13
#define MOVE_DF_RET(D, T) DELAY##D (T, dm##D##c1 $2,$f0)
#define MOVE_DC_RET(D, T) dm##D##c1 $3,$f1; MOVE_DF_RET (D, T)
#elif __mips_fpr != 32 && __mips_isa_rev >= 2 && defined(__MIPSEB__)
#define MOVE_DF_BYTE0(D) m##D##c1 $5,$f12; m##D##hc1 $4,$f12
#define MOVE_DF_BYTE8(D) m##D##c1 $7,$f14; m##D##hc1 $6,$f14
#define MOVE_DF_RET(D, T) m##D##c1 $3,$f0; DELAY##D (T, m##D##hc1 $2,$f0)
#define MOVE_DC_RET(D, T) m##D##c1 $5,$f2; m##D##hc1 $4,$f2; MOVE_DF_RET (D, T)
#elif __mips_fpr != 32 && __mips_isa_rev >= 2
#define MOVE_DF_BYTE0(D) m##D##c1 $4,$f12; m##D##hc1 $5,$f12
#define MOVE_DF_BYTE8(D) m##D##c1 $6,$f14; m##D##hc1 $7,$f14
#define MOVE_DF_RET(D, T) m##D##c1 $2,$f0; DELAY##D (T, m##D##hc1 $3,$f0)
#define MOVE_DC_RET(D, T) m##D##c1 $4,$f2; m##D##hc1 $5,$f2; MOVE_DF_RET (D, T)
#elif __mips_fpr == 0
#define MOVE_DF_BYTE0t sw $4, 0($29); sw $5, 4($29); ldc1 $f12, 0($29)
#define MOVE_DF_BYTE0f sdc1 $f12, 0($29); lw $4, 0($29); lw $5, 4($29)
#define MOVE_DF_BYTE0(D) MOVE_DF_BYTE0##D
#define MOVE_DF_BYTE8t sw $6, 8($29); sw $7, 12($29); ldc1 $f14, 8($29)
#define MOVE_DF_BYTE8f sdc1 $f14, 8($29); lw $6, 8($29); lw $7, 12($29)
#define MOVE_DF_BYTE8(D) MOVE_DF_BYTE8##D
#define MOVE_DF_RETt(T) sw $2, 0($29); sw $3, 4($29); DELAYt (T, ldc1 $f0, 0($29))
#define MOVE_DF_RETf(T) sdc1 $f0, 0($29); lw $2, 0($29); DELAYf (T, lw $3, 4($29))
#define MOVE_DF_RET(D, T) MOVE_DF_RET##D(T)
#define MOVE_DC_RETt(T) sw $4, 8($29); sw $5, 12($29); ldc1 $f2, 8($29); MOVE_DF_RETt(T)
#define MOVE_DC_RETf(T) sdc1 $f2, 8($29); lw $4, 8($29); lw $5, 12($29); MOVE_DF_RETf(T)
#define MOVE_DC_RET(D, T) MOVE_DF_RET##D(T)
#elif defined(__MIPSEB__)
/* FPRs are little-endian.  */
#define MOVE_DF_BYTE0(D) m##D##c1 $4,$f13; m##D##c1 $5,$f12
#define MOVE_DF_BYTE8(D) m##D##c1 $6,$f15; m##D##c1 $7,$f14
#define MOVE_DF_RET(D, T) m##D##c1 $2,$f1; DELAY##D (T, m##D##c1 $3,$f0)
#define MOVE_DC_RET(D, T) m##D##c1 $4,$f3; m##D##c1 $5,$f2; MOVE_DF_RET (D, T)
#else
#define MOVE_DF_BYTE0(D) m##D##c1 $4,$f12; m##D##c1 $5,$f13
#define MOVE_DF_BYTE8(D) m##D##c1 $6,$f14; m##D##c1 $7,$f15
#define MOVE_DF_RET(D, T) m##D##c1 $2,$f0; DELAY##D (T, m##D##c1 $3,$f1)
#define MOVE_DC_RET(D, T) m##D##c1 $4,$f2; m##D##c1 $5,$f3; MOVE_DF_RET (D, T)
#endif

/* Single-precision math.  */

/* Define a function NAME that loads two single-precision values,
   performs FPU operation OPCODE on them, and returns the single-
   precision result.  */

#define OPSF3(NAME, OPCODE)	\
STARTFN (NAME);			\
	MOVE_SF_BYTE0 (t);	\
	MOVE_SF_BYTE4 (t);	\
	OPCODE	RET,ARG1,ARG2;	\
	MOVE_SF_RET (f, $31);	\
	ENDFN (NAME)

#ifdef L_m16addsf3
OPSF3 (__mips16_addsf3, add.s)
#endif
#ifdef L_m16subsf3
OPSF3 (__mips16_subsf3, sub.s)
#endif
#ifdef L_m16mulsf3
OPSF3 (__mips16_mulsf3, mul.s)
#endif
#ifdef L_m16divsf3
OPSF3 (__mips16_divsf3, div.s)
#endif

/* Define a function NAME that loads a single-precision value,
   performs FPU operation OPCODE on it, and returns the single-
   precision result.  */

#define OPSF2(NAME, OPCODE)	\
STARTFN (NAME);			\
	MOVE_SF_BYTE0 (t);	\
	OPCODE	RET,ARG1;	\
	MOVE_SF_RET (f, $31);	\
	ENDFN (NAME)

#ifdef L_m16negsf2
OPSF2 (__mips16_negsf2, neg.s)
#endif
#ifdef L_m16abssf2
OPSF2 (__mips16_abssf2, abs.s)
#endif

/* Single-precision comparisons.  */

/* Define a function NAME that loads two single-precision values,
   performs floating point comparison OPCODE, and returns TRUE or
   FALSE depending on the result.  */

#define CMPSF(NAME, OPCODE, TRUE, FALSE)	\
STARTFN (NAME);					\
	MOVE_SF_BYTE0 (t);			\
	MOVE_SF_BYTE4 (t);			\
	OPCODE	ARG1,ARG2;			\
	li	$2,TRUE;			\
	bc1t	1f;				\
	li	$2,FALSE;			\
1:;						\
	j	$31;				\
	ENDFN (NAME)

/* Like CMPSF, but reverse the comparison operands.  */

#define REVCMPSF(NAME, OPCODE, TRUE, FALSE)	\
STARTFN (NAME);					\
	MOVE_SF_BYTE0 (t);			\
	MOVE_SF_BYTE4 (t);			\
	OPCODE	ARG2,ARG1;			\
	li	$2,TRUE;			\
	bc1t	1f;				\
	li	$2,FALSE;			\
1:;						\
	j	$31;				\
	ENDFN (NAME)

#ifdef L_m16eqsf2
CMPSF (__mips16_eqsf2, c.eq.s, 0, 1)
#endif
#ifdef L_m16nesf2
CMPSF (__mips16_nesf2, c.eq.s, 0, 1)
#endif
#ifdef L_m16gtsf2
REVCMPSF (__mips16_gtsf2, c.lt.s, 1, 0)
#endif
#ifdef L_m16gesf2
REVCMPSF (__mips16_gesf2, c.le.s, 0, -1)
#endif
#ifdef L_m16lesf2
CMPSF (__mips16_lesf2, c.le.s, 0, 1)
#endif
#ifdef L_m16ltsf2
CMPSF (__mips16_ltsf2, c.lt.s, -1, 0)
#endif
#ifdef L_m16unordsf2
CMPSF(__mips16_unordsf2, c.un.s, 1, 0)
#endif


/* Single-precision conversions.  */

#ifdef L_m16fltsisf
STARTFN (__mips16_floatsisf)
	MOVE_SF_BYTE0 (t)
	cvt.s.w	RET,ARG1
	MOVE_SF_RET (f, $31)
	ENDFN (__mips16_floatsisf)
#endif

#ifdef L_m16fltunsisf
STARTFN (__mips16_floatunsisf)
	.set	noreorder
	bltz	$4,1f
	MOVE_SF_BYTE0 (t)
	.set	reorder
	cvt.s.w	RET,ARG1
	MOVE_SF_RET (f, $31)
1:		
	and	$2,$4,1
	srl	$3,$4,1
	or	$2,$2,$3
	mtc1	$2,RET
	cvt.s.w	RET,RET
	add.s	RET,RET,RET
	MOVE_SF_RET (f, $31)
	ENDFN (__mips16_floatunsisf)
#endif
	
#ifdef L_m16fix_truncsfsi
STARTFN (__mips16_fix_truncsfsi)
	MOVE_SF_BYTE0 (t)
	trunc.w.s RET,ARG1,$4
	MOVE_SI_RET (f, $31)
	ENDFN (__mips16_fix_truncsfsi)
#endif

#if !defined(__mips_single_float) && !defined(__SINGLE_FLOAT)

/* Double-precision math.  */

/* Define a function NAME that loads two double-precision values,
   performs FPU operation OPCODE on them, and returns the double-
   precision result.  */

#define OPDF3(NAME, OPCODE)	\
STARTFN (NAME);			\
	MOVE_DF_BYTE0 (t);	\
	MOVE_DF_BYTE8 (t);	\
	OPCODE RET,ARG1,ARG2;	\
	MOVE_DF_RET (f, $31);	\
	ENDFN (NAME)

#ifdef L_m16adddf3
OPDF3 (__mips16_adddf3, add.d)
#endif
#ifdef L_m16subdf3
OPDF3 (__mips16_subdf3, sub.d)
#endif
#ifdef L_m16muldf3
OPDF3 (__mips16_muldf3, mul.d)
#endif
#ifdef L_m16divdf3
OPDF3 (__mips16_divdf3, div.d)
#endif

/* Define a function NAME that loads a double-precision value,
   performs FPU operation OPCODE on it, and returns the double-
   precision result.  */

#define OPDF2(NAME, OPCODE)	\
STARTFN (NAME);			\
	MOVE_DF_BYTE0 (t);	\
	OPCODE RET,ARG1;	\
	MOVE_DF_RET (f, $31);	\
	ENDFN (NAME)

#ifdef L_m16negdf2
OPDF2 (__mips16_negdf2, neg.d)
#endif
#ifdef L_m16absdf2
OPDF2 (__mips16_absdf2, abs.d)
#endif

/* Conversions between single and double precision.  */

#ifdef L_m16extsfdf2
STARTFN (__mips16_extendsfdf2)
	MOVE_SF_BYTE0 (t)
	cvt.d.s	RET,ARG1
	MOVE_DF_RET (f, $31)
	ENDFN (__mips16_extendsfdf2)
#endif

#ifdef L_m16trdfsf2
STARTFN (__mips16_truncdfsf2)
	MOVE_DF_BYTE0 (t)
	cvt.s.d	RET,ARG1
	MOVE_SF_RET (f, $31)
	ENDFN (__mips16_truncdfsf2)
#endif

/* Double-precision comparisons.  */

/* Define a function NAME that loads two double-precision values,
   performs floating point comparison OPCODE, and returns TRUE or
   FALSE depending on the result.  */

#define CMPDF(NAME, OPCODE, TRUE, FALSE)	\
STARTFN (NAME);					\
	MOVE_DF_BYTE0 (t);			\
	MOVE_DF_BYTE8 (t);			\
	OPCODE	ARG1,ARG2;			\
	li	$2,TRUE;			\
	bc1t	1f;				\
	li	$2,FALSE;			\
1:;						\
	j	$31;				\
	ENDFN (NAME)

/* Like CMPDF, but reverse the comparison operands.  */

#define REVCMPDF(NAME, OPCODE, TRUE, FALSE)	\
STARTFN (NAME);					\
	MOVE_DF_BYTE0 (t);			\
	MOVE_DF_BYTE8 (t);			\
	OPCODE	ARG2,ARG1;			\
	li	$2,TRUE;			\
	bc1t	1f;				\
	li	$2,FALSE;			\
1:;						\
	j	$31;				\
	ENDFN (NAME)

#ifdef L_m16eqdf2
CMPDF (__mips16_eqdf2, c.eq.d, 0, 1)
#endif
#ifdef L_m16nedf2
CMPDF (__mips16_nedf2, c.eq.d, 0, 1)
#endif
#ifdef L_m16gtdf2
REVCMPDF (__mips16_gtdf2, c.lt.d, 1, 0)
#endif
#ifdef L_m16gedf2
REVCMPDF (__mips16_gedf2, c.le.d, 0, -1)
#endif
#ifdef L_m16ledf2
CMPDF (__mips16_ledf2, c.le.d, 0, 1)
#endif
#ifdef L_m16ltdf2
CMPDF (__mips16_ltdf2, c.lt.d, -1, 0)
#endif
#ifdef L_m16unorddf2
CMPDF(__mips16_unorddf2, c.un.d, 1, 0)
#endif

/* Double-precision conversions.  */

#ifdef L_m16fltsidf
STARTFN (__mips16_floatsidf)
	MOVE_SI_BYTE0 (t)
	cvt.d.w	RET,ARG1
	MOVE_DF_RET (f, $31)
	ENDFN (__mips16_floatsidf)
#endif
	
#ifdef L_m16fltunsidf
STARTFN (__mips16_floatunsidf)
	MOVE_SI_BYTE0 (t)
	cvt.d.w RET,ARG1
	bgez	$4,1f
	li.d	ARG1, 4.294967296e+9
	add.d	RET, RET, ARG1
1:	MOVE_DF_RET (f, $31)
	ENDFN (__mips16_floatunsidf)
#endif
	
#ifdef L_m16fix_truncdfsi
STARTFN (__mips16_fix_truncdfsi)
	MOVE_DF_BYTE0 (t)
	trunc.w.d RET,ARG1,$4
	MOVE_SI_RET (f, $31)
	ENDFN (__mips16_fix_truncdfsi)
#endif
#endif /* !__mips_single_float */

/* We don't export stubs from libgcc_s.so and always require static
   versions to be pulled from libgcc.a as needed because they use $2
   and possibly $3 as arguments, diverging from the standard SysV ABI,
   and as such would require severe pessimisation of MIPS16 PLT entries
   just for this single special case.

   For compatibility with old binaries that used safe standard MIPS PLT
   entries and referred to these functions we still export them at
   version GCC_4.4.0 for run-time loading only.  */

#ifdef SHARED
#define CE_STARTFN(NAME)			\
STARTFN (NAME##_compat);			\
	.symver NAME##_compat, NAME@GCC_4.4.0
#define CE_ENDFN(NAME) ENDFN (NAME##_compat)
#else
#define CE_STARTFN(NAME)			\
STARTFN (NAME);					\
	.hidden NAME
#define CE_ENDFN(NAME) ENDFN (NAME)
#endif

/* Define a function NAME that moves a return value of mode MODE from
   FPRs to GPRs.  */

#define RET_FUNCTION(NAME, MODE)	\
CE_STARTFN (NAME);			\
	MOVE_##MODE##_RET (t, $31);	\
	CE_ENDFN (NAME)

#ifdef L_m16retsf
RET_FUNCTION (__mips16_ret_sf, SF)
#endif

#ifdef L_m16retsc
RET_FUNCTION (__mips16_ret_sc, SC)
#endif

#if !defined(__mips_single_float) && !defined(__SINGLE_FLOAT)
#ifdef L_m16retdf
RET_FUNCTION (__mips16_ret_df, DF)
#endif

#ifdef L_m16retdc
RET_FUNCTION (__mips16_ret_dc, DC)
#endif
#endif /* !__mips_single_float */

/* STUB_ARGS_X copies the arguments from GPRs to FPRs for argument
   code X.  X is calculated as ARG1 + ARG2 * 4, where ARG1 and ARG2
   classify the first and second arguments as follows:

	1: a single-precision argument
	2: a double-precision argument
	0: no argument, or not one of the above.  */

#define STUB_ARGS_0						/* () */
#define STUB_ARGS_1 MOVE_SF_BYTE0 (t)				/* (sf) */
#define STUB_ARGS_5 MOVE_SF_BYTE0 (t); MOVE_SF_BYTE4 (t)	/* (sf, sf) */
#define STUB_ARGS_9 MOVE_SF_BYTE0 (t); MOVE_DF_BYTE8 (t)	/* (sf, df) */
#define STUB_ARGS_2 MOVE_DF_BYTE0 (t)				/* (df) */
#define STUB_ARGS_6 MOVE_DF_BYTE0 (t); MOVE_SF_BYTE8 (t)	/* (df, sf) */
#define STUB_ARGS_10 MOVE_DF_BYTE0 (t); MOVE_DF_BYTE8 (t)	/* (df, df) */

/* These functions are used by 16-bit code when calling via a function
   pointer.  They must copy the floating point arguments from the GPRs
   to FPRs and then call function $2.  */

#define CALL_STUB_NO_RET(NAME, CODE)	\
CE_STARTFN (NAME);			\
	STUB_ARGS_##CODE;		\
	.set	noreorder;		\
	jr	$2;			\
	move	$25,$2;			\
	.set	reorder;		\
	CE_ENDFN (NAME)

#ifdef L_m16stub1
CALL_STUB_NO_RET (__mips16_call_stub_1, 1)
#endif

#ifdef L_m16stub5
CALL_STUB_NO_RET (__mips16_call_stub_5, 5)
#endif

#if !defined(__mips_single_float) && !defined(__SINGLE_FLOAT)

#ifdef L_m16stub2
CALL_STUB_NO_RET (__mips16_call_stub_2, 2)
#endif

#ifdef L_m16stub6
CALL_STUB_NO_RET (__mips16_call_stub_6, 6)
#endif

#ifdef L_m16stub9
CALL_STUB_NO_RET (__mips16_call_stub_9, 9)
#endif

#ifdef L_m16stub10
CALL_STUB_NO_RET (__mips16_call_stub_10, 10)
#endif
#endif /* !__mips_single_float */

/* Now we have the same set of functions, except that this time the
   function being called returns an SFmode, SCmode, DFmode or DCmode
   value; we need to instantiate a set for each case.  The calling
   function will arrange to preserve $18, so these functions are free
   to use it to hold the return address.

   Note that we do not know whether the function we are calling is 16
   bit or 32 bit.  However, it does not matter, because 16-bit
   functions always return floating point values in both the gp and
   the fp regs.  It would be possible to check whether the function
   being called is 16 bits, in which case the copy is unnecessary;
   however, it's faster to always do the copy.  */

#define CALL_STUB_RET(NAME, CODE, MODE)					\
CE_STARTFN (NAME);							\
	.cfi_startproc;							\
	/* Create a fake CFA 4 bytes below the stack pointer.  */	\
	.cfi_def_cfa 29,-4;						\
	/* "Save" $sp in itself so we don't use the fake CFA.		\
	   This is: DW_CFA_val_expression r29, { DW_OP_reg29 }.  */	\
	.cfi_escape 0x16,29,1,0x6d;					\
	move	$18,$31;						\
	.cfi_register 31,18;						\
	STUB_ARGS_##CODE;						\
	.set	noreorder;						\
	jalr	$2;							\
	move	$25,$2;							\
	.set	reorder;						\
	MOVE_##MODE##_RET (f, $18);					\
	.cfi_endproc;							\
	CE_ENDFN (NAME)

/* First, instantiate the single-float set.  */

#ifdef L_m16stubsf0
CALL_STUB_RET (__mips16_call_stub_sf_0, 0, SF)
#endif

#ifdef L_m16stubsf1
CALL_STUB_RET (__mips16_call_stub_sf_1, 1, SF)
#endif

#ifdef L_m16stubsf5
CALL_STUB_RET (__mips16_call_stub_sf_5, 5, SF)
#endif

#if !defined(__mips_single_float) && !defined(__SINGLE_FLOAT)
#ifdef L_m16stubsf2
CALL_STUB_RET (__mips16_call_stub_sf_2, 2, SF)
#endif

#ifdef L_m16stubsf6
CALL_STUB_RET (__mips16_call_stub_sf_6, 6, SF)
#endif

#ifdef L_m16stubsf9
CALL_STUB_RET (__mips16_call_stub_sf_9, 9, SF)
#endif

#ifdef L_m16stubsf10
CALL_STUB_RET (__mips16_call_stub_sf_10, 10, SF)
#endif
#endif /* !__mips_single_float */


/* Now we have the same set of functions again, except that this time
   the function being called returns an DFmode value.  */

#if !defined(__mips_single_float) && !defined(__SINGLE_FLOAT)
#ifdef L_m16stubdf0
CALL_STUB_RET (__mips16_call_stub_df_0, 0, DF)
#endif

#ifdef L_m16stubdf1
CALL_STUB_RET (__mips16_call_stub_df_1, 1, DF)
#endif

#ifdef L_m16stubdf5
CALL_STUB_RET (__mips16_call_stub_df_5, 5, DF)
#endif

#ifdef L_m16stubdf2
CALL_STUB_RET (__mips16_call_stub_df_2, 2, DF)
#endif

#ifdef L_m16stubdf6
CALL_STUB_RET (__mips16_call_stub_df_6, 6, DF)
#endif

#ifdef L_m16stubdf9
CALL_STUB_RET (__mips16_call_stub_df_9, 9, DF)
#endif

#ifdef L_m16stubdf10
CALL_STUB_RET (__mips16_call_stub_df_10, 10, DF)
#endif
#endif /* !__mips_single_float */


/* Ho hum.  Here we have the same set of functions again, this time
   for when the function being called returns an SCmode value.  */

#ifdef L_m16stubsc0
CALL_STUB_RET (__mips16_call_stub_sc_0, 0, SC)
#endif

#ifdef L_m16stubsc1
CALL_STUB_RET (__mips16_call_stub_sc_1, 1, SC)
#endif

#ifdef L_m16stubsc5
CALL_STUB_RET (__mips16_call_stub_sc_5, 5, SC)
#endif

#if !defined(__mips_single_float) && !defined(__SINGLE_FLOAT)
#ifdef L_m16stubsc2
CALL_STUB_RET (__mips16_call_stub_sc_2, 2, SC)
#endif

#ifdef L_m16stubsc6
CALL_STUB_RET (__mips16_call_stub_sc_6, 6, SC)
#endif

#ifdef L_m16stubsc9
CALL_STUB_RET (__mips16_call_stub_sc_9, 9, SC)
#endif

#ifdef L_m16stubsc10
CALL_STUB_RET (__mips16_call_stub_sc_10, 10, SC)
#endif
#endif /* !__mips_single_float */


/* Finally, another set of functions for DCmode.  */

#if !defined(__mips_single_float) && !defined(__SINGLE_FLOAT)
#ifdef L_m16stubdc0
CALL_STUB_RET (__mips16_call_stub_dc_0, 0, DC)
#endif

#ifdef L_m16stubdc1
CALL_STUB_RET (__mips16_call_stub_dc_1, 1, DC)
#endif

#ifdef L_m16stubdc5
CALL_STUB_RET (__mips16_call_stub_dc_5, 5, DC)
#endif

#ifdef L_m16stubdc2
CALL_STUB_RET (__mips16_call_stub_dc_2, 2, DC)
#endif

#ifdef L_m16stubdc6
CALL_STUB_RET (__mips16_call_stub_dc_6, 6, DC)
#endif

#ifdef L_m16stubdc9
CALL_STUB_RET (__mips16_call_stub_dc_9, 9, DC)
#endif

#ifdef L_m16stubdc10
CALL_STUB_RET (__mips16_call_stub_dc_10, 10, DC)
#endif
#endif /* !__mips_single_float */

#endif
#endif /* defined(__mips_micromips) || defined(__mips_soft_float) */
