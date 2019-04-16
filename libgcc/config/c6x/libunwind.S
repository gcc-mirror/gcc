.text
.macro do_call fn
#ifdef _TMS320C6400_PLUS
	callp	.s2	(\fn), B3
#elif defined(_TMS320C6400)
	call	.s2	(\fn)
	addkpc	.s2	9f, B3, 0
	nop		4
9f:
#else
	call	.s2	(\fn)
	mhkl	.s2	9f, B3
	mhkh	.s2	9f, B3
	nop		3
9f:
#endif
.endm
.align 2
.global restore_core_regs
.type restore_core_regs, STT_FUNC
restore_core_regs:
	mv	.s2x	A4, B4
	ldw	.d1t1	*+A4[0], A0
	|| ldw	.d2t2	*++B4[16], B0
	ldw	.d1t1	*+A4[1], A1
	|| ldw	.d2t2	*+B4[1], B1
	ldw	.d1t1	*+A4[2], A2
	|| ldw	.d2t2	*+B4[2], B2
	ldw	.d1t1	*+A4[3], A3
	|| ldw	.d2t2	*+B4[3], B3
	;; Base registers are loaded later
	ldw	.d1t1	*+A4[5], A5
	|| ldw	.d2t2	*+B4[5], B5
	ldw	.d1t1	*+A4[6], A6
	|| ldw	.d2t2	*+B4[6], B6
	ldw	.d1t1	*+A4[7], A7
	|| ldw	.d2t2	*+B4[7], B7
	ldw	.d1t1	*+A4[8], A8
	|| ldw	.d2t2	*+B4[8], B8
	ldw	.d1t1	*+A4[9], A9
	|| ldw	.d2t2	*+B4[9], B9
	;; load PC into B10 so that it is ready for the branch
	ldw	.d2t2	*+B4[16], B10
	ldw	.d1t1	*+A4[11], A11
	|| ldw	.d2t2	*+B4[11], B11
	ldw	.d1t1	*+A4[12], A12
	|| ldw	.d2t2	*+B4[12], B12
	ldw	.d1t1	*+A4[13], A13
	|| ldw	.d2t2	*+B4[13], B13
	ldw	.d1t1	*+A4[14], A14
	|| ldw	.d2t2	*+B4[14], B14
	;; Loads have 4 delay slots.  Take advantage of this to restore the
	;; scratch registers and stack pointer before the base registers
	;; disappear.  We also need to make sure no interrupts occur,
	;; so put the whole thing in the delay slots of a dummy branch
	;; We cannot move the ret earlier as that would cause it to occur
	;; before the last load completes
	b	.s1	(1f)
	ldw	.d1t1	*+A4[4], A4
	|| ldw	.d2t2	*+B4[4], B4
	ldw	.d1t1	*+A4[15], A15
	|| ldw	.d2t2	*+B4[15], B15
	ret	.s2	B10
	ldw	.d1t1	*+A4[10], A10
	|| ldw	.d2t2	*+B4[10], B10
	nop		1
1:
	nop		3
.size restore_core_regs, . - restore_core_regs

.macro UNWIND_WRAPPER name argreg argside
.global \name
.type \name, STT_FUNC
\name:
	# Create saved register state: flags,A0-A15,B0-B15,PC = 136 bytes.
	# Plus 4 (rounded to 8) for saving return.
	addk	.s2	-144, B15
	stw	.d2t1	A0, *+B15[2]
	stw	.d2t1	A1, *+B15[3]
	stw	.d2t1	A2, *+B15[4]
	stw	.d2t1	A3, *+B15[5]
	stw	.d2t1	A4, *+B15[6]
	stw	.d2t1	A5, *+B15[7]
	stw	.d2t1	A6, *+B15[8]
	stw	.d2t1	A7, *+B15[9]
	stw	.d2t1	A8, *+B15[10]
	stw	.d2t1	A9, *+B15[11]
	stw	.d2t1	A10, *+B15[12]
	stw	.d2t1	A11, *+B15[13]
	stw	.d2t1	A12, *+B15[14]
	stw	.d2t1	A13, *+B15[15]
	stw	.d2t1	A14, *+B15[16]
	stw	.d2t1	A15, *+B15[17]
	mv	.s1x	B15, A0
	addk	.s1	144, A0
	stw	.d2t2	B0, *+B15[18]
	stw	.d2t2	B1, *+B15[19]
	stw	.d2t2	B2, *+B15[20]
	stw	.d2t2	B3, *+B15[21]
	stw	.d2t2	B4, *+B15[22]
	stw	.d2t2	B5, *+B15[23]
	stw	.d2t2	B6, *+B15[24]
	stw	.d2t2	B7, *+B15[25]
	stw	.d2t2	B8, *+B15[26]
	stw	.d2t2	B9, *+B15[27]
	stw	.d2t2	B10, *+B15[28]
	stw	.d2t2	B11, *+B15[29]
	stw	.d2t2	B12, *+B15[30]
	stw	.d2t2	B13, *+B15[31]
	stw	.d2t2	B14, *+B15[32]
	stw	.d2t1	A0, *+B15[33]
	stw	.d2t1	A0, *+B15[34]
	# Zero demand saved flags
	mvk	.s1	0, A0
	stw	.d2t1	A0, *+B15[1]
	# Save return address, setup additional argument and call function
	stw	.d2t2	B3, *+B15[35]
	add	.d\argside	B15, 4, \argreg
	do_call	__gnu\name
	# Restore stack and return
	ldw	.d2t2	*+B15[35], B3
	addk	.s2	144, B15
	nop		3
	ret	.s2	B3
	nop		5
.size \name, . - \name
.endm

UNWIND_WRAPPER _Unwind_RaiseException B4 2
UNWIND_WRAPPER _Unwind_Resume B4 2
UNWIND_WRAPPER _Unwind_Resume_or_Rethrow B4 2
UNWIND_WRAPPER _Unwind_ForcedUnwind B6 2
UNWIND_WRAPPER _Unwind_Backtrace A6 1x
