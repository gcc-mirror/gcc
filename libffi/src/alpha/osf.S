/* -----------------------------------------------------------------------
   osf.S - Copyright (c) 1998 Cygnus Solutions
   
   Alpha/OSF Foreign Function Interface 

   $Id: osf.S,v 1.1.1.1 1998/11/29 16:48:16 green Exp $

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   ``Software''), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL CYGNUS SOLUTIONS BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#define LIBFFI_ASM	
#include <ffi.h>

#define callback $16
#define ecifp	 $17
#define bytes	 $18
#define flags	 $19
#define raddr    $20
#define fn       $21

#define flags_ofs	16
#define raddr_ofs	24
#define fn_ofs		32

#define SIZEOF_FRAME	(6*8)

	.text
	.align	4
	.globl	ffi_call_osf
	.ent	ffi_call_osf
	
ffi_call_osf:
	lda	$30, -SIZEOF_FRAME($30)
	stq	$26, 0($30)
	stq	$15, 8($30)
	stq	flags, flags_ofs($30)
	stq	raddr, raddr_ofs($30)
	stq	fn, fn_ofs($30)
	mov	$30, $15
	.frame	$15, SIZEOF_FRAME, $26, 0
        .mask   0x4008000, -SIZEOF_FRAME
	.prologue 0

	mov	callback, $27		# mov callback into place
	subq	$30, bytes, $30		# allocate stack space
	
	# Call ffi_prep_args; ecif, bytes and flags are already in place.
	mov	$30, $16		# push stack arg
	jsr	$26, ($27), 0

	# Load up all of the (potential) argument registers.
	ldt	$f16, 0($30)
	ldt	$f17, 8($30)
	ldt	$f18, 16($30)
	ldt	$f19, 24($30)
	ldt	$f20, 32($30)
	ldt	$f21, 40($30)
	ldq	$16, 48($30)
	ldq	$17, 56($30)
	ldq	$18, 64($30)
	ldq	$19, 72($30)
	ldq	$20, 80($30)
	ldq	$21, 88($30)

	# Get rid of the arg reg temp space and call the function.
	ldq	$27, fn_ofs($15)
	lda	$30, 12*8($30)
	jsr	$26, ($27), 0

	# If the return value pointer is NULL, assume no return value.
	ldq	raddr, raddr_ofs($15)
	beq	raddr, $noretval

	ldq	flags, flags_ofs($15)
	cmpeq	flags, FFI_TYPE_INT, $1
	bne	$1, $retint
	cmpeq	flags, FFI_TYPE_FLOAT, $2
	bne	$2, $retfloat
	cmpeq	flags, FFI_TYPE_DOUBLE, $3
	bne	$3, $retdouble
	br	$retstruct
	
	.align 3
$retint:
	stq	$0, 0(raddr)
	br	$noretval
$retfloat:
	sts	$f0, 0(raddr)
	br	$noretval
$retdouble:
	stt	$f0, 0(raddr)

$retstruct:
$noretval:
	mov	$15, $30
	ldq	$26, 0($15)
	ldq	$15, 8($15)
	lda	$30, SIZEOF_FRAME($30)
	ret

	.end	ffi_call_osf
