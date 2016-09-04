/* -----------------------------------------------------------------------
   linux.S - (c) 2003-2004 Randolph Chung <tausq@debian.org>
	     (c) 2008 Red Hat, Inc.
	     (c) 2016 John David Anglin

   HPPA Foreign Function Interface

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
   IN NO EVENT SHALL RENESAS TECHNOLOGY BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#define LIBFFI_ASM
#include <fficonfig.h>
#include <ffi.h>

	.text
	.level 1.1
	.align 4

	/* void ffi_call_pa32(void (*)(char *, extended_cif *),
			       extended_cif *ecif,
			       unsigned bytes,
			       unsigned flags,
			       unsigned *rvalue,
			       void (*fn)(void),
			       ffi_go_closure *closure);
	 */

	.export ffi_call_pa32,code
	.import ffi_prep_args_pa32,code

	.type ffi_call_pa32, @function
	.cfi_startproc
ffi_call_pa32:
	.proc
	.callinfo FRAME=64,CALLS,SAVE_RP,SAVE_SP,ENTRY_GR=4
	.entry
	stw %rp, -20(%sp)
	copy %r3, %r1
        .cfi_offset 2, -20
        .cfi_register 3, 1

	copy %sp, %r3
	.cfi_def_cfa_register 3

	/* Setup the stack for calling prep_args...
	   We want the stack to look like this:

	   [ Previous stack                            ] <- %r3

	   [ 64-bytes register save area               ] <- %r4

	   [ Stack space for actual call, passed as    ] <- %arg0
	   [     arg0 to ffi_prep_args_pa32           ]

	   [ Stack for calling prep_args               ] <- %sp
	 */

	stwm %r1, 64(%sp)
	.cfi_offset 3, 0
	stw %r4, 12(%r3)
	copy %sp, %r4

	addl %arg2, %r4, %arg0      /* arg stack */
	stw %arg3, -48(%r3)         /* save flags; we need it later */

	/* Call prep_args:
	   %arg0(stack) -- set up above
	   %arg1(ecif) -- same as incoming param
	   %arg2(bytes) -- same as incoming param */
	bl ffi_prep_args_pa32,%r2
	ldo 64(%arg0), %sp
	ldo -64(%sp), %sp

	/* now %sp should point where %arg0 was pointing.  */

	/* Load the arguments that should be passed in registers
	   The fp args were loaded by the prep_args function.  */
	ldw -36(%sp), %arg0
	ldw -40(%sp), %arg1
	ldw -44(%sp), %arg2
	ldw -48(%sp), %arg3

	/* in case the function is going to return a structure
	   we need to give it a place to put the result.  */
	ldw -52(%r3), %ret0                     /* %ret0 <- rvalue */
	ldw -56(%r3), %r22                      /* %r22 <- function to call */
	ldw -60(%r3), %ret1                     /* %ret1 <- closure */
	bl $$dyncall, %r31                      /* Call the user function */
	copy %r31, %rp

	/* Prepare to store the result; we need to recover flags and rvalue.  */
	ldw -48(%r3), %r21                      /* r21 <- flags */
	ldw -52(%r3), %r20                      /* r20 <- rvalue */

	/* Store the result according to the return type.  */

.Lcheckint:
	comib,<>,n FFI_TYPE_INT, %r21, .Lcheckint8
	b	.Ldone
	stw	%ret0, 0(%r20)

.Lcheckint8:
	comib,<>,n FFI_TYPE_UINT8, %r21, .Lcheckint16
	b	.Ldone
	stb	%ret0, 0(%r20)

.Lcheckint16:
	comib,<>,n FFI_TYPE_UINT16, %r21, .Lcheckdbl
	b	.Ldone
	sth	%ret0, 0(%r20)

.Lcheckdbl:
	comib,<>,n FFI_TYPE_DOUBLE, %r21, .Lcheckfloat
	b	.Ldone
	fstd	%fr4,0(%r20)

.Lcheckfloat:
	comib,<>,n FFI_TYPE_FLOAT, %r21, .Lcheckll
	b	.Ldone
	fstw	%fr4L,0(%r20)

.Lcheckll:
	comib,<>,n FFI_TYPE_UINT64, %r21, .Lchecksmst2
	stw	%ret0, 0(%r20)
	b	.Ldone
	stw	%ret1, 4(%r20)

.Lchecksmst2:
	comib,<>,n FFI_TYPE_SMALL_STRUCT2, %r21, .Lchecksmst3
	/* 2-byte structs are returned in ret0 as ????xxyy.  */
	extru	%ret0, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	b	.Ldone
	stb	%ret0, 0(%r20)

.Lchecksmst3:
	comib,<>,n FFI_TYPE_SMALL_STRUCT3, %r21, .Lchecksmst4
	/* 3-byte structs are returned in ret0 as ??xxyyzz.  */
	extru	%ret0, 15, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret0, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	b	.Ldone
	stb	%ret0, 0(%r20)

.Lchecksmst4:
	comib,<>,n FFI_TYPE_SMALL_STRUCT4, %r21, .Lchecksmst5
	/* 4-byte structs are returned in ret0 as wwxxyyzz.  */
	extru	%ret0, 7, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret0, 15, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret0, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	b	.Ldone
	stb	%ret0, 0(%r20)

.Lchecksmst5:
	comib,<>,n FFI_TYPE_SMALL_STRUCT5, %r21, .Lchecksmst6
	/* 5 byte values are returned right justified:
	      ret0     ret1
	   5: ??????aa bbccddee */
	stbs,ma	%ret0, 1(%r20)
	extru	%ret1, 7, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret1, 15, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret1, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	b	.Ldone
	stb	%ret1, 0(%r20)

.Lchecksmst6:
	comib,<>,n FFI_TYPE_SMALL_STRUCT6, %r21, .Lchecksmst7
	/* 6 byte values are returned right justified:
	      ret0     ret1
	   6: ????aabb ccddeeff */
	extru	%ret0, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	stbs,ma	%ret0, 1(%r20)
	extru	%ret1, 7, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret1, 15, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret1, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	b	.Ldone
	stb	%ret1, 0(%r20)

.Lchecksmst7:
	comib,<>,n FFI_TYPE_SMALL_STRUCT7, %r21, .Lchecksmst8
	/* 7 byte values are returned right justified:
	      ret0     ret1
	   7: ??aabbcc ddeeffgg */
	extru	%ret0, 15, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret0, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	stbs,ma	%ret0, 1(%r20)
	extru	%ret1, 7, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret1, 15, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret1, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	b	.Ldone
	stb	%ret1, 0(%r20)

.Lchecksmst8:
	comib,<>,n FFI_TYPE_SMALL_STRUCT8, %r21, .Ldone
	/* 8 byte values are returned right justified:
	      ret0     ret1
	   8: aabbccdd eeffgghh */
	extru	%ret0, 7, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret0, 15, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret0, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	stbs,ma	%ret0, 1(%r20)
	extru	%ret1, 7, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret1, 15, 8, %r22
	stbs,ma	%r22, 1(%r20)
	extru	%ret1, 23, 8, %r22
	stbs,ma	%r22, 1(%r20)
	stb	%ret1, 0(%r20)

.Ldone:
	/* all done, return */
	copy %r4, %sp                           /* pop arg stack */
	ldw 12(%r3), %r4
	ldwm -64(%sp), %r3                      /* .. and pop stack */
	ldw -20(%sp), %rp
	bv %r0(%rp)
	nop
	.exit
	.procend
	.cfi_endproc

	/* void ffi_closure_pa32(void);
	   Called with ffi_closure argument in %r21.  */
	.export ffi_closure_pa32,code
	.import ffi_closure_inner_pa32,code
	.type ffi_closure_pa32, @function
	.cfi_startproc
ffi_closure_pa32:
	.proc
	.callinfo FRAME=64,CALLS,SAVE_RP,SAVE_SP,ENTRY_GR=3
	.entry

	stw %rp, -20(%sp)
	copy %r3, %r1
	.cfi_offset 2, -20
	.cfi_register 3, 1
	copy %sp, %r3
	.cfi_def_cfa_register 3
	stwm %r1, 64(%sp)
	.cfi_offset 3, 0

	/* Put arguments onto the stack and call ffi_closure_inner.  */
	stw %arg0, -36(%r3)
	stw %arg1, -40(%r3)
	stw %arg2, -44(%r3)
	stw %arg3, -48(%r3)

	/* Closure type 0.  */
	copy %r21, %arg0
	copy %r0, %arg2
	bl ffi_closure_inner_pa32, %r2
	copy %r3, %arg1

	ldwm -64(%sp), %r3
	ldw -20(%sp), %rp
	ldw -36(%sp), %ret0
	bv %r0(%r2)
	ldw -40(%sp), %ret1

	.exit
	.procend
	.cfi_endproc

	/* void ffi_go_closure_pa32(void);
	   Called with ffi_go_closure argument in %ret1.  */
	.export ffi_go_closure_pa32,code
	.import ffi_closure_inner_pa32,code
	.type ffi_go_closure_pa32, @function
	.cfi_startproc
ffi_go_closure_pa32:
	.proc
	.callinfo FRAME=64,CALLS,SAVE_RP,SAVE_SP,ENTRY_GR=3
	.entry

	stw %rp, -20(%sp)
	copy %r3, %r1
	.cfi_offset 2, -20
	.cfi_register 3, 1
	copy %sp, %r3
	.cfi_def_cfa_register 3
	stwm %r1, 64(%sp)
	.cfi_offset 3, 0

	/* Put arguments onto the stack and call ffi_closure_inner.  */
	stw %arg0, -36(%r3)
	stw %arg1, -40(%r3)
	stw %arg2, -44(%r3)
	stw %arg3, -48(%r3)

	/* Closure type 1.  */
	copy %ret1, %arg0
	ldi 1, %arg2
	bl ffi_closure_inner_pa32, %r2
	copy %r3, %arg1

	ldwm -64(%sp), %r3
	ldw -20(%sp), %rp
	ldw -36(%sp), %ret0
	bv %r0(%r2)
	ldw -40(%sp), %ret1

	.exit
	.procend
	.cfi_endproc
