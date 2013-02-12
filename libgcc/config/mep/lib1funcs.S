/* libgcc routines for Toshiba Media Processor.
   Copyright (C) 2001-2013 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.
  
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

#define SAVEALL \
	add3	$sp, $sp, -16*4 ; \
	sw	$0, ($sp) ; \
	sw	$1, 4($sp) ; \
	sw	$2, 8($sp) ; \
	sw	$3, 12($sp) ; \
	sw	$4, 16($sp) ; \
	sw	$5, 20($sp) ; \
	sw	$6, 24($sp) ; \
	sw	$7, 28($sp) ; \
	sw	$8, 32($sp) ; \
	sw	$9, 36($sp) ; \
	sw	$10, 40($sp) ; \
	sw	$11, 44($sp) ; \
	sw	$12, 48($sp) ; \
	sw	$13, 52($sp) ; \
	sw	$14, 56($sp) ; \
	ldc	$5, $lp	; \
	add	$5, 3 ; \
	mov	$6, -4 ; \
	and	$5, $6

#define RESTOREALL \
	stc	$5, $lp ; \
	lw	$14, 56($sp) ; \
	lw	$13, 52($sp) ; \
	lw	$12, 48($sp) ; \
	lw	$11, 44($sp) ; \
	lw	$10, 40($sp) ; \
	lw	$9, 36($sp) ; \
	lw	$8, 32($sp) ; \
	lw	$7, 28($sp) ; \
	lw	$6, 24($sp) ; \
	lw	$5, 20($sp) ; \
	lw	$4, 16($sp) ; \
	lw	$3, 12($sp) ; \
	lw	$2, 8($sp) ; \
	lw	$1, 4($sp) ; \
	lw	$0, ($sp) ; \
	add3	$sp, $sp, 16*4 ; \
	ret

#ifdef L_mep_profile
	.text
	.global __mep_mcount
__mep_mcount:
	SAVEALL
	ldc	$1, $lp
	mov	$2, $0
	bsr	__mep_mcount_2
	RESTOREALL
#endif

#ifdef L_mep_bb_init_trace
	.text
	.global __mep_bb_init_trace_func
__mep_bb_init_trace_func:
	SAVEALL
	lw	$1, ($5)
	lw	$2, 4($5)
	add	$5, 8
	bsr	__bb_init_trace_func
	RESTOREALL
#endif

#ifdef L_mep_bb_init
	.text
	.global __mep_bb_init_func
__mep_bb_init_func:
	SAVEALL
	lw	$1, ($5)
	add	$5, 4
	bsr	__bb_init_func
	RESTOREALL
#endif

#ifdef L_mep_bb_trace
	.text
	.global __mep_bb_trace_func
__mep_bb_trace_func:
	SAVEALL
	movu	$3, __bb
	lw	$1, ($5)
	sw	$1, ($3)
	lw	$2, 4($5)
	sw	$2, 4($3)
	add	$5, 8
	bsr	__bb_trace_func
	RESTOREALL
#endif

#ifdef L_mep_bb_increment
	.text
	.global __mep_bb_increment_func
__mep_bb_increment_func:
	SAVEALL
	lw	$1, ($5)
	lw	$0, ($1)
	add	$0, 1
	sw	$0, ($1)
	add	$5, 4
	RESTOREALL
#endif
