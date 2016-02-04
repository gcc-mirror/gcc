! crt1.s for sparc & sparcv9 (SunOS 5)

!   Copyright (C) 1992-2016 Free Software Foundation, Inc.
!   Written By David Vinayak Henkel-Wallace, June 1992
! 
! This file is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by the
! Free Software Foundation; either version 3, or (at your option) any
! later version.
! 
! This file is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! General Public License for more details.
! 
! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.
!
! You should have received a copy of the GNU General Public License and
! a copy of the GCC Runtime Library Exception along with this program;
! see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
! <http://www.gnu.org/licenses/>.

! This file takes control of the process from the kernel, as specified
! in section 3 of the SVr4 ABI.
! This file is the first thing linked into any executable.

#ifdef __sparcv9
#define	CPTRSIZE	8
#define	CPTRSHIFT	3
#define	STACK_BIAS	2047
#define	ldn		ldx
#define	stn		stx
#define setn(s, scratch, dst)	setx s, scratch, dst
#else
#define	CPTRSIZE	4
#define	CPTRSHIFT	2
#define	STACK_BIAS	0
#define	ldn		ld
#define	stn		st
#define setn(s, scratch, dst)	set s, dst
#endif

	.section	".text"
	.proc	022
	.global	_start

_start:
	mov	0, %fp		! Mark bottom frame pointer
	ldn	[%sp + (16 * CPTRSIZE) + STACK_BIAS], %l0	! argc
	add	%sp, (17 * CPTRSIZE) + STACK_BIAS, %l1		! argv

	! Leave some room for a call.  Sun leaves 32 octets (to sit on
	! a cache line?) so we do too.
#ifdef __sparcv9
	sub	%sp, 48, %sp
#else
	sub	%sp, 32, %sp
#endif

	! %g1 may contain a function to be registered w/atexit
	orcc	%g0, %g1, %g0
#ifdef __sparcv9
	be	%xcc, .nope
#else
	be	.nope
#endif
	mov	%g1, %o0
	call	atexit
	nop   
.nope:
	! Now make sure constructors and destructors are handled.
	setn(_fini, %o1, %o0)
	call	atexit, 1
	nop
	call	_init, 0
	nop

	! We ignore the auxiliary vector; there is no defined way to
	! access those data anyway.  Instead, go straight to main:
	mov	%l0, %o0	! argc
	mov	%l1, %o1	! argv
#ifdef GCRT1
	setn(___Argv, %o4, %o3)
	stn	%o1, [%o3]      ! *___Argv
#endif
	! Skip argc words past argv, to env:
	sll	%l0, CPTRSHIFT, %o2
	add	%o2, CPTRSIZE, %o2
	add	%l1, %o2, %o2	! env
	setn(_environ, %o4, %o3)
	stn	%o2, [%o3]	! *_environ
	call	main, 4
	nop   
	call	exit, 0
	nop   
	call	_exit, 0
	nop   
	! We should never get here.

	.type	_start,#function
	.size	_start,.-_start
