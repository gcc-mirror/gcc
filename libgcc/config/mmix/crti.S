/* Copyright (C) 2001-2013 Free Software Foundation, Inc.
   Contributed by Hans-Peter Nilsson <hp@bitrange.com>

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

% This is the crt0 equivalent for mmix-knuth-mmixware, for setting up
% things for compiler-generated assembly-code and for setting up things
% between where the simulator calls and main, and shutting things down on
% the way back.  There's an actual crt0.o elsewhere, but that's a dummy.

% This file and the GCC output are supposed to be *reasonably*
% mmixal-compatible to enable people to re-use output with Knuth's mmixal.
% However, forward references are used more freely: we are using the
% binutils tools.  Users of mmixal beware; you will sometimes have to
% re-order things or use temporary variables.

% Users of mmixal will want to set up 8H and 9H to be .text and .data
% respectively, so the compiler can switch between them pretending they're
% segments.

% This little treasure (some contents) is required so the 32 lowest
% address bits of user data will not be zero.  Because of truncation,
% that would cause testcase gcc.c-torture/execute/980701-1.c to
% incorrectly fail.

	.data	! mmixal:= 8H LOC Data_Segment
	.p2align 3
dstart	OCTA 2009

	.text	! mmixal:= 9H LOC 8B; LOC #100
	.global Main

% The __Stack_start symbol is provided by the link script.
stackpp	OCTA __Stack_start
crtstxt	OCTA _init	% Assumed to be the lowest executed address.
	OCTA __etext	% Assumed to be beyond the highest executed address.

crtsdat	OCTA dstart	% Assumed to be the lowest accessed address.
	OCTA _end	% Assumed to be beyond the highest accessed address.

% "Main" is the magic symbol the simulator jumps to.  We want to go
% on to "main".
% We need to set rG explicitly to avoid hard-to-debug situations.
Main	SETL	$255,32
	PUT	rG,$255

% Make sure we have valid memory for addresses in .text and .data (and
% .bss, but we include this in .data), for the benefit of mmo-using
% simulators that require validation of addresses for which contents
% is not present.  Due to its implicit-zero nature, zeros in contents
% may be left out in the mmo format, but we don't know the boundaries
% of those zero-chunks; for mmo files from binutils, they correspond
% to the beginning and end of sections in objects before linking.  We
% validate the contents by executing PRELD (0; one byte) on each
% 2048-byte-boundary of our .text .data, and we assume this size
% matches the magic lowest-denominator chunk-size for all
% validation-requiring simulators.  The effect of the PRELD (any size)
% is assumed to be the same as initial loading of the contents, as
% long as the PRELD happens before the first PUSHJ/PUSHGO.  If it
% happens after that, we'll need to distinguish between
% access-for-execution and read/write access.

	GETA	$255,crtstxt
	LDOU	$2,$255,0
	ANDNL	$2,#7ff		% Align the start at a 2048-boundary.
	LDOU	$3,$255,8
	SETL	$4,2048
0H	PRELD	0,$2,0
	ADDU	$2,$2,$4
	CMP	$255,$2,$3
	BN	$255,0B

	GETA	$255,crtsdat
	LDOU	$2,$255,0
	ANDNL	$2,#7ff
	LDOU	$3,$255,8
0H	PRELD	0,$2,0
	ADDU	$2,$2,$4
	CMP	$255,$2,$3
	BN	$255,0B

% Initialize the stack pointer.  It is supposedly made a global
% zero-initialized (allowed to change) register in crtn.S; we use the
% explicit number.
	GETA	$255,stackpp
	LDOU	$254,$255,0

	PUSHJ	$2,_init

#ifdef __MMIX_ABI_GNU__
% Copy argc and argv from their initial position to argument registers
% where necessary.
	SET	$231,$0
	SET	$232,$1
#else
% For the mmixware ABI, we need to move arguments.  The return value will
% appear in $0.
	SET	$2,$1
	SET	$1,$0
#endif

	PUSHJ	$0,main
	JMP	exit

% Provide the first part of _init and _fini.  Save the return address on the
% register stack.  We eventually ignore the return address of these
% PUSHJ:s, so it doesn't matter that whether .init and .fini code calls
% functions or where they store rJ.  We shouldn't get there, so die
% (TRAP Halt) if that happens.

	.section .init,"ax",@progbits
	.global	_init
_init:
	GET	$0,:rJ
	PUSHJ	$1,0F
	SETL	$255,255
	TRAP	0,0,0
0H	IS	@

% Register _fini to be executed as the last atexit function.
#ifdef __MMIX_ABI_GNU__
	GETA	$231,_fini
#else
	GETA	$1,_fini
#endif
	PUSHJ	$0,atexit

	.section .fini,"ax",@progbits
	.global	_fini
_fini:
	GET	$0,:rJ
	PUSHJ	$1,0F
	SETL	$255,255
	TRAP	0,0,0
0H	IS	@
