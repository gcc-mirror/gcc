/* .init/.fini section handling + C++ global constructor/destructor
   handling of Andes NDS32 cpu for GNU compiler.
   This file is based on crtstuff.c, sol2-crti.asm, sol2-crtn.asm.
   Copyright (C) 2012-2024 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <stddef.h>
/* Need header file for `struct object' type.  */
#include "../libgcc/unwind-dw2-fde.h"

/*  Declare a pointer to void function type.  */
typedef void (*func_ptr) (void);

#ifdef CRT_BEGIN

/* NOTE:  In order to be able to support SVR4 shared libraries, we arrange
   to have one set of symbols { __CTOR_LIST__, __DTOR_LIST__, __CTOR_END__,
   __DTOR_END__ } per root executable and also one set of these symbols
   per shared library.  So in any given whole process image, we may have
   multiple definitions of each of these symbols.  In order to prevent
   these definitions from conflicting with one another, and in order to
   ensure that the proper lists are used for the initialization/finalization
   of each individual shared library (respectively), we give these symbols
   only internal (i.e. `static') linkage, and we also make it a point to
   refer to only the __CTOR_END__ symbol in crtfini.o and the __DTOR_LIST__
   symbol in crtinit.o, where they are defined.  */

static func_ptr __CTOR_LIST__[1] __attribute__ ((section (".ctors"), used))
     = { (func_ptr) 0 };

static func_ptr __DTOR_LIST__[1] __attribute__ ((section (".dtors"), used))
     = { (func_ptr) 0 };


#ifdef SUPPORT_UNWINDING_DWARF2
/* Preparation of exception handling with dwar2 mechanism registration.  */

asm ("\n\
	.section .eh_frame,\"aw\",@progbits\n\
	.global __EH_FRAME_BEGIN__\n\
	.type	__EH_FRAME_BEGIN__, @object\n\
	.align 2\n\
__EH_FRAME_BEGIN__:\n\
	! Beginning location of eh_frame section\n\
	.previous\n\
");

extern func_ptr __EH_FRAME_BEGIN__[];


/* Note that the following two functions are going to be chained into
   constructor and destructor list, repectively.  So these two declarations
   must be placed after __CTOR_LIST__ and __DTOR_LIST.  */
extern void __nds32_register_eh(void) __attribute__((constructor, used));
extern void __nds32_deregister_eh(void) __attribute__((destructor, used));

/* Register the exception handling table as the first constructor.  */
void
__nds32_register_eh (void)
{
  static struct object object;
  if (__register_frame_info)
    __register_frame_info (__EH_FRAME_BEGIN__, &object);
}

/* Unregister the exception handling table as a deconstructor.  */
void
__nds32_deregister_eh (void)
{
  static int completed = 0;

  if (completed)
    return;

  if (__deregister_frame_info)
    __deregister_frame_info (__EH_FRAME_BEGIN__);

  completed = 1;
}
#endif

/* Run all the global destructors on exit from the program.  */

/* Some systems place the number of pointers in the first word of the
   table.  On SVR4 however, that word is -1.  In all cases, the table is
   null-terminated.  On SVR4, we start from the beginning of the list and
   invoke each per-compilation-unit destructor routine in order
   until we find that null.

   Note that this function MUST be static.  There will be one of these
   functions in each root executable and one in each shared library, but
   although they all have the same code, each one is unique in that it
   refers to one particular associated `__DTOR_LIST__' which belongs to the
   same particular root executable or shared library file.  */

static void __do_global_dtors (void)
asm ("__do_global_dtors") __attribute__ ((section (".text"), used));

static void
__do_global_dtors (void)
{
  func_ptr *p;
  for (p = __DTOR_LIST__ + 1; *p; p++)
    (*p) ();
}

/* .init section start.
   This must appear at the start of the .init section.  */

asm ("\n\
	.section .init\n\
	.global _init\n\
	.type	_init, @function\n\
_init:\n\
	! 1. store $fp\n\
	! 2. adjust $fp by $sp\n\
	! 3. adjust $sp\n\
");

/* .fini section start.
   This must appear at the start of the .fini section.  */

asm ("\n\
	.section .fini\n\
	.global _fini\n\
	.type	_fini, @function\n\
_fini:\n\
	! 1. store $fp\n\
	! 2. adjust $fp by $sp\n\
	! 3. adjust $sp\n\
	! 4. call __do_global_dtors\n\
	j	__do_global_dtors\n\
");

#endif /* CRT_BEGIN */

#ifdef CRT_END

/* Define __dso_handle which would be needed for C++ library.
   Since our elf-toolchain only builds programs with static link,
   we can directly define 'void *__dso_handle = 0'.  */
void *__dso_handle = 0;

/* Put a word containing zero at the end of each of our two lists of function
   addresses.  Note that the words defined here go into the .ctors and .dtors
   sections of the crtend.o file, and since that file is always linked in
   last, these words naturally end up at the very ends of the two lists
   contained in these two sections.  */

static func_ptr __CTOR_END__[1] __attribute__ ((section (".ctors"), used))
     = { (func_ptr) 0 };

static func_ptr __DTOR_END__[1] __attribute__ ((section (".dtors"), used))
     = { (func_ptr) 0 };

#ifdef SUPPORT_UNWINDING_DWARF2
/* ZERO terminator in .eh_frame section.  */
asm ("\n\
	.section .eh_frame,\"aw\",@progbits\n\
	.global __EH_FRAME_END__\n\
	.type	__EH_FRAME_END__, @object\n\
	.align 2\n\
__EH_FRAME_END__:\n\
	! End location of eh_frame section with ZERO terminator\n\
	.word 0\n\
	.previous\n\
");
#endif

/* Run all global constructors for the program.
   Note that they are run in reverse order.  */

static void __do_global_ctors (void)
asm ("__do_global_ctors") __attribute__ ((section (".text"), used));

static void
__do_global_ctors (void)
{
  func_ptr *p;
  for (p = __CTOR_END__ - 1; *p; p--)
    (*p) ();
}

/* .init section end.
   This must live at the end of the .init section.  */

asm ("\n\
	.section .init\n\
	! 1. call __do_global_ctors\n\
	! 2. adjust back $sp\n\
	! 3. restore $fp\n\
	j	__do_global_ctors\n\
");

/* .fini section end.
   This must live at the end of the .fini section.  */

asm ("\n\
	.section .fini\n\
	! 1. adjust back $sp\n\
	! 2. restore $fp\n\
");

#endif /* CRT_END */
