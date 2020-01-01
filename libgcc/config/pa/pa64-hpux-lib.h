/* Definitions of target machine for GNU compiler, for HPs running
   HP-UX using the 64bit runtime model.
   Copyright (C) 1999-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* We use DTOR_LIST_BEGIN to carry a bunch of hacks to allow us to use
   the init and fini array sections with both the HP and GNU linkers.
   The linkers setup the required dynamic entries in the dynamic segment
   and the dynamic linker does the calls.  This approach avoids using
   collect2.

   The first hack is to implement __do_global_ctors_aux in crtbegin as
   it needs to be the first entry in the init array so that it is called
   last.  HP got the order of the init array backwards.  The DT_INIT_ARRAY
   is supposed to be executed in the same order as the addresses appear in
   the array.  DT_FINI_ARRAY is supposed to be executed in the opposite
   order.

   The second hack is a set of plabels to implement the effect of
   CRT_CALL_STATIC_FUNCTION.  HP-UX 11 only supports DI_INIT_ARRAY and
   DT_FINI_ARRAY and they put the arrays in .init and .fini, rather than
   in .init_array and .fini_array.  The standard defines for .init and
   .fini have the execute flag set.  So, the assembler has to be hacked
   to munge the standard flags for these sections to make them agree
   with what the HP linker expects.  With the GNU linker, we need to
   used the .init_array and .fini_array sections.  So, we set up for
   both just in case.  Once we have built the table, the linker does
   the rest of the work.
   The order is significant.  Placing __do_global_ctors_aux first in
   the list, results in it being called last.  User specified initializers,
   either using the linker +init command or a plabel, run before the
   initializers specified here.  */

/* We need to add frame_dummy to the initializer list if EH_FRAME_SECTION_NAME
   is defined.  */
#if defined(__LIBGCC_EH_FRAME_SECTION_NAME__)
#define PA_INIT_FRAME_DUMMY_ASM_OP ".dword P%frame_dummy"
#else
#define PA_INIT_FRAME_DUMMY_ASM_OP ""
#endif

/* The following hack sets up the .init, .init_array, .fini and
   .fini_array sections.  */
#define PA_CRTBEGIN_HACK \
asm (TEXT_SECTION_ASM_OP);                                              \
static void __attribute__((used))                                       \
__do_global_ctors_aux (void)                                            \
{                                                                       \
  func_ptr *p = __CTOR_LIST__;                                          \
  while (*(p + 1))                                                      \
    p++;                                                                \
  for (; *p != (func_ptr) -1; p--)                                      \
    (*p) ();                                                            \
}                                                                       \
                                                                        \
asm (HP_INIT_ARRAY_SECTION_ASM_OP);                                     \
asm (".align 8");                                                       \
asm (".dword P%__do_global_ctors_aux");                                 \
asm (PA_INIT_FRAME_DUMMY_ASM_OP);                                       \
asm (GNU_INIT_ARRAY_SECTION_ASM_OP);                                    \
asm (".align 8");                                                       \
asm (".dword P%__do_global_ctors_aux");                                 \
asm (PA_INIT_FRAME_DUMMY_ASM_OP);                                       \
asm (HP_FINI_ARRAY_SECTION_ASM_OP);                                     \
asm (".align 8");                                                       \
asm (".dword P%__do_global_dtors_aux");                                 \
asm (GNU_FINI_ARRAY_SECTION_ASM_OP);                                    \
asm (".align 8");                                                       \
asm (".dword P%__do_global_dtors_aux")

/* The following two variants of DTOR_LIST_BEGIN are identical to those
   in crtstuff.c except for the addition of the above crtbegin hack.  */
#ifdef __LIBGCC_DTORS_SECTION_ASM_OP__
#define DTOR_LIST_BEGIN \
asm (DTORS_SECTION_ASM_OP);                                             \
STATIC func_ptr __DTOR_LIST__[1]                                        \
  __attribute__ ((aligned(sizeof(func_ptr))))                           \
  = { (func_ptr) (-1) };                                                \
PA_CRTBEGIN_HACK
#else
#define DTOR_LIST_BEGIN \
STATIC func_ptr __DTOR_LIST__[1]                                        \
  __attribute__ ((section(".dtors"), aligned(sizeof(func_ptr))))        \
  = { (func_ptr) (-1) };                                                \
PA_CRTBEGIN_HACK
#endif
