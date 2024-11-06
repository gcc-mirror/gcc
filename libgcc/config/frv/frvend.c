/* Frv initialization file linked after all user modules
   Copyright (C) 1999-2024 Free Software Foundation, Inc.
    Contributed by Red Hat, Inc.

   This file is part of GCC.

   GCC is free software ; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY ; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <stddef.h>
#include "../libgcc/unwind-dw2-fde.h"

#ifdef __FRV_UNDERSCORE__
#define UNDERSCORE "_"
#else
#define UNDERSCORE ""
#endif

#define FINI_SECTION_ZERO(SECTION, FLAGS, NAME)				\
__asm__ (".section " SECTION "," FLAGS "\n\t"				\
	 ".globl   " UNDERSCORE NAME "\n\t"				\
	 ".type    " UNDERSCORE NAME ",@object\n\t"			\
	 ".p2align  2\n"						\
	 UNDERSCORE NAME ":\n\t"					\
	 ".word     0\n\t"						\
	 ".previous")

#define FINI_SECTION(SECTION, FLAGS, NAME)				\
__asm__ (".section " SECTION "," FLAGS "\n\t"				\
	 ".globl   " UNDERSCORE NAME "\n\t"				\
	 ".type    " UNDERSCORE NAME ",@object\n\t"			\
	 ".p2align  2\n"						\
	 UNDERSCORE NAME ":\n\t"					\
	 ".previous")

/* End of .ctor/.dtor sections that provides a list of constructors and
   destructors to run.  */

FINI_SECTION_ZERO (".ctors", "\"a\"", "__CTOR_END__");
FINI_SECTION_ZERO (".dtors", "\"a\"", "__DTOR_END__");

/* End of .eh_frame section that provides all of the exception handling
   tables.  */

FINI_SECTION_ZERO (".eh_frame", "\"aw\"", "__FRAME_END__");

#if ! __FRV_FDPIC__
/* In FDPIC, the linker itself generates this.  */
/* End of .rofixup section that provides a list of pointers that we
   need to adjust.  */

FINI_SECTION (".rofixup", "\"a\"", "__ROFIXUP_END__");
#endif /* __FRV_FDPIC__ */
