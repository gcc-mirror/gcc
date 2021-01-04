/* GNU D Compiler bindings for built-in functions and types.
   Copyright (C) 2006-2021 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/**
  Declarations are automatically created by the compiler.  All
  declarations start with "__builtin_". Refer to _builtins.def in the
  GCC source for a list of functions.  Not all of the functions are
  supported.

  In addition to built-in functions, the following types are defined.

  $(TABLE
  $(TR $(TD ___builtin_va_list)      $(TD The target's va_list type ))
  $(TR $(TD ___builtin_clong  )      $(TD The D equivalent of the target's
                                       C "long" type ))
  $(TR $(TD ___builtin_culong )      $(TD The D equivalent of the target's
                                       C "unsigned long" type ))
  $(TR $(TD ___builtin_machine_int ) $(TD Signed word-sized integer ))
  $(TR $(TD ___builtin_machine_uint) $(TD Unsigned word-sized integer ))
  $(TR $(TD ___builtin_pointer_int ) $(TD Signed pointer-sized integer ))
  $(TR $(TD ___builtin_pointer_uint) $(TD Unsigned pointer-sized integer ))
  )
 */

module gcc.builtins;
