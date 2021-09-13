/* Data types used in the IL symbol table.
   Copyright (C) 2009-2021 Free Software Foundation, Inc.
   Contributed by Rafael Espindola <espindola@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_LTO_SYMTAB_H
#define GCC_LTO_SYMTAB_H

enum gcc_plugin_symbol_kind
  {
    GCCPK_DEF,
    GCCPK_WEAKDEF,
    GCCPK_UNDEF,
    GCCPK_WEAKUNDEF,
    GCCPK_COMMON
  };

enum gcc_plugin_symbol_visibility
  {
    GCCPV_DEFAULT,
    GCCPV_PROTECTED,
    GCCPV_INTERNAL,
    GCCPV_HIDDEN
  };

enum gcc_plugin_symbol_type
{
  GCCST_UNKNOWN,
  GCCST_FUNCTION,
  GCCST_VARIABLE
};

enum gcc_plugin_symbol_section_kind
{
  GCCSSK_DEFAULT,
  GCCSSK_BSS
};

#endif /* GCC_LTO_SYMTAB_H  */
