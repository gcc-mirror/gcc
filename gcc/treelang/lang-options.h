/* Definitions for switches for TREELANG.

   Copyright (C) 1995, 96-98, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

DEFINE_LANG_NAME ("treelang")
     
/* This is the contribution to the `lang_options' array in gcc.c for
   treelang.  */
 {"-fparser-trace", N_("(debug) trace parsing process")},
 {"-flexer-trace", N_("(debug) trace lexical analysis")},


