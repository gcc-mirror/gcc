/* Definitions for specs for TREELANG

   The format of the specs file is documented in gcc.c

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

/* 
   This is the contribution to the `default_compilers' array in GCC.c for
   treelang.  
   
   This file must compile with 'traditional', so no ANSI string concatenations
   
*/

{".tree", "@treelang", NULL, 0, 0},
{".TREE", "@treelang", NULL, 0, 0},
{".tre", "@treelang", NULL, 0, 0},
{".TRE", "@treelang", NULL, 0, 0},
{"@treelang",
    "tree1\
       %{!Q:-quiet}\
       -dumpbase %B \
       %{d*}\
       %{m*}\
       %{a}\
       %{g*}\
       %{O*}\
       %{W*}\
       %{w}\
       %{ansi}\
       %{v}\
       %{--help:--help}\
       %{pg:-p}\
       %{p}\
       %{f*}\
       %{pg|p:%{fomit-frame-pointer:%e-pg or -p and -fomit-frame-pointer are incompatible}}\
       %{S:%W{o*}%{!o*:-o %b.s}}\
       %{!S:-o %g.s}\
       %i\n\
       %{!S:as %a\
       %Y\
       %{c:%W{o*}%{!o*:-o %w%b%O}}\
       %{!c:-o %d%w%u%O}\
       %g.s\
       %A\n}\
       ", NULL , 0, 0
},
