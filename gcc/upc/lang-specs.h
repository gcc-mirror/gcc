/* Definitions for specs for Objective-C.
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This is the contribution to the `default_compilers' array in gcc.c for
   upc.  */

  {".upc", "@upc", 0, 1, 1},
  {"@upc",
   /* cc1upc has an integrated ISO C preprocessor.  We should invoke the
      external preprocessor if -save-temps or -traditional is given.  */
     "%{E|M|MM:cc1upc -E %(upc_options) %(cpp_options) %(cpp_debug_options)}\
      %{!E:%{!M:%{!MM:\
	%{traditional|ftraditional|traditional-cpp:\
%e UPC does not support traditional compilation}\
	%{save-temps|no-integrated-cpp:cc1upc -E %(upc_options) %(cpp_options) %{save-temps:%b.mi} %{!save-temps:%g.mi} \n\
	    cc1upc -fpreprocessed %{save-temps:%b.mi} %{!save-temps:%g.mi} %(upc_options) %(cc1_options)}\
	%{!save-temps:%{!no-integrated-cpp:\
	    cc1upc %(cpp_unique_options) %(upc_options) %(cc1_options)}}\
        %{!fsyntax-only:%(invoke_as)}}}}", 0, 1, 1},
  {".upci", "@upc-cpp-output", 0, 1, 1},
  {"@upc-cpp-output",
     "%{!M:%{!MM:%{!E:cc1upc -fpreprocessed %i %(cc1_options)\
     -lang-upc\
     %{!fsyntax-only:%(invoke_as)}}}}", 0, 1, 1},
