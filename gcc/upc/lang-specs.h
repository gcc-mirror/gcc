/* upc/lang-specs.h: UPC-specific specs.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.
   Based on original implementation
     by Jesse M. Draper <jdraper@super.org>
     and William W. Carlson <wwc@super.org>.
   Derived from objc/lang-specs.h.

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

/* This is the contribution to the `default_compilers' array in gcc.c for
   upc.  */

  {".upc", "@upc", 0, 0, 0},
  {"@upc",
   /* cc1upc has an integrated ISO C preprocessor.  We should invoke the
      external preprocessor if -save-temps or -traditional is given.  */
     "%{E|M|MM:cc1upc -E %(upc_options) %(cpp_options) %(cpp_debug_options)}\
      %{!E:%{!M:%{!MM:\
	%{traditional|ftraditional|traditional-cpp:\
              %e UPC does not support traditional compilation}\
	%{save-temps|no-integrated-cpp:\
	      cc1upc -E %(upc_options) %(cpp_options)\
	  %{save-temps:%b.mi} %{!save-temps:%g.mi} \n\
	      cc1upc -fpreprocessed %{save-temps:%b.mi} %{!save-temps:%g.mi}\
	             %(upc_options) %(cc1_options)}\
	%{!save-temps:%{!no-integrated-cpp:\
	    cc1upc %(cpp_unique_options) %(upc_options) %(cc1_options)}}\
        %{!fsyntax-only:%(invoke_as)}}}}", 0, 0, 0},
  {".upci", "@upc-cpp-output", 0, 0, 0},
  {"@upc-cpp-output",
     "%{!M:%{!MM:%{!E:cc1upc -fpreprocessed %i %(cc1_options)\
     -lang-upc\
     %{!fsyntax-only:%(invoke_as)}}}}", 0, 0, 0},
