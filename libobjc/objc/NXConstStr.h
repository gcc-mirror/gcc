/* Interface for the NXConstantString class for Objective-C.
   Copyright (C) 1995-2017 Free Software Foundation, Inc.
   Contributed by Pieter J. Schoenmakers <tiggr@es.ele.tue.nl>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#ifndef __nxconstantstring_INCLUDE_GNU
#define __nxconstantstring_INCLUDE_GNU

#include "Object.h"

#ifdef __cplusplus
extern "C" {
#endif

@interface NXConstantString: Object
{
  char *c_string;
  unsigned int len;
}

-(const char *) cString;
-(unsigned int) length;

@end

#ifdef __cplusplus
}
#endif

#endif
