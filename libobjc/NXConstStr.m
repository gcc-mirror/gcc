/* Implementation of the NXConstantString class for Objective-C.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by Pieter J. Schoenmakers <tiggr@es.ele.tue.nl>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */
 
/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "objc/NXConstStr.h"

@implementation NXConstantString

-(const char *) cString
{
  return (c_string);
} /* -cString */

-(unsigned int) length
{
  return (len);
} /* -length */

@end
