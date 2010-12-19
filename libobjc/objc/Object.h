/* Interface for the Object class for Objective-C.
   Copyright (C) 1993, 1994, 1995, 2009, 2010 Free Software Foundation, Inc.

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


#ifndef __object_INCLUDE_GNU
#define __object_INCLUDE_GNU

#include "objc.h"

#ifdef __cplusplus
extern "C" {
#endif

/* The Object class is a very minimal root class included with the
   runtime.  It is used as superclass for the two classes included
   with the runtime, Protocol and NXConstantString.

   Because Objective-C allows multiple root classes, you can define
   your own root class, different from Object.

   In particular, a Foundation library (such as GNUstep Base) is
   expected to provide its own root class (typically called NSObject),
   fully integrated with the library's own high-level features.  It is
   expected that you should always use and interact with NSObject, and
   mostly ignore Object.  */

/* All classes are derived from Object.  As such, this is the overhead
   tacked onto those objects.  */
@interface Object
{
  Class isa; /* A pointer to the instance's class structure.  */
}
- (Class)class;
- (BOOL)isEqual: (id)anObject;
@end

/* All of the following includes were deprecated in GCC 4.6 and will
   be removed in the next release.  */
#include "deprecated/hash.h"
#include "deprecated/typedstream.h"
#include "deprecated/Object.h"

#ifdef __cplusplus
}
#endif

#endif
