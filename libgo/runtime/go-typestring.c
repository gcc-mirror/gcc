/* go-typestring.c -- the runtime.typestring function.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"
#include "interface.h"
#include "go-type.h"

String typestring(struct __go_empty_interface) __asm__ (GOSYM_PREFIX "runtime.typestring");

String
typestring (struct __go_empty_interface e)
{
  return *e.__type_descriptor->__reflection;
}
