/* go-typestring.c -- the runtime.typestring function.

   Copyright 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "interface.h"
#include "go-type.h"
#include "go-string.h"

struct __go_string typestring(struct __go_empty_interface)
  asm ("runtime.typestring");

struct __go_string
typestring (struct __go_empty_interface e)
{
  return *e.__type_descriptor->__reflection;
}
