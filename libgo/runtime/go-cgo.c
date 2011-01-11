/* go-cgo.c -- SWIG support routines for libgo.

   Copyright 2011 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "go-alloc.h"
#include "interface.h"
#include "go-panic.h"
#include "go-string.h"

/* These are routines used by SWIG.  The gc runtime library provides
   the same routines under the same name, though in that case the code
   is required to import runtime/cgo.  */

void *
_cgo_allocate (size_t n)
{
  return __go_alloc (n);
}

extern const struct __go_type_descriptor string_type_descriptor
  asm ("__go_tdn_string");

void
_cgo_panic (const char *p)
{
  int len;
  unsigned char *data;
  struct __go_string *ps;
  struct __go_empty_interface e;

  len = __builtin_strlen (p);
  data = __go_alloc (len);
  __builtin_memcpy (data, p, len);
  ps = __go_alloc (sizeof *ps);
  ps->__data = data;
  ps->__length = len;
  e.__type_descriptor = &string_type_descriptor;
  e.__object = ps;
  __go_panic (e);
}
