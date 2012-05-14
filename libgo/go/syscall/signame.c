/* signame.c -- get the name of a signal

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <string.h>

#include "config.h"
#include "runtime.h"
#include "arch.h"
#include "malloc.h"

String Signame (int sig) asm ("syscall.Signame");

String
Signame (int sig)
{
  const char* s = NULL;
  char buf[100];
  size_t len;
  unsigned char *data;
  String ret;

#if defined(HAVE_STRSIGNAL)
  s = strsignal (sig);
#endif

  if (s == NULL)
    {
      snprintf(buf, sizeof buf, "signal %d", sig);
      s = buf;
    }
  len = __builtin_strlen (s);
  data = runtime_mallocgc (len, FlagNoPointers, 0, 0);
  __builtin_memcpy (data, s, len);
  ret.__data = data;
  ret.__length = len;
  return ret;
}
