/* go-strerror.c -- wrapper around XSI-compliant strerror_r.

   Copyright 2022 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* There are two version of strerror_r on GNU/Linux: a GNU-specific
   and an XSI-compliant version.  The former version is only available
   on glibc.  Since glibc 2.13, the XSI-compliant version is also
   provided by glibc if _GNU_SOURCE is not defined.  Since the
   entirety of gofrontend is compiled with _GNU_SOURCE, this file
   exists to selectively undefine it and provides an alias to the
   XSI-compliant version of strerror_r(3).  */

#if defined(__linux__) || defined(__gnu_hurd__)

/* Force selection of XSI-compliant strerror_r by glibc.  */
#undef XOPEN_SOURCE
#define XOPEN_SOURCE 600
#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200112L
#undef _GNU_SOURCE

#endif /* defined(__linux__) || defined(__gnu_hurd__) */

#include <string.h>

#ifndef HAVE_STRERROR_R
// Provided by go-nosys.c if not provided by libc itself.
extern int strerror_r (int, char *, size_t);
#endif

int
go_strerror (int errnum, char *buf, size_t buflen)
{
  return strerror_r (errnum, buf, buflen);
}
