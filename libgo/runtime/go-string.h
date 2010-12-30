/* go-string.h -- the string type for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#ifndef LIBGO_GO_STRING_H
#define LIBGO_GO_STRING_H

#include <stddef.h>

/* A string is an instance of this structure.  */

struct __go_string
{
  /* The bytes.  */
  const unsigned char *__data;
  /* The length.  */
  int __length;
};

static inline _Bool
__go_strings_equal (struct __go_string s1, struct __go_string s2)
{
  return (s1.__length == s2.__length
	  && __builtin_memcmp (s1.__data, s2.__data, s1.__length) == 0);
}

static inline _Bool
__go_ptr_strings_equal (const struct __go_string *ps1,
			const struct __go_string *ps2)
{
  if (ps1 == NULL)
    return ps2 == NULL;
  if (ps2 == NULL)
    return 0;
  return __go_strings_equal (*ps1, *ps2);
}

extern int __go_get_rune (const unsigned char *, size_t, int *);

#endif /* !defined(LIBGO_GO_STRING_H) */
