/* go-fieldtrack.c -- structure field data analysis.

   Copyright 2012 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "runtime.h"
#include "go-type.h"

/* The compiler will track fields that have the tag go:"track".  Any
   function that refers to such a field will call this function with a
   string
       fieldtrack "package.type.field"

   This function does not actually do anything.  Instead, we gather
   the field tracking information by looking for strings of that form
   in the read-only data section.  This is, of course, a horrible
   hack, but it's good enough for now.  We can improve it, e.g., by a
   linker plugin, if this turns out to be useful.  */

void
__go_fieldtrack (byte *p __attribute__ ((unused)))
{
}

/* A runtime function to add all the tracked fields to a
   map[string]bool.  */

extern const char _etext[] __attribute__ ((weak));
extern const char _edata[] __attribute__ ((weak));
#ifdef _AIX
// Following symbols do not exist on AIX
const char *__etext = NULL;
const char *__data_start = NULL;
const char *__edata = NULL;
const char *__bss_start = NULL;
#else
extern const char __etext[] __attribute__ ((weak));
extern const char __data_start[] __attribute__ ((weak));
extern const char __edata[] __attribute__ ((weak));
extern const char __bss_start[] __attribute__ ((weak));
#endif

extern void *mapassign (const struct __go_map_type *, void *hmap,
			const void *key)
  __asm__ (GOSYM_PREFIX "runtime.mapassign");

// The type descriptor for map[string] bool.  */
extern const char map_string_bool[] __attribute__ ((weak));
extern const char map_string_bool[]
  __asm__ (GOSYM_PREFIX "type..map.6string.7bool");

void runtime_Fieldtrack (void *) __asm__ (GOSYM_PREFIX "runtime.Fieldtrack");

void
runtime_Fieldtrack (void *m)
{
  const char *p;
  const char *pend;
  const char *prefix;
  size_t prefix_len;

  if (map_string_bool == NULL)
    return;

  p = __data_start;
  if (p == NULL)
    p = __etext;
  if (p == NULL)
    p = _etext;
  if (p == NULL)
    return;

  pend = __edata;
  if (pend == NULL)
    pend = _edata;
  if (pend == NULL)
    pend = __bss_start;
  if (pend == NULL)
    return;

  prefix = "fieldtrack ";
  prefix_len = __builtin_strlen (prefix);

  while (p < pend)
    {
      const char *q1;
      const char *q2;

      q1 = __builtin_memchr (p + prefix_len, '"', pend - (p + prefix_len));
      if (q1 == NULL)
	break;

      if (__builtin_memcmp (q1 - prefix_len, prefix, prefix_len) != 0)
	{
	  p = q1 + 1;
	  continue;
	}

      q1++;
      q2 = __builtin_memchr (q1, '"', pend - q1);
      if (q2 == NULL)
	break;

      if (__builtin_memchr (q1, '\0', q2 - q1) == NULL)
	{
	  String s;
	  void *p;

	  s.str = (const byte *) q1;
	  s.len = q2 - q1;
	  p = mapassign((const void*) map_string_bool, m, &s);
	  *(_Bool*)p = 1;
	}

      p = q2;
    }
}
