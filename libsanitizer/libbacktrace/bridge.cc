/* bridge.cc -- extern "C" wrappers around sanitizer_common APIs.
   Copyright (C) 2013 Free Software Foundation, Inc.
   Written by Jakub Jelinek, Red Hat, Inc.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    (1) Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer. 

    (2) Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.  
    
    (3) The name of the author may not be used to
    endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.  */

#include "config.h"

#include <string.h>

#include "sanitizer_common/sanitizer_allocator_internal.h"

extern "C"
{

void *
__asan_internal_memcpy (void *dest, const void *src, size_t n)
{
  return __sanitizer::internal_memcpy (dest, src, n);
}

void *
__asan_internal_memset (void *dest, int c, size_t n)
{
  return __sanitizer::internal_memset (dest, c, n);
}

int
__asan_internal_memcmp (const void *s1, const void *s2, size_t n)
{
  return __sanitizer::internal_memcmp (s1, s2, n);
}

int
__asan_internal_strcmp (const char *s1, const char *s2)
{
  return __sanitizer::internal_strcmp (s1, s2);
}

int
__asan_internal_strncmp (const char *s1, const char *s2, size_t n)
{
  return __sanitizer::internal_strncmp (s1, s2, n);
}

size_t
__asan_internal_strlen (const char *str)
{
  return __sanitizer::internal_strlen (str);
}

size_t
__asan_internal_strnlen (const char *str, size_t n)
{
  return __sanitizer::internal_strnlen (str, n);
}

}
