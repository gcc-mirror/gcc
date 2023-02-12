/* Declarations of functions and data types used for SHA1 sum
   library functions.
   Copyright (C) 2000-2023 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef SHA1_H
# define SHA1_H 1

#include <stdio.h>

#if defined HAVE_LIMITS_H || _LIBC
# include <limits.h>
#endif

#include "ansidecl.h"

/* The following contortions are an attempt to use the C preprocessor
   to determine an unsigned integral type that is 32 bits wide.  An
   alternative approach is to use autoconf's AC_CHECK_SIZEOF macro, but
   doing that would require that the configure script compile and *run*
   the resulting executable.  Locally running cross-compiled executables
   is usually not possible.  */

#ifdef _LIBC
# include <sys/types.h>
typedef u_int32_t sha1_uint32;
typedef uintptr_t sha1_uintptr;
#elif defined (HAVE_SYS_TYPES_H) && defined (HAVE_STDINT_H)
#include <stdint.h>
#include <sys/types.h>
typedef uint32_t sha1_uint32;
typedef uintptr_t sha1_uintptr;
#else
#  define INT_MAX_32_BITS 2147483647

/* If UINT_MAX isn't defined, assume it's a 32-bit type.
   This should be valid for all systems GNU cares about because
   that doesn't include 16-bit systems, and only modern systems
   (that certainly have <limits.h>) have 64+-bit integral types.  */

# ifndef INT_MAX
#  define INT_MAX INT_MAX_32_BITS
# endif

# if INT_MAX == INT_MAX_32_BITS
   typedef unsigned int sha1_uint32;
# else
#  if SHRT_MAX == INT_MAX_32_BITS
    typedef unsigned short sha1_uint32;
#  else
#   if LONG_MAX == INT_MAX_32_BITS
     typedef unsigned long sha1_uint32;
#   else
     /* The following line is intended to evoke an error.
        Using #error is not portable enough.  */
     "Cannot determine unsigned 32-bit data type."
#   endif
#  endif
# endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Structure to save state of computation between the single steps.  */
struct sha1_ctx
{
  sha1_uint32 A;
  sha1_uint32 B;
  sha1_uint32 C;
  sha1_uint32 D;
  sha1_uint32 E;

  sha1_uint32 total[2];
  sha1_uint32 buflen;
  sha1_uint32 buffer[32];
};


/* Initialize structure containing state of computation. */
extern void sha1_init_ctx (struct sha1_ctx *ctx);

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is necessary that LEN is a multiple of 64!!! */
extern void sha1_process_block (const void *buffer, size_t len,
				struct sha1_ctx *ctx);

/* Starting with the result of former calls of this function (or the
   initialization function update the context for the next LEN bytes
   starting at BUFFER.
   It is NOT required that LEN is a multiple of 64.  */
extern void sha1_process_bytes (const void *buffer, size_t len,
				struct sha1_ctx *ctx);

/* Process the remaining bytes in the buffer and put result from CTX
   in first 20 bytes following RESBUF.  The result is always in little
   endian byte order, so that a byte-wise output yields to the wanted
   ASCII representation of the message digest.

   IMPORTANT: On some systems it is required that RESBUF be correctly
   aligned for a 32 bits value.  */
extern void *sha1_finish_ctx (struct sha1_ctx *ctx, void *resbuf);


/* Put result from CTX in first 20 bytes following RESBUF.  The result is
   always in little endian byte order, so that a byte-wise output yields
   to the wanted ASCII representation of the message digest.

   IMPORTANT: On some systems it is required that RESBUF is correctly
   aligned for a 32 bits value.  */
extern void *sha1_read_ctx (const struct sha1_ctx *ctx, void *resbuf);


/* Compute SHA1 message digest for bytes read from STREAM.  The
   resulting message digest number will be written into the 20 bytes
   beginning at RESBLOCK.  */
extern int sha1_stream (FILE *stream, void *resblock);

/* Compute SHA1 message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
extern void *sha1_buffer (const char *buffer, size_t len, void *resblock);

#ifdef __cplusplus
}
#endif

#endif
