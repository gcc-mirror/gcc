/*
 * Copyright (c) 2000-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#ifndef UTIL_H
#define UTIL_H

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <regions.h>
#include "compiler.h"
#include "linkage.h"
#include "bool.h"


EXTERN_C_BEGIN

#ifdef HAVE_VARIADIC_MACROS
#define fail(args...) __fail(__FILE__, __LINE__, __FUNCTION__, args)
#else
void fail(const char *fmt, ...);
#endif

void __fail(const char *file, unsigned int line,
	    const char *func, const char *fmt, ...) __attribute__ ((__noreturn__));


/* insist(action) is like assert(action), but action may have
   side-effects */
#ifdef NDEBUG
# define insist(action)  (action)
#else
# define insist assert
#endif

#ifdef NDEBUG
# define insistnot(action)  (action)
#else
# define insistnot(action) assert(!(action))
#endif

void failure(const char *message);

/* Concatenate 2 strings, allocating space in r for the result */
char *rstrcat(region, const char *, const char *);

/* Concatenate n strings, allocating space in r for the result.  The
   last argument should be a null pointer. */
char *rstrscat(region, ...);

/* Convert an integer to a string, storing the result in r */
const char *inttostr(region r, int);

/* sprintf a string, allocating space in r for the result */
char *rsprintf(region r, const char *fmt, ...);
char *rvsprintf(region r, const char *fmt, va_list args);

/* Convert a pointer to an ascii string with leading 0x.  Re-uses
   internal buffer. */
char *ptr_to_ascii(void *ptr);

/* Convert a pointer to an integer */
long ptr_hash(void *ptr);

/* Return TRUE iff ptr1 == ptr2 */
bool ptr_eq(void *ptr1, void *ptr2);

/* Return TRUE iff s1 == s2 */
bool str_eq(const char *s1, const char *s2);

/* A total ordering on pointers.  Returns 0 if ptr1 = ptr2, a value <0
   if ptr1 < ptr2, or a value >0 if ptr1 > ptr2. */
int ptr_cmp(const void *ptr1, const void *ptr2);

extern inline int min(int, int);
extern inline int max(int, int);
extern inline int min(int a, int b) { if (a < b) return a; else return b; }
extern inline int max(int a, int b) { if (a < b) return b; else return a; }
EXTERN_C_END

#endif
