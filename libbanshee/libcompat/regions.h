/*
 * Copyright (c) 1999-2001
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
#ifndef REGIONS_H
#define REGIONS_H

#define RPAGELOG 13

typedef struct Region *region;

#include <stdlib.h>

void region_init(void); /* You MUST call this function before using any
			   part of this library */

region newregion(void);
region newsubregion(region parent);

typedef int type_t;
#define rctypeof(type) 0

/* Low-level alloc with dynamic type info */
void *typed_ralloc(region r, size_t size, type_t type);
void *typed_rarrayalloc(region r, size_t n, size_t size, type_t type);
void *typed_rarrayextend(region r, void *old, size_t n, size_t size, type_t type);
void typed_rarraycopy(void *to, void *from, size_t n, size_t size, type_t type);

#define ralloc(r, type) typed_ralloc((r), sizeof(type), rctypeof(type))
#define rarrayalloc(r, n, type) typed_rarrayalloc((r), (n), sizeof(type), rctypeof(type))
#define rarrayextend(r, old, n, type) typed_rarrayextend((r), (old), (n), sizeof(type), rctypeof(type))
#define rarraycopy(to, from, n, type) typed_rarraycopy((to), (from), (n), sizeof(type), rctypeof(type))

char *rstralloc(region r, size_t size);
char *rstralloc0(region r, size_t size);
char *rstrdup(region r, const char *s);

/* rstrextend is used to extend an old string. The string MUST have been
   initially allocated by a call to rstrextend with old == NULL (you cannot
   initially allocate the string with rstralloc) */
char *rstrextend(region r, const char *old, size_t newsize);
char *rstrextend0(region r, const char *old, size_t newsize);

void deleteregion(region r);
void deleteregion_ptr(region *r);
void deleteregion_array(int n, region *regions);
region regionof(void *ptr);

typedef void (*nomem_handler)(void);
nomem_handler set_nomem_handler(nomem_handler newhandler);

/* Debugging support */
void findrefs(region r, void *from, void *to);
void findgrefs(region r);
void findrrefs(region r, region from);

#ifdef REGION_PROFILE
#include "profile.h"
#endif

#endif
