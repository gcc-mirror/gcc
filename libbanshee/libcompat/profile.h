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
#ifndef PROFILE_H
#define PROFILE_H

/* Should only be included in regions.h and profile.c */

void *profile_typed_ralloc(region r, size_t size, type_t type, char *file, int line);
void *profile_typed_rarrayalloc(region r, size_t n, size_t size, type_t type, char *file, int line);
void *profile_typed_rarrayextend(region r, void *old, size_t n, size_t size, type_t type, char *file, int line);
char *profile_rstralloc(region r, size_t size, char *file, int line);
char *profile_rstralloc0(region r, size_t size, char *file, int line);
char *profile_rstrdup(region r, const char *s, char *file, int line);
char *profile_rstrextend(region r, const char *old, size_t newsize, char *file, int line);
char *profile_rstrextend0(region r, const char *old, size_t newsize, char *file, int line);

#ifdef REGION_PROFILE
#define typed_ralloc(r, size, type) profile_typed_ralloc(r, size, type, __FILE__, __LINE__)
#define typed_rarrayalloc(r, n, size, type) profile_typed_rarrayalloc(r, n, size, type, __FILE__, __LINE__)
#define typed_rarrayextend(r, old, n, size, type) profile_typed_rarrayextend(r, old, n, size, type, __FILE__, __LINE__)

#define rstralloc(r, size) profile_rstralloc(r, size, __FILE__, __LINE__)
#define rstralloc0(r, size) profile_rstralloc0(r, size, __FILE__, __LINE__)
#define rstrdup(r, s) profile_rstrdup(r, s, __FILE__, __LINE__)

#define rstrextend(r, old, newsize) profile_rstrextend(r, old, newsize, __FILE__, __LINE__)
#define rstrextend0(r, old, newsize) profile_rstrextend0(r, old, newsize, __FILE__, __LINE__)
#endif

void profile(void);

#endif
