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

#ifndef ARRAY_H
#define ARRAY_H

/* A region-based growable array type */

struct array;

struct array *new_array(region r, size_t initialsize,
			size_t typesize, type_t typeinfo);
void *array_extend(struct array *a, int by);
void array_reset(struct array *a);
size_t array_length(struct array *a);
void *array_data(struct array *a);


#define DECLARE_ARRAY(name, type) \
typedef struct name ## _a *name; \
name new_ ## name(region r, size_t initialsize); \
type *name ## _extend(name a, int by); \
void name ## _reset(name a); \
size_t name ## _length(name a); \
type *name ## _data(name a);

#define DEFINE_ARRAY(name, type) \
name new_ ## name(region r, size_t initialsize) \
{ \
  return (name)new_array(r, initialsize, sizeof(type), rctypeof(type)); \
} \
type *name ## _extend(name a, int by) \
{ \
  return array_extend((struct array *)a, by); \
} \
void name ## _reset(name a) \
{ \
  return array_reset((struct array *)a); \
} \
size_t name ## _length(name a) \
{ \
  return array_length((struct array *)a); \
} \
type *name ## _data(name a) \
{ \
  return array_data((struct array *)a); \
}

#endif
