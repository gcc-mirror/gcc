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

#ifndef UFIND_H
#define UFIND_H

#include <regions.h>
#include "linkage.h"
#include "bool.h"

EXTERN_C_BEGIN

struct uf_element;

typedef struct uf_element *uf_element;
typedef void *uf_info;

typedef uf_info (*combine_fn_ptr)(uf_info,uf_info);

struct uf_element *new_uf_element(region r,uf_info i);  
uf_info uf_get_info(struct uf_element *); 
bool uf_unify(combine_fn_ptr,struct uf_element *,struct uf_element *);
bool uf_union(struct uf_element *,struct uf_element *);
bool uf_eq(struct uf_element *,struct uf_element *);
void uf_update(struct uf_element *,uf_info i);

#define DECLARE_UFIND(name,type) \
typedef struct name *name; \
typedef type (* name ## _combine_fn_ptr)(type info1,type info2); \
name new_ ## name(region r, type info); \
type name ## _get_info(name); \
bool name ## _unify(name ## _combine_fn_ptr,name e1, name e2); \
bool name ## _union(name e1, name e2); \
bool name ## _eq(name e1, name e2); \
void name ## _update(name e1, type info);

#define DEFINE_UFIND(name,type) \
name new_ ## name(region r, type info) \
{ \
 return (name)new_uf_element(r,info);\
}\
type name ## _get_info(name elem) \
{ \
 return (type)uf_get_info((struct uf_element *)elem);\
} \
bool name ## _unify(name ## _combine_fn_ptr cmb,name e1, name e2) \
{ \
 return uf_unify((combine_fn_ptr)cmb,(struct uf_element *)e1,(struct uf_element *)e2); \
} \
bool name ## _union(name e1, name e2) \
{ \
 return uf_union((struct uf_element *)e1,(struct uf_element *)e2); \
}\
bool name ## _eq(name e1, name e2) \
{ \
 return uf_eq((struct uf_element *)e1,(struct uf_element *)e2); \
} \
void name ##_update(name e1, type info) \
{ \
 uf_update((struct uf_element *)e1,(uf_info)info); \
} \

EXTERN_C_END

#endif /* UFIND_H */






















































































