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

#ifndef SETST_VAR_H
#define SETST_VAR_H

#include "linkage.h"
#include "banshee.h"
#include "jcollection.h"

EXTERN_C_BEGIN

typedef struct setst_var *setst_var;

DECLARE_LIST(setst_var_list,setst_var)

bool st_eq(setst_var v1, setst_var v2);
setst_var st_fresh(region r, const char *name);
setst_var st_fresh_large(region r, const char *name);
setst_var st_fresh_small(region r, const char *name);
stamp st_get_stamp(setst_var v);
const char *st_get_name(setst_var v);
void st_unify(setst_var v,setst_var_list vars);
setst_var_list st_get_lbs(setst_var v); 
gen_e_list st_get_sources(setst_var v);
gen_e_list st_get_sinks(setst_var v);
gen_e st_get_ub_proj(setst_var v, get_proj_fn_ptr get_proj);
bool st_add_lb(setst_var v, setst_var lb);
bool st_add_source(setst_var v, gen_e source, stamp s);
bool st_add_sink(setst_var v, gen_e sink, stamp s);

void st_set_path_pos(setst_var v, int pos);
int st_get_path_pos(setst_var v);
void st_set_seen(setst_var v, bool b);
bool st_get_seen(setst_var v);
void st_set_src_sz(setst_var v, int size);
int st_get_src_sz(setst_var v);
void st_set_snk_sz(setst_var v, int size);
int st_get_snk_sz(setst_var v);

jcoll st_get_tlb_cache(setst_var v);
void st_set_tlb_cache(setst_var v, jcoll j);
void st_clear_tlb_cache(setst_var v);

void st_repair_bounds(setst_var v);

EXTERN_C_END

#endif /* SETST_VAR_H */

