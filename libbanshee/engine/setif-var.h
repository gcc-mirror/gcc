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

#ifndef SETIF_VAR_H
#define SETIF_VAR_H

#include "linkage.h"
#include "banshee.h"
#include "jcollection.h"

EXTERN_C_BEGIN

typedef struct setif_var *setif_var;

DECLARE_LIST(setif_var_list,setif_var)
    
bool sv_lt(setif_var v1, setif_var v2);
bool sv_eq(setif_var v1, setif_var v2);
setif_var sv_fresh(region r, const char *name);
setif_var sv_fresh_large(region r, const char *name);
setif_var sv_fresh_small(region r, const char *name);
stamp sv_get_stamp(setif_var v);
const char *sv_get_name(setif_var v);
void sv_unify(setif_var v,setif_var_list vars);
gen_e_list sv_get_lbs(setif_var v);
gen_e_list sv_get_ubs(setif_var v);
bool sv_add_ub(setif_var v, gen_e e, stamp st);
bool sv_add_lb(setif_var v, gen_e e, stamp st);
bool sv_is_ub(setif_var v, stamp st);
bool sv_is_lb(setif_var v, stamp st);
void sv_set_tlb_cache(setif_var v, jcoll j);
jcoll sv_get_tlb_cache(setif_var v);
void sv_clear_tlb_cache(setif_var v);
void sv_add_ub_proj(setif_var v, gen_e e);

gen_e sv_get_ub_proj(setif_var v, get_proj_fn_ptr get_proj);
gen_e_list sv_get_ub_projs(setif_var v);

bool sv_union_component(setif_var v1, setif_var v2);

EXTERN_C_END


#endif /* SETIF_VAR_H */

