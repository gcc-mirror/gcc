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

#include <stdio.h>
#include <assert.h>
#include <regions.h>
#include "setif-var.h"
#include "ufind.h"
#include "bounds.h"

struct sv_info
{
  stamp st;
  bounds sameregion lbs;
  bounds sameregion ubs;
  jcoll tlb_cache;
  gen_e_list ub_projs;
  const char *name;
  uf_element component;
};

typedef struct sv_info *sv_info;

DECLARE_UFIND(sv_elt,sv_info)
 
DEFINE_UFIND(sv_elt,sv_info)

DEFINE_LIST(setif_var_list,setif_var)

#define get_info(v) (sv_elt_get_info((v)->elt))

struct setif_var /* extends gen_e */
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  sv_elt elt;
};
    
bool sv_lt(setif_var v1, setif_var v2)
{
  return ( sv_get_stamp(v1) < sv_get_stamp(v2) );
}

bool sv_eq(setif_var v1, setif_var v2)
{
  return ( sv_get_stamp(v1) == sv_get_stamp(v2) );
}

static setif_var make_var(region r, const char *name, stamp st)
{
  setif_var result = ralloc(r, struct setif_var);
  sv_info info = ralloc(r, struct sv_info);
 
  info->st = st;
  info->lbs = bounds_create(r);
  info->ubs = bounds_create(r);
  info->tlb_cache = NULL;
  info->ub_projs = new_gen_e_list(r);
  info->name = name ? rstrdup(r,name) : "fv";
  info->component = new_uf_element(r, NULL);

  result->type = VAR_TYPE;
  result->elt = new_sv_elt(r,info); 
  
#ifdef NONSPEC
  result->sort = setif_sort;
#endif  
  
  return result;
}

setif_var sv_fresh(region r, const char *name)
{
  return make_var(r,name,stamp_fresh());
}

setif_var sv_fresh_large(region r, const char *name)
{
  return make_var(r,name,stamp_fresh_large());
}

setif_var sv_fresh_small(region r, const char *name)
{
  return make_var(r,name,stamp_fresh_small());
}

stamp sv_get_stamp(setif_var v)
{
  return get_info(v)->st;
}

const char *sv_get_name(setif_var v)
{
  return get_info(v)->name;
}


static sv_info combine(sv_info i1, sv_info i2)
{
  if (i1->st < i2->st)
    return i1;
  else return i2;
}

void sv_unify(setif_var v,setif_var_list vars)
{
  setif_var temp;
  setif_var_list_scanner scan;
  
  setif_var_list_scan(vars,&scan);

  while (setif_var_list_next(&scan,&temp))
    {
      sv_elt_unify(combine,v->elt,temp->elt);
    }
}

gen_e_list sv_get_lbs(setif_var v)
{
  return bounds_exprs(get_info(v)->lbs);
}

gen_e_list sv_get_ubs(setif_var v)
{
  return bounds_exprs(get_info(v)->ubs);
}

bool sv_add_ub(setif_var v, gen_e e, stamp s)
{
  return bounds_add(get_info(v)->ubs,e,s);
}

bool sv_add_lb(setif_var v, gen_e e, stamp s)
{
  return bounds_add(get_info(v)->lbs,e,s);
}

bool sv_is_ub(setif_var v, stamp s)
{
  bool self_edge = sv_get_stamp(v) == s,
    in_bounds = bounds_query(get_info(v)->ubs,s);
  
  return (self_edge || in_bounds);
}

bool sv_is_lb(setif_var v, stamp s)
{

  bool self_edge = sv_get_stamp(v) == s,
    in_bounds = bounds_query(get_info(v)->lbs,s);
  
  return (self_edge || in_bounds);

}

void sv_add_ub_proj(setif_var v, gen_e e)
{
  gen_e_list_cons(e,get_info(v)->ub_projs);
}

gen_e sv_get_ub_proj(setif_var v, get_proj_fn_ptr get_proj)
{
  return get_proj(get_info(v)->ub_projs);
}

gen_e_list sv_get_ub_projs(setif_var v)
{
  return get_info(v)->ub_projs;
}


bool sv_union_component(setif_var v1, setif_var v2)
{
  if (uf_eq(get_info(v1)->component,get_info(v2)->component))
    return FALSE;
  
  else
    {
      uf_union(get_info(v1)->component,get_info(v2)->component);
      return TRUE;
    }
}

void sv_set_tlb_cache(setif_var v, jcoll j)
{
  get_info(v)->tlb_cache = j;
}

jcoll sv_get_tlb_cache(setif_var v)
{
  return get_info(v)->tlb_cache;
}

void sv_clear_tlb_cache(setif_var v)
{
  get_info(v)->tlb_cache = NULL;
}
