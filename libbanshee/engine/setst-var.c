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
#include "setst-var.h"
#include "jcollection.h"
#include "ufind.h"
#include "bounds.h"

struct st_info
{
  stamp st;
  bounds lbs;
  bounds sources;
  bounds sinks;
  jcoll tlb_cache;
  const char *name;
  bool seen;
  int path_pos;
  int src_sz;
  int snk_sz;
};

typedef struct st_info *st_info;

DECLARE_UFIND(st_elt,st_info)

DEFINE_UFIND(st_elt,st_info)

DEFINE_LIST(setst_var_list,setst_var)

#define get_info(v) (st_elt_get_info((v)->elt))

struct setst_var /* extends gen_e */
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  st_elt elt;
};

bool st_eq(setst_var v1, setst_var v2)
{
  return (st_get_stamp(v1) == st_get_stamp(v2));
}

static setst_var make_var(region r, const char *name, stamp st)
{
  setst_var result = ralloc(r,struct setst_var);
  st_info info = ralloc(r, struct st_info);

  info->st = st;
  info->lbs = bounds_create(r);
  info->sources = bounds_create(r);
  info->sinks = bounds_create(r);
  info->tlb_cache = NULL;
  info->name = name ? rstrdup(r,name) : "fv";
  info->seen = FALSE;
  info->path_pos = 0;
  info->src_sz = 0;
  info->snk_sz = 0;

  result->type = VAR_TYPE;
  result->elt = new_st_elt(r,info);


#ifdef NONSPEC
  result->sort = setst_sort;
#endif

  return result;
}

setst_var st_fresh(region r, const char *name)
{
  return make_var(r,name,stamp_fresh());
}

setst_var st_fresh_large(region r, const char *name)
{
  return make_var(r,name,stamp_fresh_large());
}

setst_var st_fresh_small(region r, const char *name)
{
  return make_var(r,name,stamp_fresh_small());
}

stamp st_get_stamp(setst_var v)
{
  return get_info(v)->st;
}

const char *st_get_name(setst_var v)
{
  return get_info(v)->name;
}

void st_unify(setst_var v,setst_var_list vars)
{
  setst_var temp;
  setst_var_list_scanner scan;

  setst_var_list_scan(vars,&scan);

  while (setst_var_list_next(&scan,&temp)) 
    {
      st_elt_union(v->elt,temp->elt);
    }
}

setst_var_list st_get_lbs(setst_var v)
{
  return (setst_var_list)bounds_exprs(get_info(v)->lbs);
}

gen_e_list st_get_sources(setst_var v)
{
  return bounds_exprs(get_info(v)->sources);
}

gen_e_list st_get_sinks(setst_var v)
{
  return bounds_exprs(get_info(v)->sinks);
}

bool st_add_lb(setst_var v, setst_var lb)
{
  return bounds_add(get_info(v)->lbs,(gen_e)lb,st_get_stamp(lb));
}

bool st_add_source(setst_var v, gen_e source, stamp s)
{
  return bounds_add(get_info(v)->sources,source,s);
}

bool st_add_sink(setst_var v, gen_e sink, stamp s)
{
  return bounds_add(get_info(v)->sinks,sink,s);
}

jcoll st_get_tlb_cache(setst_var v)
{
  return get_info(v)->tlb_cache;
}

void st_set_tlb_cache(setst_var v, jcoll j)
{
  get_info(v)->tlb_cache = j;
}

void st_clear_tlb_cache(setst_var v)
{
  get_info(v)->tlb_cache = NULL;
}

gen_e st_get_ub_proj(setst_var v, get_proj_fn_ptr get_proj)
{
  return get_proj(st_get_sinks(v));
}
static setst_var neq_temp;
static bool neq (const setst_var v2)
{
  return (!(st_get_stamp (neq_temp) == st_get_stamp (v2)));
}
void st_repair_bounds(setst_var v1)
{
  setst_var_list lbs;
  neq_temp = v1;
  lbs = setst_var_list_filter2(st_get_lbs(v1),neq);  

  bounds_set(get_info(v1)->lbs,(gen_e_list)lbs);
}

void st_set_path_pos(setst_var v, int pos)
{
  get_info(v)->path_pos = pos;
}

int st_get_path_pos(setst_var v)
{
  return get_info(v)->path_pos;
}

void st_set_seen(setst_var v, bool b)
{
  get_info(v)->seen = b;
}

bool st_get_seen(setst_var v)
{
  return get_info(v)->seen;
}

void st_set_src_sz(setst_var v, int size)
{
  get_info(v)->src_sz = size;
}

int st_get_src_sz(setst_var v)
{
  return get_info(v)->src_sz;
}

void st_set_snk_sz(setst_var v, int size)
{
  get_info(v)->snk_sz = size;
}

int st_get_snk_sz(setst_var v)
{
  return get_info(v)->snk_sz;
}






