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
#include <regions.h>
#include <assert.h>
#include "ufind.h"
#include "term-var.h"

DECLARE_UFIND(tv_elt,gen_e)
 
DEFINE_UFIND(tv_elt,gen_e)

DEFINE_LIST(term_var_list,term_var)

struct term_var
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
  gen_e_list pending;
  const char *name;
  tv_elt elt;
};

static term_var make_var(region r, const char *name, stamp st)
{
  term_var result = ralloc(r, struct term_var);
  gen_e info = (gen_e) result;

  result->type = VAR_TYPE;
  result->st = st;
  result->pending = new_gen_e_list(r);
  result->name = name ? rstrdup(r,name) : "fv";
  result->elt = new_tv_elt(r,info);

  return result;
}

term_var tv_fresh(region r, const char *name)
{
  return make_var(r,name,stamp_fresh());
}

term_var tv_fresh_small(region r, const char *name)
{
  return make_var(r,name,stamp_fresh_small());
}

term_var tv_fresh_large(region r, const char *name)
{
  return make_var(r,name,stamp_fresh_large());
}

static term_var tv_get_v_ecr(term_var v)
{
  term_var ecr = (term_var)tv_get_ecr(v);
  assert (ecr->type == VAR_TYPE); /* this is a hack, but should be ok */

  return ecr;
}

const char *tv_get_name(term_var v)
{
  return tv_get_v_ecr(v)->name;
}

gen_e_list tv_get_pending(term_var v)
{
  return tv_get_v_ecr(v)->pending;
}

void tv_add_pending(term_var v,gen_e e)
{
  gen_e_list_cons(e,tv_get_v_ecr(v)->pending);
}

void tv_unify(term_var v, gen_e e)
{
  tv_elt_update(v->elt,e);
  
  assert(tv_get_ecr(v) == e);
}

static gen_e tv_combine(gen_e e1, gen_e e2)
{
  term_var v1 = (term_var)e1,
    v2 = (term_var)e2;
  
  if (! (v1 == v2) )
    gen_e_list_append(tv_get_pending(v1), tv_get_pending(v2));
  
  return e1;
}

void tv_unify_vars(term_var v1, term_var v2)
{
  tv_elt_unify(tv_combine,v1->elt, v2->elt);
}

gen_e tv_get_ecr(term_var v)
{
  return tv_elt_get_info(v->elt);
}
