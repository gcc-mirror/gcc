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
#include "banshee.h"
#include "flow-var.h"
#include "ufind.h"
#include "bounds.h"

DECLARE_UFIND(contour_elt,contour)

struct flow_var /* extends gen_e */
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type; /* alias or var */
  stamp st;
  gen_e alias;
  bounds sameregion ubs;
  bounds sameregion lbs;
  contour_elt elt; 
  const char *name;
};

DEFINE_UFIND(contour_elt,contour)
DEFINE_LIST(flow_var_list, flow_var)

#define get_contour(x) (contour_elt_get_info((x)->elt))

static flow_var make_var(region r, const char *name, stamp st)
{
  flow_var result = ralloc(r,struct flow_var);

  result->type = VAR_TYPE;
  result->st = st;
  result->alias = NULL;
  result->ubs = bounds_create(r);
  result->lbs = bounds_create(r);
  result->elt = new_contour_elt(r,NULL);
  result->name = name;

#ifdef NONSPEC
  result->sort = flow_sort;
#endif

  return result;
}

flow_var fv_fresh(region r, const char *name)
{
  return make_var(r,name,stamp_fresh());
}

flow_var fv_fresh_large(region r, const char *name)
{
  return make_var(r,name,stamp_fresh_large());
}

flow_var fv_fresh_small(region r, const char *name)
{
  return make_var(r,name,stamp_fresh_small());
}

const char * fv_get_name(flow_var v)
{
  return v->name;
}

gen_e_list fv_get_lbs(flow_var v)
{
  return bounds_exprs(v->lbs);
}

gen_e_list fv_get_ubs(flow_var v)
{
  return bounds_exprs(v->ubs);
}

bool fv_add_ub(flow_var v, gen_e e, stamp st)
{
  return bounds_add(v->ubs,e,st);
}

bool fv_add_lb(flow_var v, gen_e e, stamp st)
{
  return bounds_add(v->lbs,e,st);
}

bool fv_is_ub(flow_var v, stamp st)
{
  bool self_edge = v->st == st,
    in_bounds = bounds_query(v->ubs,st);

  return (self_edge || in_bounds);
}

bool fv_is_lb(flow_var v, stamp st)
{
  bool self_edge = v->st == st,
    in_bounds = bounds_query(v->lbs,st);

  return (self_edge || in_bounds);
}

void fv_set_alias(flow_var v, gen_e e)
{
  assert(v->type == VAR_TYPE);

  v->type = ALIAS_TYPE;
  v->alias = e;
}

gen_e fv_get_alias(flow_var v)
{
  return v->alias;
}

bool fv_has_contour(flow_var v)
{
  return (get_contour(v) != NULL);
}

void fv_set_contour(flow_var v, contour c)
{
  contour_elt_update(v->elt,c);
}

static contour combine_contour(contour c1, contour c2)
{
  if (c1 == NULL)
    return c2;
  else if (c2 == NULL)
    return c1;

  else 
    {
      fail("Attempt to unify two distinct contours\n");
      return NULL;
    }

}
void fv_unify_contour(flow_var v1, flow_var v2)
{
  contour_elt_unify(combine_contour,v1->elt,v2->elt);
}


gen_e fv_instantiate_contour(flow_var v) deletes
{
  contour c = get_contour(v);
  return c->instantiate(c->fresh,c->get_stamp,c->shape);
}
