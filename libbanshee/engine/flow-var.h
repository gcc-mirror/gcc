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

#ifndef FLOW_VAR_H
#define FLOW_VAR_H

#include "linkage.h"
#include "banshee.h"
#include "jcollection.h"

EXTERN_C_BEGIN

typedef struct flow_var *flow_var;

typedef gen_e (*contour_inst_fn_ptr) (fresh_fn_ptr,get_stamp_fn_ptr,gen_e) deletes;

struct contour
{
  gen_e shape;
  fresh_fn_ptr fresh;
  get_stamp_fn_ptr get_stamp;
  contour_inst_fn_ptr instantiate;
};

typedef struct contour *contour;

DECLARE_LIST(flow_var_list, flow_var)
   
flow_var fv_fresh(region r, const char *name);
flow_var fv_fresh_large(region r, const char *name);
flow_var fv_fresh_small(region r, const char *name);
const char * fv_get_name(flow_var v);
gen_e_list fv_get_lbs(flow_var v);
gen_e_list fv_get_ubs(flow_var v);
bool fv_add_ub(flow_var v, gen_e e, stamp st);
bool fv_add_lb(flow_var v, gen_e e, stamp st);
bool fv_is_ub(flow_var v, stamp st);
bool fv_is_lb(flow_var v, stamp st);

void fv_set_alias(flow_var v, gen_e e);
gen_e fv_get_alias(flow_var v);
void fv_set_contour(flow_var v, contour c);
bool fv_has_contour(flow_var v);
void fv_unify_contour(flow_var v1, flow_var v2);
gen_e fv_instantiate_contour(flow_var v) deletes;

EXTERN_C_END

#endif /* FLOW_VAR_H */
