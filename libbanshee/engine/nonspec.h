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

#ifndef NONSPEC_H
#define NONSPEC_H

#include <stdio.h>

EXTERN_C_BEGIN

typedef enum 
{
  vnc_pos,
  vnc_neg,
  vnc_non
} vnc_kind;

struct sig_elt
{
  vnc_kind variance;
  sort_kind sort;
};

typedef struct sig_elt sig_elt;
typedef struct constructor *constructor;

struct flowrow_field
{
  char *label;
  gen_e expr;
};

DECLARE_LIST(flowrow_map,flowrow_field)

/*
   Flags
*/
extern bool flag_merge_projections;
extern bool flag_eliminate_cycles;
extern bool flag_occurs_check;

/* 
   Operations for building terms
*/

/* Defines a new constructor for sort s with the given signature */
constructor make_constructor(const char *name,sort_kind sort, sig_elt[],
			     int arity);

/* Build the term c(exps[0]....exps[n]) */
gen_e constructor_expr(constructor c, gen_e exps[], int arity);

/* make a constant of sort s */
gen_e setif_constant(const char *name);

gen_e setst_constant(const char *name);

gen_e term_constant(const char *name);

/* Creates a projection pattern projpat(c,i,e) */
gen_e setif_proj_pat(constructor c,int i,gen_e e);

gen_e setst_proj_pat(constructor c, int i, gen_e e);

/* Adds a constraint e <= projpat(c,i,fv) where fv is a fresh variable */
gen_e setif_proj(constructor c, int i, gen_e e);

gen_e setst_proj(constructor c, int i, gen_e e);

/* Make a new variable of sort s */
gen_e setif_fresh(const char *name);

gen_e term_fresh(const char *name);

gen_e flowrow_fresh(const char *name);

gen_e setst_fresh(const char *name);

/* Operations for unions */

gen_e setif_union(gen_e exps[]);

gen_e setif_inter(gen_e exps[]);

gen_e setst_union(gen_e exps[]);

gen_e setst_inter(gen_e exps[]);

/* Empty set of sort s */
gen_e setif_zero(void);

gen_e setst_zero(void);

gen_e flowrow_zero(sort_kind base_sort);

gen_e term_zero(void);

/* Universal set of sort s */
gen_e setif_one(void);

gen_e setst_one(void);

gen_e flowrow_one(sort_kind base_sort);

gen_e term_one(void);

/*
  Operations for building flowrows 
*/

/* Closed flowrow of base sort s */
gen_e flowrow_abs(sort_kind base_sort);

/* Wild flowrow of base sort s */
gen_e flowrow_wild(sort_kind base_sort);

/* Build a flowrow of <l : e>_fields o <rest>  */
gen_e flowrow_row(flowrow_map fields, gen_e rest);

/* 
   Inclusion functions
*/
void call_setif_inclusion(gen_e e1,gen_e e2);
void call_setif_unify(gen_e e1, gen_e e2);

void call_setst_inclusion(gen_e e1, gen_e e2);
void call_setst_unify(gen_e e1, gen_e e2);

void call_flowrow_inclusion(gen_e e1,gen_e e2);
void call_flowrow_unify(gen_e e1, gen_e e2);

void call_term_unify(gen_e e1, gen_e e2);
void call_term_cunify(gen_e e1, gen_e e2);

/*
  Extracting solutions 
 */
struct decon
{
  int arity;
  gen_e[1];
};
struct decon deconstruct_expr(constructor c,gen_e e);

gen_e_list setif_tlb(gen_e e);

gen_e_list setst_tlb(gen_e e);

gen_e term_get_ecr(gen_e e);

gen_e flowrow_extract_field(const char *label,gen_e row);
flowrow_map flowrow_extract_fields(gen_e row);
gen_e flowrow_extract_rest(gen_e row);

void nonspec_init(void);
void nonspec_reset(void);

void expr_print(FILE *f,gen_e e);

EXTERN_C_END

#endif /* NONSPEC_H */
