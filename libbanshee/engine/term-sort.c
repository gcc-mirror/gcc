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

#include <regions.h>
#include <assert.h>
#include <ansidecl.h>
#include "term-sort.h"

struct term_constant_ /* extends gen_e */
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
  char *name;
};

typedef struct term_constant_ *term_constant_;

region term_sort_region;
term_hash term_sort_hash;
bool flag_occurs_check = FALSE;

struct term_stats term_stats;

stamp term_get_stamp(gen_e e)
{
  if ( ((gen_term)e)->type == VAR_TYPE )
    return ((gen_term)term_get_ecr(e))->st;
  else 
    return ((gen_term)e)->st;
}

gen_e term_fresh(const char *name)
{
  term_stats.fresh++;
  return (gen_e)tv_fresh(term_sort_region,name);
}

gen_e term_fresh_large(const char *name)
{
  term_stats.fresh_large++;
  return (gen_e)tv_fresh_large(term_sort_region,name);
}

gen_e term_fresh_small(const char *name)
{
  term_stats.fresh_small++;
  return (gen_e)tv_fresh_small(term_sort_region,name);
}


#ifdef NONSPEC
static struct gen_term zero = {ZERO_TYPE,term_sort,ZERO_TYPE};
static struct gen_term one  = {ONE_TYPE,term_sort,ONE_TYPE};
#else
static struct gen_term zero = {ZERO_TYPE,ZERO_TYPE};
static struct gen_term one  = {ONE_TYPE,ONE_TYPE};
#endif /* NONSPEC */

gen_e term_zero(void)
{
  return (gen_e)&zero;
}

gen_e term_one(void)
{
  return (gen_e)&one;
}


gen_e term_constant(const char *str)
{
  stamp st[2];
  gen_e result;
  char *name = rstrdup(term_sort_region,str);

  assert (str != NULL);
  
  st[0] = CONSTANT_TYPE;
  st[1] = stamp_string(name); 

  if ( (result = term_hash_find(term_sort_hash,st,2)) == NULL)
    {
      term_constant_ c = ralloc(term_sort_region, struct term_constant_);
      c->type = CONSTANT_TYPE;
      c->st = stamp_fresh();
      c->name = name;

      result = (gen_e) c;
      term_hash_insert(term_sort_hash,result,st,2);
      
      return result;
    }
  
  else
    {
      return result;
    }

}

static bool term_is_bottom(gen_e e)
{
  return (term_is_zero(e) || term_is_var(e));
}

bool term_is_zero(gen_e e)
{
  return ( ((gen_term)term_get_ecr(e))->type == ZERO_TYPE);
}

bool term_is_one(gen_e e)
{
  return ( ((gen_term)term_get_ecr(e))->type == ONE_TYPE);
}

bool term_is_var(gen_e e)
{
  return ( ((gen_term)term_get_ecr(e))->type == VAR_TYPE);
}

bool term_is_constant(gen_e e)
{
  return ( ((gen_term)term_get_ecr(e))->type == CONSTANT_TYPE);
}

char *term_get_constant_name(gen_e e)
{
  gen_e ecr = term_get_ecr(e);
  if(! term_is_constant(ecr))
    return NULL;
  else
    return ((term_constant_)ecr)->name;
}

gen_e term_get_ecr(gen_e e)
{
  if (((gen_term)e)->type == VAR_TYPE)
    return tv_get_ecr((term_var)e);
  else return e;
}

static void fire_pending(term_var v, gen_e e, 
			 con_match_fn_ptr con_match, 
			 occurs_check_fn_ptr occurs)
{
  gen_e_list_scanner scan;
  gen_e temp;

  gen_e_list_scan(tv_get_pending(v),&scan);
  while (gen_e_list_next(&scan,&temp))
    {
      term_unify(con_match,occurs,temp,e);
    }
}

static bool eq(gen_e e1, gen_e e2)
{
  return term_get_ecr(e1) == term_get_ecr(e2);
}

void term_unify(con_match_fn_ptr con_match, occurs_check_fn_ptr occurs,
		gen_e a, gen_e b)
{
  gen_e e1 = term_get_ecr(a),
    e2 = term_get_ecr(b);

  if ( eq(e1,e2) )
    {
      return;
    }
  if (term_is_constant(e1) && term_is_constant(e2))
    { 
      failure("Inconsistent system of constraints\n");
    }
  else if (term_is_var(e1))
    {
      term_var v = (term_var)e1;
   

      if (! term_is_bottom(e2))
	fire_pending(v,e2,con_match,occurs);

      if (term_is_var(e2)) 
	tv_unify_vars(v,(term_var)e2);
      else /* v = e2, e2 is not a var */
	{ 
	  if (occurs(v,e2))
	    failure("Unify terms: occurs check failed\n");
	  tv_unify(v,e2); 
	}
    }
  else if (term_is_var(e2))
    {
      term_var v = (term_var)e2;

      if (! term_is_bottom(e2))
	fire_pending(v,e1,con_match,occurs);
      
      /* v = e1, e1 is not a var */
      if (occurs(v,e1))
	failure("Unify terms: occurs check failed\n");
      tv_unify(v,e1); 
      
    }
  else con_match(e1,e2);
}

void term_cunify(con_match_fn_ptr con_match, occurs_check_fn_ptr occurs,
		 gen_e e1, gen_e e2)
{
  if (term_is_bottom(e1) && term_is_var(e1))
    {
      term_var v1 = (term_var)e1;
      tv_add_pending(v1,e2);
    }
  else 
    {
      term_unify(con_match,occurs,e1,e2);
    }
}

static void term_reset_stats(void)
{
  term_stats.fresh = 0;
  term_stats.fresh_small = 0;
  term_stats.fresh_large = 0;
}

void term_print_stats(FILE *f)
{
  fprintf(f,"\n========== Term Var Stats ==========\n");
  fprintf(f,"Fresh : %d\n",term_stats.fresh); 
  fprintf(f,"Fresh Small : %d\n",term_stats.fresh_small);
  fprintf(f,"Fresh Large : %d\n",term_stats.fresh_large);
  fprintf(f,"=====================================\n");
}

/* TODO */
void term_print_constraint_graph(FILE *f ATTRIBUTE_UNUSED)
{
}

void term_init(void)
{
  term_sort_region = newregion();
  term_sort_hash = make_term_hash(term_sort_region);
}

void term_reset(void)
{
  term_hash_delete(term_sort_hash);
  deleteregion_ptr(&term_sort_region);
 
  term_reset_stats();
 
  term_sort_region = newregion();
  term_sort_hash = make_term_hash(term_sort_region);
}



