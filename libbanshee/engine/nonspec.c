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

#include "banshee.h"
#include "flowrow-sort.h"
#include "flowrow-var.h"
#include "setif-sort.h"
#include "setif-var.h"
#include "setst-sort.h"
#include "setst-var.h"
#include "term-sort.h"
#include "term-var.h"

struct constructor
{
  sort_kind sort;
  int type;
  int arity;
  char *name;
  sig_elt *sig;
};

typedef struct constructor *constructor;

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

typedef struct proj_pat
{
  sort_kind sort;
  int type;
  stamp st;
  int i;
  gen_e exp;
  vnc_kind variance;
  constructor c;
} *proj_pat;


typedef struct cons_expr  
{
  sort_kind sort;
  int type;
  stamp st;
  int arity;
  char *name;
  sig_elt *sig;
  gen_e *exps;
} * cons_expr;


static int new_type()
{
  static int type = 10;
  int ret = type;
  if (type > 2000)
    {
      fprintf(stderr, "Exceeded maximum number of constructors\n");
      assert(0);
    }
  type += 2;
  return ret;
}

static bool fixed_sort(sort_kind s)
{
  return !(s == sort_term || s == sort_set);
}

/* 
   Convention : constructor types are even, pats are odd.
   The smallest specialized type is 10.
*/
static bool setif_is_pat(gen_e e)
{
  int type = ((setif_term)e)->type;
  return ( (type & 1) && (type > 10) );
}

static bool setst_is_pat(gen_e e)
{
  int type = ((setst_term)e)->type;
  return ( (type & 1) && (type > 10) );
}

static gen_e get_proj_var(sort_kind s, bool large)
{
  switch (s)
    {
    case setif_sort:
      {
	if (large)
	  return (gen_e)sv_fresh_large(get_sort_region(setif_sort),NULL);
	else return (gen_e)sv_fresh(get_sort_region(setif_sort),NULL);	  
      }
      break;
    case setst_sort:
      {
	if (large)
	  return (gen_e)st_fresh_large(get_sort_region(setst_sort),NULL);
	else return (gen_e)st_fresh(get_sort_region(setst_sort),NULL);	
      }
      break;
    case flowrow_sort:
      {
	if (large)
	  return (gen_e)fv_fresh_large(get_sort_region(flowrow_sort),NULL);
	else return (gen_e)fv_fresh(get_sort_region(flowrow_sort),NULL);
      }
      break;
    case term_sort:
      {
	if (large)
	  return (gen_e)tv_fresh_large(get_sort_region(term_sort),NULL);
	else return (gen_e)tv_fresh(get_sort_region(term_sort),NULL)
      }	
      break;
    default:
      {
	fail("Unmatched sort in get_proj_var\n");
	return NULL;
      }
      break;
    }

  return NULL;
}

static gen_e get_sort_zero(sort_kind s)
{
switch (s)
    {
    case setif_sort:
      return setif_zero();
    case setst_sort:
      return setst_zero();
    case flowrow_sort:
      return flowrow_zero();
    case term_sort:
      return term_zero();
    default:
      fail("Unmatched sort in get_sort_zero\n");
      return NULL;
    }
  return NULL;
}

static gen_e get_sort_one(sort_kind s)
{
switch (s)
    {
    case setif_sort:
      return setif_one();
    case setst_sort:
      return setst_one();
    case flowrow_sort:
      return flowrow_one();
    case term_sort:
      return term_one();
    default:
      fail("Unmatched sort in get_sort_zero\n");
      return NULL;
    }
  return NULL;
}

static region get_sort_region(sort s)
{
  switch (s)
    {
    case setif_sort:
      return setif_region;
    case setst_sort:
      return setst_region;
    case flowrow_sort:
      return flowrow_region;
    case term_sort:
      return term_region:
    default:
      fail("Unmatched sort in get_sort_region\n");
      return NULL;
    }
  return NULL;
}

static term_hash get_sort_hash(sort s)
{
  switch (s)
    {
    case setif_sort:
      return setif_hash;
    case setst_sort:
      return setst_hash;
    case flowrow_sort:
      return flowrow_hash;
    case term_sort:
      return term_hash:
    default:
      fail("Unmatched sort in get_sort_hash\n");
      return NULL;
    }
  return NULL;
}

constructor make_constructor(const char *name,sort_kind sort, sig_elt s[],
			     int arity)
{
  constructor c = ralloc(expr_region,struct constructor);
  sig_elt *sig = rarrayalloc(expr_region,arity,sig_elt);
  
  c->type = new_type();

  if (arity)
    {
      memcpy(sig,s,sizeof(sig_elt)*arity);
    }

  if ( fixed_sort(sort) )
    failure("Specified sort does not allow constructor types\n");
  
  c->sort = sort;
  c->arity = arity;
  c->name = rstrdup(expr_region,name);
  c->sig = sig;
  
  return c;
}

gen_e constructor_expr(constructor c, gen_e exps[], int arity)
{
  cons_expr result;
  int i;
  region sort_region = get_sort_region(c->sort);
  term_hash sort_hash = get_sort_hash(c->sort);
  
  stamp *st = rarrayalloc(sort_region,arity + 1,stamp);
  st[0] = c->type;
  
  // Dynamic arity check
  if(arity != c->arity)
      failure("Signature mismatch\n");
  
  // Dynamic sort checks
  for (i = 0; i < arity; i++)
    {
      if ( c->sig[i].sort != exps[i]->sort)
	failure(stderr,"Signature mismatch\n");
      st[i+1] = exps[i]->st;
    }

  // Hash-consing of terms
  if (!(result = term_hash_find(sort_hash,st,arity+1)) || arity == 0 )
    {
      gen_e *e = rarrayalloc(sort_region,arity,gen_e);
      
      if (arity)
	memcpy(e,exps,sizeof(gen_e)*arity);
      else 
	e = NULL;

      result = ralloc(sort_region,struct cons_expr);  
      result->type = st[0];
      result->st = stamp_fresh();
      result->sort = c->sort;
      result->arity = c->arity;
      result->name = c->name;
      result->sig = c->sig;
      result->exps = e;
      
      term_hash_insert(expr_hash,result,st,arity+1);
    }

  return (gen_e)result;
}

static gen_e proj_pat(constructor c, int i, gen_e e)
{
  proj_pat pat;
  region sort_region = get_sort_region(e->sort);
  term_hash sort_hash = get_sort_hash(e->sort);
  
  stamp s[3];
  s[0] = c->type + 1;
  s[1] = e->st;
  s[2] = i;
  
  if (! (pat = term_hash_find(sort_hash,s,3)) )
    {
      pat = ralloc(sort_region,struct proj_pat);
      pat->type = s[0];
      pat->st = stamp_fresh();
      pat->sort = c->sort;
      pat->exp = e;
      pat->variance = c->sig[i].variance;
      pat->c = c;
      pat->i = i;
      term_hash_insert(sort_hash,pat,s,3);
    }
  
  return (gen_e)pat;
}

gen_e setif_proj_pat(constructor c,int i,gen_e e)
{
  return proj_pat(c,i,e);
}

gen_e setst_proj_pat(constructor c, int i, gen_e e)
{
  return proj_pat(c,i,e);
}

gen_e setif_proj(constructor c, int i, gen_e e)
{
  setif_var v;
  gen_e proj_var, proj;
  
  gen_e nonspec_get_proj(gen_e_list arg1)
    {
      proj_pat pat;
      gen_e_list_scanner scan;
      gen_e temp;
      
      gen_e_list_scan(arg1,&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  pat = (proj_pat)temp;
	  if ( pat_match(pat->type,c->type) && i == pat->i )
	    return pat->exp;
	}
      return NULL;
    }
  
  if (e->sort != setif_sort)
    {
      failure("Sort check : setif_proj\n");
    }

  else if (i < 0 || i > c->arity)
    {
      failure("Signature mismatch\n");
    }
  
  else if (setif_is_zero(e))
    return get_sort_zero(c->sig[i].sort);
  
  else if ( ((setif_term)e)->type == c->type )
    {
      cons_expr constructed = (cons_expr)e;
      return constructed->exps[i];
    }
  
  else if (setif_is_var(e))
    {
      v = (setif_var)e;
      if ( (proj = sv_get_ub_proj(v,nonspec_get_proj)) )
	{
	  return proj;
	}
      else
	{
	  gen_e pat;
	  gen_e_list_scanner scan;
	  gen_e lb;
	  proj_var = get_proj_var(c->sig[i].sort,FALSE);
	  pat = setif_proj_pat(c,i,proj_var);
	  sv_add_ub_proj(sort_region,v,pat);
	  
	  gen_e_list_scan(sv_get_lbs(v),&scan);
	  while (gen_e_list_next(&scan,&lb))
	    {
	      setif_inclusion(lb,pat);
	    }
	  return proj_var;
	}
    }

  else if (setif_is_union(e))
    {
      if( (proj = nonspec_get_proj(setif_get_proj_cache(e))) )
	return proj;
      else
	{
	  gen_e pat;
	  proj_var = get_proj_var(c->sig[i].sort,FALSE);
	  pat = setif_proj_pat(c,i,proj_var);
	  
	  setif_set_proj_cache(e,pat);
	  
	  setif_inclusion(e,pat);
	  return proj_var;
	}
    }
  else 
    {
      gen_e pat;
      proj_var = get_proj_var(c->sig[i].sort,FALSE);
      pat = setif_proj_pat(c,i,proj_var);
      setif_inclusion(e,pat);
      return proj_var;
    }
}

gen_e setst_proj(constructor c, int i, gen_e e)
{
  setst_var v;
  gen_e proj_var, proj;
  
  gen_e nonspec_get_proj(gen_e_list arg1)
    {
      proj_pat pat;
      gen_e_list_scanner scan;
      gen_e temp;
      
      gen_e_list_scan(arg1,&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  pat = (proj_pat)temp;
	  if ( pat_match(pat->type,c->type) && i == pat->i )
	    return pat->exp;
	}
      return NULL;
    }
  
  if (e->sort != setst_sort)
    {
      failure("Sort check : setst_proj\n");
    }

  else if (i < 0 || i > c->arity)
    {
      failure("Signature mismatch\n");
    }
  
  else if (setst_is_zero(e))
    return get_sort_zero(c->sig[i].sort);
  
  else if ( ((setst_term)e)->type == c->type )
    {
      cons_expr constructed = (cons_expr)e;
      return constructed->exps[i];
    }
  
  else if (setst_is_var(e))
    {
      v = (setst_var)e;
      if ( (proj = sv_get_ub_proj(v,nonspec_get_proj)) )
	{
	  return proj;
	}
      else
	{
	  gen_e pat;
	  gen_e_list_scanner scan;
	  gen_e lb;
	  proj_var = get_proj_var(c->sig[i].sort,FALSE);
	  pat = setst_proj_pat(c,i,proj_var);
	  sv_add_ub_proj(sort_region,v,pat);
	  
	  gen_e_list_scan(sv_get_lbs(v),&scan);
	  while (gen_e_list_next(&scan,&lb))
	    {
	      setst_inclusion(lb,pat);
	    }
	  return proj_var;
	}
    }

  else if (setst_is_union(e))
    {
      if( (proj = nonspec_get_proj(setst_get_proj_cache(e))) )
	return proj;
      else
	{
	  gen_e pat;
	  proj_var = get_proj_var(c->sig[i].sort,FALSE);
	  pat = setst_proj_pat(c,i,proj_var);
	  
	  setst_set_proj_cache(e,pat);
	  
	  setst_inclusion(e,pat);
	  return proj_var;
	}
    }
  else 
    {
      gen_e pat;
      proj_var = get_proj_var(c->sig[i].sort,FALSE);
      pat = setst_proj_pat(c,i,proj_var);
      setst_inclusion(e,pat);
      return proj_var;
    }
}

static void setif_con_match(gen_e e1, gen_e e2)
{
  // Case where e1 is a constructor expression and e2 is a proj_pat
  if (pat_match(((setif_term)e2)->type,((setif_term)e1)->type))
    {
      cons_expr c = (cons_expr)e1;
      proj_pat p = (proj_pat)e2;
      int i = p->i;
      
      if (c->sig[i].variance == vnc_pos)
	call_inclusion_fn(c->exps[i],p->exp);
      else if (c->sig[i].variance == vnc_neg)
	call_inclusion_fn(p->exp,c->exps[i]);
      else
	call_unify_fn(c->exps[i],p->exp);
    }
  else if (setif_is_pat(e2)) 
    {
      return;
    }
  
  // Case where e1 and e2 are constructor expressions
  else 
    {
      cons_expr c1 = (cons_expr)e1,
	c2 = (cons_expr)e2;
      
      if (c1->type != c2->type)
	failure("Constructor mismatch\n");
      else
	{
	  int i;
	  for (i = 0; i < c1->arity; i++)
	    {
	      if (c1->sig[i].variance == vnc_pos)
		call_inclusion_fn(e1,e2);
	      else if (c1->sig[i].variance == vnc_neg)
		call_inclusion_fn(e2,e1);
	      else
		call_unify_fn(e1,e2);
	    }
	  
	}
    } 
}

static void setst_con_match(gen_e e1, gen_e e2)
{
  // Case where e1 is a constructor expression and e2 is a proj_pat
  if (pat_match(((setst_term)e2)->type,((setst_term)e1)->type))
    {
      cons_expr c = (cons_expr)e1;
      proj_pat p = (proj_pat)e2;
      int i = p->i;
      
      if (c->sig[i].variance == vnc_pos)
	call_inclusion_fn(c->exps[i],p->exp);
      else if (c->sig[i].variance == vnc_neg)
	call_inclusion_fn(p->exp,c->exps[i]);
      else
	call_unify_fn(c->exps[i],p->exp);
    }
  else if (setst_is_pat(e2)) 
    {
      return;
    }
  
  // Case where e1 and e2 are constructor expressions
  else 
    {
      cons_expr c1 = (cons_expr)e1,
	c2 = (cons_expr)e2;
      
      if (c1->type != c2->type)
	failure("Constructor mismatch\n");
      else
	{
	  int i;
	  for (i = 0; i < c1->arity; i++)
	    {
	      if (c1->sig[i].variance == vnc_pos)
		call_inclusion_fn(e1,e2);
	      else if (c1->sig[i].variance == vnc_neg)
		call_inclusion_fn(e2,e1);
	      else
		call_unify_fn(e1,e2);
	    }
	  
	}
    } 
}

// given x <= proj(c,i,e)
// proj_merge(region,e,get_proj_i_arg,fresh_large_fn_ptr,
// sort_inclusion_fn_ptr,set_inclusion)
static bool nonspec_res_proj(set_var v1,gen_e e2)
{
  proj_pat projection_pat = (proj_pat)e2;
  
  gen_e setif_get_proj(gen_e_list arg1)
    {
      gen_e_list_scanner scan;
      gen_e temp;
      proj_pat pat;

      gen_e_list_scan(arg1,&scan);
      while(gen_e_list_next(&scan,&temp))
	{
	  pat = (proj_pat)temp;
	  if ( pat->type == ((setif_term)e2)->type && 
	       pat->i == ((proj_pat)e2)->i)
	    return pat->exp;
	}
      return NULL;
    }
  
  gen_e fresh_large(void)
    {
      return get_proj_var( ((proj_pat)e2)->exp->sort,TRUE);
    }
  
  bool sort_inclusion(gen_e e1, gen_e e2)
    {
      if ( projection_pat->variance == vnc_pos )
	return call_inclusion_fn(e1,e2);
      else if ( projection_pat->variance == vnc_neg)
	return call_inclusion_fn(e2,e1);
      else 
	return call_unify_fn(e1,e2);
    }
  
  gen_e proj_con(gen_e e)
    {
      return make_proj_pat( ((proj_pat)e2)->c, ((proj_pat)e2)->i,e);
    }
  
  return setif_proj_merge(setif_region,v1,((proj_pat)e2)->exp,
			  setif_get_proj,proj_con,
			  fresh_large,sort_inclusion,
			  call_setif_inclusion);
  
}


void call_setif_inclusion(gen_e e1,gen_e e2)
{
  setif_inclusion(setif_con_match,setif_res_proj,e1,e2);
}

void call_setif_unify(gen_e e1, gen_e e2)
{
  setif_inclusion(setif_con_match,setif_res_proj,e1,e2);
  setif_inclusion(setif_con_match,setif_res_proj,e2,e1);
}

void call_setst_inclusion(gen_e e1, gen_e e2)
{
  setst_inclusion(setst_con_match,e1,e2);
}

void call_setst_unify(gen_e e1, gen_e e2)
{
  setst_inclusion(setst_con_match,e1,e2);
  setst_inclusion(setst_con_match,e2,e1);
}

void call_flowrow_inclusion(gen_e e1,gen_e e2)
{

  if ( (e1->sort != flowrow_sort) || (e2->sort != flowrow_sort) )
    failure("Constraint system is not well-sorted\n");

  if ( flowrow_base_sort(e1) != flowrow_base_sert(e2))
    failure("Constraint system is not well-sorted\n");


  flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e1,e2);
}

void call_flowrow_unify(gen_e e1, gen_e e2)
{
  flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e1,e2);
  flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e2,e1);
}

static void term_con_match(gen_e e1, gen_e e2)
{
  cons_expr c1 = (cons_expr)e1,
    c2 = (cons_expr)e2;
  
  if (c1->type != c2->type)
    failure("Constructor mismatch\n");
  else
    {
      int i;
      for (i = 0; i < c1->arity; i++)
	{
	  call_unify_fn(e1,e2);
	}
      
    }
}

static void term_occurs(term_var v, gen_e e)
{
  gen_e ecr = term_get_ecr(e);
  
  if (((gen_term)ecr)->type == VAR_TYPE)
    return ( term_get_stamp((gen_e)v) == term_get_stamp(e) );

  else if (((gen_term)ecr)->type >= 10)
    {
      cons_expr c_e = (cons_expr) e;
      int i;
      for (int i = 0; i < arity; i++)
	{
	  if (term_occurs(v,c->exps[i]))
	    return TRUE;
	}
    }
  
  return FALSE;
}

void call_term_unify(gen_e e1, gen_e e2)
{
  term_unify(term_con_match,term_occurs,e1,e2);
}

void call_term_cunify(gen_e e1, gen_e e2)
{
  term_cunify(term_con_match,term_occurs,e1,e2);
}


static void call_inclusion_fn(gen_e e1, gen_e e2)
{
  switch (e1->sort)
    {
    case sort_setif:
      {
	setif_inclusion(setif_con_match,setif_res_proj,e1,e2);
      }
      break;
    case sort_setst:
      {
	setst_inclusion(setst_con_match,e1,e2);
      }
      break;
    case sort_term:
      {
	term_unify(term_con_match,term_occurs,e1,e2);
      }    
      break;
    case sort_row:
      {
	/* TODO */
	flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e1,e2);
      }
      break;
    default :
      fail("Unmatched sort in call inclusion\n");
    }
}

static bool call_unify_fn(gen_e e1, gen_e e2)
{

  switch (e1->sort)
    {
    case sort_setif:
      {
	setif_inclusion(setif_con_match,setif_res_proj,e1,e2);
	setif_inclusion(setif_con_match,setif_res_proj,e2,e1);
      }
      break;
    case sort_setst:
      {
	setst_inclusion(setst_con_match,e1,e2);
	setst_inclusion(setst_con_match,e2,e1);
      }
      break;
    case sort_term:
      {
	term_unify(term_con_match,term_occurs,e1,e2);
      }    
      break;
    case sort_row:
      {
	/* TODO */
	flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e1,e2);
	flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e2,e1);
      }
      break;
    default :
      fail("Unmatched sort in call inclusion\n");
    }
}

void nonspec_init(void)
{
  banshee_init();
  setif_init();
  setst_init();
  flowrow_init();
}

void nonspec_reset(void)
{
  flowrow_reset();
  setst_reset();
  setif_reset();
  banshee_reset();
}

void expr_print(FILE *f,gen_e e)
{

}
