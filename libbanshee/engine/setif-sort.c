/*
 * Copyright (c) 2000-2004
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

#include <assert.h>
#include <setjmp.h>
#include "regions.h"
#include "bounds.h"
#include "jcollection.h"
#include "setif-sort.h"
#include "util.h"

bool flag_eliminate_cycles = TRUE;
bool flag_merge_projections = TRUE;

struct setif_union_ /* extends gen_e */
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
  gen_e_list exprs;
  gen_e_list proj_cache;
};

struct setif_inter_ /* extends gen_e */
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
  gen_e_list exprs;
};

struct setif_constant_ /* extends gen_e */
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
  char *name;
};

typedef struct setif_inter_ *setif_inter_;
typedef struct setif_union_ *setif_union_;
typedef struct setif_constant_ *setif_constant_;

static setif_var_list setif_vars;
static region tlb_cache_region;
static setif_var_list tlb_var_cache;
static jcoll_dict tlb_dict;


region setif_region;
term_hash setif_hash;
struct setif_stats setif_stats;

stamp setif_get_stamp(gen_e e) 
{
#ifdef NONSPEC
  assert(e->sort == setif_sort);
#endif
  
  if ( ((setif_term)e)->type == VAR_TYPE)
    return sv_get_stamp( (setif_var)e );
  
  else
    return ((setif_term)e)->st;
}

static void tlv_lower_aux(jmp_buf buf,stamp st, gen_e e)
{
  if ( setif_is_var(e) && (setif_get_stamp(e) > st) ) 
    longjmp(buf,1);
  
  else if (setif_is_union(e))
    {
      gen_e temp;
      gen_e_list exprs = ((setif_union_)e)->exprs;
      gen_e_list_scanner scan;
      
      gen_e_list_scan(exprs,&scan);
      while (gen_e_list_next(&scan,&temp))
	tlv_lower_aux(buf,st,temp);
    }
}

static bool tlv_lower(stamp st, gen_e e)
{
  jmp_buf buf;
  int higher;
  
  higher = setjmp(buf);
  if (higher)
    return FALSE;

  tlv_lower_aux(buf,st,e);
  
  return TRUE;
}

static void invalidate_tlb_cache(void) deletes
{
  assert(tlb_cache_region);

  setif_var_list_app(tlb_var_cache,sv_clear_tlb_cache);
  jcoll_delete_dict(tlb_dict);
  deleteregion_ptr(&tlb_cache_region);

  tlb_cache_region = newregion();
  tlb_dict = jcoll_create_dict(tlb_cache_region,setif_get_stamp);
  tlb_var_cache = new_setif_var_list(tlb_cache_region);
}

static void set_tlb_cache(setif_var v, jcoll j)
{
  setif_var_list_cons(v,tlb_var_cache);
  sv_set_tlb_cache(v,j);
}

/* 
   A constraint e1 <= e2 is l-inductive iff e2 is a variable x and
   for each y in tlv(e1), stamp(y) < stamp(x) 
*/ 
static bool l_inductive(gen_e e1, gen_e e2)
{
  if (setif_is_var(e2) && tlv_lower(setif_get_stamp(e2), e1))
    return TRUE;
  
  else return FALSE;
}

/*  
    A constraint e1 <= e2 is r-inductive iff e1 is a variable x and
    for each y in tlv(e2), stamp(y) < stamp(x) 
*/ 
static bool r_inductive(gen_e e1, gen_e e2)
{
  if (setif_is_var(e1) && tlv_lower(setif_get_stamp(e1), e2))
    return TRUE;
  
  else return FALSE;
}

static bool eq(gen_e e1, gen_e e2)
{
  return ( setif_get_stamp(e1) == setif_get_stamp(e2) );
}

gen_e_list setif_get_union(gen_e e)
{
  assert ( ((setif_term)e)->type == UNION_TYPE);

  return ( (setif_union_) e)->exprs;
}

gen_e_list setif_get_inter(gen_e e)
{
  assert ( ((setif_term)e)->type == INTER_TYPE);

  return ( (setif_inter_) e)->exprs;
}

static setif_var_list search_ubs(region r, setif_var v1, setif_var goal)
{
  bool found = FALSE;
  setif_var_list cycle;
  
  static void search_ubs_aux(setif_var v)
    {
      assert(! found); 
      
      if (sv_eq (v, goal))
	{
	  found = TRUE;
	  return;
	}
      else if (sv_lt(v,goal))
	{
	  return;
	}
      else 
	{
	  gen_e_list_scanner scan;
	  gen_e ub;
	  gen_e_list ubs = sv_get_ubs(v);
	  
	  gen_e_list_scan(ubs,&scan);
	  while (gen_e_list_next(&scan,&ub))
	  {
	    if (setif_is_var(ub))
	      {
		search_ubs_aux((setif_var)ub);
		if (found)
		  {
		    setif_var_list_cons(v,cycle);
		    return;
		  }
	      }
	  }
	}
    }

  found = FALSE;
  cycle = new_setif_var_list(r);
  search_ubs_aux(v1);

  return cycle;
}

static setif_var_list search_lbs(region r, setif_var v1, setif_var goal)
{
  bool found;
  setif_var_list cycle;
 
  static void search_lbs_aux(setif_var v)
    {
      assert (! found);
      if (sv_eq(v,goal))
	{
	  found = TRUE;
	  return;
	}
      else if (sv_lt(v,goal))
	{
	  return;
	}
      else
	{
	  gen_e_list_scanner scan;
	  gen_e lb;
	  gen_e_list lbs = sv_get_lbs(v);
	  
	  gen_e_list_scan(lbs,&scan);
	  while (gen_e_list_next(&scan,&lb))
	  {
	    if (setif_is_var(lb))
	      {
		search_lbs_aux((setif_var)lb);
		if (found)
		  {
		    setif_var_list_cons(v,cycle);
		    return;
		  }
	      }
	  }
	}
	
    }

  found = FALSE;
  cycle = new_setif_var_list(r);
  search_lbs_aux(v1);

  return cycle; 
}

static setif_var_list cycle_detect(region r,setif_var v1,setif_var v2)
{
  if (sv_union_component(v1,v2))
    return new_setif_var_list(r);

  else
    {
      setif_stats.cycles_searched_forward++;
      return search_ubs(r, v2, v1);
    }
}


static setif_var_list cycle_detect_rev(region r, setif_var v1, setif_var v2)
{
  if (sv_union_component(v1,v2))
    return new_setif_var_list(r);

  else
    {
      setif_stats.cycles_searched_backward++;
      return search_lbs(r, v1, v2);
    }
}

void setif_inclusion(con_match_fn_ptr con_match, res_proj_fn_ptr res_proj, 
		     gen_e e1, gen_e e2) deletes
{

  static void collapse_cycle_lower(region r, setif_var witness, 
			    setif_var_list cycle) deletes
    {
      gen_e lb;
      gen_e_list_scanner scan_bounds;
      setif_var_list_scanner scan_cycle;
      setif_var var;

#ifndef NDEBUG
      stamp lowest = sv_get_stamp(witness);
#endif
      bounds b = bounds_create(r);
      
      /* Collect all lower bounds in the cycle, and add transitive edges */
      setif_var_list_scan(cycle,&scan_cycle);
      while(setif_var_list_next(&scan_cycle,&var))
	{
	  assert( sv_get_stamp(var) > lowest);
	  gen_e_list_scan(sv_get_lbs(var),&scan_bounds);
	  while(gen_e_list_next(&scan_bounds,&lb))
	    bounds_add(b,lb,setif_get_stamp(lb));
	}

      sv_unify(witness,cycle);
      assert(sv_get_stamp(witness) == lowest);
      
      gen_e_list_scan(bounds_exprs(b),&scan_bounds);
      while (gen_e_list_next(&scan_bounds,&lb))
	setif_inclusion(con_match,res_proj,lb, (gen_e) witness);
      
      bounds_delete(b);
      invalidate_tlb_cache();

      setif_stats.cycles_collapsed_backward++;
      setif_stats.cycles_length_backward += setif_var_list_length(cycle);
    }
  
  static void collapse_cycle_upper(region r, setif_var witness,
			    setif_var_list cycle) deletes
    {
      gen_e ub;
      gen_e_list_scanner scan_bounds;
      setif_var_list_scanner scan_cycle;
      setif_var var;

#ifndef NDEBUG
      stamp lowest = sv_get_stamp(witness);
#endif
      bounds b = bounds_create(r);
     
      /* Collect all upper bounds in the cycle, and add transitive edges */
      setif_var_list_scan(cycle,&scan_cycle);
      while(setif_var_list_next(&scan_cycle,&var))
	{ 
	  assert( sv_get_stamp(var) > lowest);

	  gen_e_list_scan(sv_get_ubs(var),&scan_bounds);
	  while(gen_e_list_next(&scan_bounds,&ub))
	    bounds_add(b,ub,setif_get_stamp(ub));
	  
	  gen_e_list_scan(sv_get_ub_projs(var),&scan_bounds);
	  while(gen_e_list_next(&scan_bounds,&ub))
	    bounds_add(b,ub,setif_get_stamp(ub));
	}

      sv_unify(witness,cycle);
      assert(sv_get_stamp(witness) == lowest);

      gen_e_list_scan(bounds_exprs(b),&scan_bounds);
      while (gen_e_list_next(&scan_bounds,&ub))
	setif_inclusion(con_match,res_proj,(gen_e) witness, ub);
	
      bounds_delete(b);
      invalidate_tlb_cache();

      setif_stats.cycles_collapsed_forward++;
      setif_stats.cycles_length_backward += setif_var_list_length(cycle);
    }
  
  static void update_lower_bound(setif_var v, gen_e e) deletes
    {
      if (sv_add_lb(v,e,setif_get_stamp(e)))
	{
	  if (setif_is_var(e))
	    setif_stats.redundant_succ++;
	  
	  else
	    setif_stats.redundant_source++;
	}

      else
	{
	  gen_e_list_scanner scan;
	  gen_e ub;
	  
	  if (setif_is_var(e))
	    setif_stats.added_succ++;
	  else
	    setif_stats.added_source++;
	  
	  invalidate_tlb_cache();

	  gen_e_list_scan(sv_get_ubs(v),&scan);
	  while(gen_e_list_next(&scan,&ub))
	    setif_inclusion(con_match,res_proj,e,ub);
	  
	  gen_e_list_scan(sv_get_ub_projs(v),&scan);
	  while (gen_e_list_next(&scan,&ub))
	    setif_inclusion(con_match,res_proj,e,ub);

	}
      
    }

  static void update_upper_bound(setif_var v, gen_e e) deletes
    {
      if (sv_add_ub(v,e,setif_get_stamp(e)))
	{
	  if (setif_is_var(e))
	    setif_stats.redundant_pred++;
	
	  else
	    setif_stats.redundant_sink++;
	}
      
      else
	{
	  gen_e_list_scanner scan;
	  gen_e lb;

	  if (setif_is_var(e))
	    setif_stats.added_pred++;
	  else
	    setif_stats.added_sink++;

	  invalidate_tlb_cache();
	  
	  gen_e_list_scan(sv_get_lbs(v),&scan);
	  while (gen_e_list_next(&scan,&lb))
	    setif_inclusion(con_match,res_proj,lb,e);

	}
      
    }


  if (eq(e1,e2))
    return;
  
  else if ( setif_is_zero(e1) || setif_is_one(e2) )
    return;

  /* c <= d */
  else if ( setif_is_constant(e1) && setif_is_constant(e2) )
    {

  failure("Inconsistent system of constraints\n");
      return;
    }

  else if ( setif_is_union(e1) )
    {
      gen_e_list_scanner scan;
      gen_e temp;

      gen_e_list exprs = setif_get_union(e1);
      
      gen_e_list_scan(exprs,&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  setif_inclusion(con_match,res_proj,temp,e2);
	}

      return;
    }
  
  else if ( setif_is_inter(e2) )
    {
      gen_e_list_scanner scan;
      gen_e temp;

      gen_e_list exprs = setif_get_inter(e2);

      gen_e_list_scan(exprs,&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  setif_inclusion(con_match,res_proj,e1,temp);
	}

      return;
    }

  else if ( l_inductive(e1,e2) ) /* _ <= 'x */ 
    {
      setif_var v2 = ((setif_var)e2);

      if (setif_is_var(e1))
	{
	  setif_var v1 = ((setif_var)e1);
	  
	  if (flag_eliminate_cycles)
	    {
	      region scratch = newregion();
	      setif_var_list cycle = cycle_detect(scratch,v1,v2);
	      
	      if (! setif_var_list_empty(cycle))
		collapse_cycle_upper(scratch,v1,cycle);
	      else
		update_lower_bound(v2,e1);
	      
	      deleteregion(scratch);
	    }
	  
	  else 
	    update_lower_bound(v2,e1);
	}
      else /* e1 is a source */
	update_lower_bound(v2,e1);
    }

  else if ( r_inductive(e1,e2) ) /* 'x <= _ */
    {
      setif_var v1 = ((setif_var)e1);
     
      if (setif_is_var(e2))
	{
	  setif_var v2 = ((setif_var)e2);
	  
	  if (flag_eliminate_cycles)
	    {
	      region scratch = newregion();
	      setif_var_list cycle = cycle_detect_rev(scratch,v1,v2);
	      
	      if (! setif_var_list_empty(cycle))
		collapse_cycle_lower(scratch,v2,cycle);
	      else
		update_upper_bound(v1,e2);
	      
	      deleteregion(scratch);
	    }
      
	  else
	    update_upper_bound(v1,e2);
	}
      else /* e2 is a sink */
	{
	  if (flag_merge_projections && res_proj(v1,e2))
	    return;
	  else
	    update_upper_bound(v1,e2);
	}
    }

  else /* c(...) <= c(...) or c(...) <= projpat(c,i,e) */
  {
    con_match(e1,e2);
    return;
  }
  
}

#ifdef NONSPEC
static struct setif_term zero = {setif_sort,ZERO_TYPE,ZERO_TYPE}; 
static struct setif_term one  = {setif_sort,ONE_TYPE,ONE_TYPE};
#else
static struct setif_term zero = {ZERO_TYPE,ZERO_TYPE};
static struct setif_term one  = {ONE_TYPE,ONE_TYPE};
#endif /* NONSPEC */

gen_e setif_zero(void)
{
  return (gen_e)&zero;
}

gen_e setif_one(void)
{
  return (gen_e)&one;
}

gen_e setif_fresh(const char *name)
{
  setif_var result = sv_fresh(setif_region,name);
  setif_var_list_cons(result,setif_vars);

  setif_stats.fresh++;
  return (gen_e)result;
}

gen_e setif_fresh_large(const char *name)
{
  setif_var result = sv_fresh_large(setif_region,name);
  setif_var_list_cons(result,setif_vars);

  setif_stats.fresh_large++;
  return (gen_e)result;
}

gen_e setif_fresh_small(const char *name)
{
  setif_var result = sv_fresh_small(setif_region,name);
  setif_var_list_cons(result,setif_vars);

  setif_stats.fresh_small++;
  return (gen_e)result;
}

gen_e setif_constant(const char *str) deletes
{
  stamp st[2];
  gen_e result;
  char *name = rstrdup(setif_region,str);

  assert (str != NULL);
  
  st[0] = CONSTANT_TYPE;
  st[1] = stamp_string(name); 

  if ( (result = term_hash_find(setif_hash,st,2)) == NULL)
    {
      setif_constant_ c = ralloc(setif_region, struct setif_constant_);
#ifdef NONSPEC
      c->sort = setif_sort;
#endif
      c->type = CONSTANT_TYPE;
      c->st = stamp_fresh();
      c->name = name;

      result = (gen_e) c;
      term_hash_insert(setif_hash,result,st,2);
      
      setif_stats.distinct_constants++;
      
      return result;
    }
  
  else
    {
      setif_stats.hashed_constants++;
      return result;
    }
}

static bool filter_zero(const gen_e e)
{
  return (!setif_is_zero(e));
}


static bool filter_one(const gen_e e)
{
  return (!setif_is_one(e));
}

gen_e setif_union(gen_e_list exprs) deletes
{
  gen_e_list filtered = gen_e_list_filter(setif_region,exprs,filter_zero);
  
  if ( gen_e_list_empty(filtered) )
    {
      setif_stats.filtered_unions++;
      return setif_zero();
    }
  else if (gen_e_list_length(filtered) == 1)
    {
      setif_stats.filtered_unions++;
      return gen_e_list_head(filtered);
    }

  else 
    {
      int i = 0;
      gen_e temp,result;
      gen_e_list_scanner scan;
      stamp st[ gen_e_list_length(filtered) + 1 ];
     
      st[0] = UNION_TYPE;

      gen_e_list_scan(filtered,&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  st[++i] = setif_get_stamp(temp);
	}
      
      if ( (result = 
	   term_hash_find(setif_hash,st,gen_e_list_length(filtered)+1)) 
	   == NULL )
	{
	  struct setif_union_ *u = ralloc(setif_region,struct setif_union_);
	  
	  u->type = UNION_TYPE;
	  u->st = stamp_fresh();
	  u->proj_cache = new_gen_e_list(setif_region);
	  u->exprs = filtered;
	 
	  result = (gen_e)u;
	  term_hash_insert(setif_hash,result,st,gen_e_list_length(filtered)+1);
	
	  setif_stats.distinct_unions++;
	  return result;
	}
      else
	{
	  setif_stats.hashed_unions++;
	  return result;
	}
    }
}

gen_e setif_inter(gen_e_list exprs) deletes
{
  gen_e_list filtered = gen_e_list_filter(setif_region,exprs,filter_one);
  
  if ( gen_e_list_empty(filtered) )
    {
      setif_stats.filtered_intersections++;
      return setif_one();
    }
  else if (gen_e_list_length(filtered) == 1)
    {
      setif_stats.filtered_intersections++;
      return gen_e_list_head(filtered);
    }

  else 
    {
      int i = 0;
      gen_e temp,result;
      gen_e_list_scanner scan;
      stamp st[ gen_e_list_length(filtered) + 1 ];
     
      st[0] = INTER_TYPE;

      gen_e_list_scan(filtered,&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  st[++i] = setif_get_stamp(temp);
	}
      
      if ( (result = 
	   term_hash_find(setif_hash,st,gen_e_list_length(filtered)+1)) 
	   == NULL )
	{
	  struct setif_inter_ *u = ralloc(setif_region,struct setif_inter_);
	  
	  u->type = UNION_TYPE;
	  u->st = stamp_fresh();
	  u->exprs = filtered;
	 
	  result = (gen_e)u;
	  term_hash_insert(setif_hash,result,st,gen_e_list_length(filtered)+1);
	  
	  setif_stats.distinct_intersections++;
	  
	  return result;
	}
      else 
	{
	  setif_stats.hashed_intersections++;
	  return result;
	}
    }
}

bool setif_is_zero(gen_e e)
{
  return ((setif_term)e)->type == ZERO_TYPE;
}

bool setif_is_one(gen_e e)
{  
  return ((setif_term)e)->type == ONE_TYPE;
}

bool setif_is_var(gen_e e)
{
  return ((setif_term)e)->type == VAR_TYPE;
}

bool setif_is_union(gen_e e)
{
  return ((setif_term)e)->type == UNION_TYPE;
}

bool setif_is_inter(gen_e e)
{
  return ((setif_term)e)->type == INTER_TYPE;
}

bool setif_is_constant(gen_e e)
{
  return ((setif_term)e)->type == CONSTANT_TYPE;
}

char *setif_get_constant_name(gen_e e)
{
  assert( ((setif_term)e)->type == CONSTANT_TYPE );
  
  return ((setif_constant_)e)->name;
}

void setif_init(void)
{
  setif_region = newregion();
  tlb_cache_region = newregion(); 
  setif_vars = new_setif_var_list(setif_region);
  tlb_var_cache = new_setif_var_list(tlb_cache_region);
  setif_hash = make_term_hash(setif_region);
  tlb_dict = jcoll_create_dict(tlb_cache_region,setif_get_stamp);
}



static void setif_reset_stats(void)
{
  setif_stats.fresh = 0;
  setif_stats.fresh_small = 0;
  setif_stats.fresh_large = 0;

  setif_stats.distinct_constructors = 0;
  setif_stats.hashed_constructors = 0;
  setif_stats.distinct_constants = 0;
  setif_stats.hashed_constants = 0;
  setif_stats.distinct_unions = 0;
  setif_stats.filtered_unions = 0;
  setif_stats.hashed_unions = 0;
  setif_stats.distinct_intersections = 0;
  setif_stats.filtered_intersections = 0;
  setif_stats.hashed_intersections = 0;

  setif_stats.redundant_pred = 0;
  setif_stats.redundant_succ = 0;
  setif_stats.redundant_source = 0;
  setif_stats.redundant_sink = 0;
  
  setif_stats.added_pred = 0;
  setif_stats.added_succ = 0;
  setif_stats.added_source = 0;
  setif_stats.added_sink = 0;
  
  setif_stats.cycles_searched_forward = 0;
  setif_stats.cycles_searched_backward = 0;
  
  setif_stats.cycles_collapsed_forward = 0;
  setif_stats.cycles_collapsed_backward = 0;
  
  setif_stats.cycles_length_forward = 0;
  setif_stats.cycles_length_backward = 0;
}

void setif_reset(void) deletes
{
  term_hash_delete(setif_hash);
  invalidate_tlb_cache();
  deleteregion_ptr(&setif_region);
  deleteregion_ptr(&tlb_cache_region);

  setif_reset_stats();

  setif_region = newregion();
  tlb_cache_region = newregion();
  setif_vars = new_setif_var_list(setif_region);
  tlb_var_cache = new_setif_var_list(tlb_cache_region);
  setif_hash = make_term_hash(setif_region);
}

static jcoll tlb_aux(gen_e e)
{
  if (setif_is_var(e))
    {
      setif_var v = (setif_var)e;

      if ( sv_get_tlb_cache(v) != NULL)
	return sv_get_tlb_cache(v);
      
      else
	{
	  jcoll result;
	  gen_e_list sources = new_gen_e_list(tlb_cache_region);
	  jcoll_list jvars = new_jcoll_list(tlb_cache_region);
	  gen_e_list_scanner scan;
	  gen_e lb;

	  gen_e_list_scan(sv_get_lbs(v),&scan);
	  while (gen_e_list_next(&scan,&lb))
	    {
	      if (setif_is_var(lb))
		jcoll_list_cons(tlb_aux(lb),jvars);
	      else
		gen_e_list_cons(lb,sources);
		/* jsources = jcoll_jcons(tlb_cache_region,lb,jsources); */
	    }

	  if (! gen_e_list_empty(sources))
	   jcoll_list_cons(jcoll_create_chain(tlb_dict,sources),jvars);
	  
	  result =
	    jcoll_jjoin(tlb_dict,jvars);
	  
	  set_tlb_cache(v,result);
	  return result;	
	}
    }
  else if (setif_is_union(e))
    {
      gen_e_list_scanner scan;
      gen_e temp;
      jcoll_list jexprs = new_jcoll_list(tlb_cache_region);
      
      gen_e_list_scan(setif_get_union(e),&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  jcoll_list_cons(tlb_aux(temp),jexprs);
	}

      return jcoll_jjoin(tlb_dict,jexprs);
      
    }
  else
    {
      failure("Unmatched case in setif tlb computation\n");
      return NULL;
    }
}

gen_e_list setif_tlb(gen_e e) deletes
{
  return jcoll_flatten(tlb_dict,tlb_aux(e));
}

void setif_set_proj_cache(gen_e e,gen_e elem)
{
  if (setif_is_union(e))
    {
      setif_union_ u = (setif_union_)e;
      gen_e_list_cons(elem,u->proj_cache);
    }
}

gen_e_list setif_get_proj_cache(gen_e e)
{
  if (setif_is_union(e))
    {
      setif_union_ u = (setif_union_)e;
      return u->proj_cache;
    }
  else
    {
      failure("Term does not cache projections\n");
      return NULL;
    }
}


bool setif_proj_merge(setif_var v, gen_e se, get_proj_fn_ptr get_proj,
		      proj_con_fn_ptr proj_con,fresh_large_fn_ptr fresh_large,
		      incl_fn_ptr sort_incl, incl_fn_ptr set_incl) deletes
{
  gen_e proj;
  
  if ((proj = sv_get_ub_proj(v,get_proj)) != NULL)
    {
      sort_incl(proj, se);
      return TRUE;
    }
  
  else
    {
      gen_e_list_scanner scan;
      gen_e lb;
      
      gen_e proj_var;
      gen_e proj_cons;
      
      /* create a projection variable for this projection */ 
      proj_var = fresh_large(NULL);
      
      assert(setif_is_var(proj_var));

      proj_cons = proj_con(proj_var);

      sv_add_ub_proj(v, proj_cons);
      
      /* apply the transitive rule to each of v's lower bounds */ 
      gen_e_list_scan(sv_get_lbs(v),&scan);
      while (gen_e_list_next(&scan,&lb))
	{
	  set_incl(lb,proj_cons);
	}
	
      sort_incl(proj_var, se);
      return TRUE;
    }

}


void setif_print_stats(FILE *f)
{
  fprintf(f,"\n========== SetIF Var Stats ==========\n");
  fprintf(f,"Fresh : %d\n",setif_stats.fresh); 
  fprintf(f,"Fresh Small : %d\n",setif_stats.fresh_small);
  fprintf(f,"Fresh Large : %d\n",setif_stats.fresh_large);
  fprintf(f,"Total : %d\n",setif_stats.fresh + setif_stats.fresh_small 
	  + setif_stats.fresh_large);
  fprintf(f,"\n========== SetIF Sort Stats ==========\n");
  fprintf(f,"\n");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Additions");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Pred:   %d\n",setif_stats.added_pred);
  fprintf(f,"Succ:   %d\n",setif_stats.added_succ);
  fprintf(f,"Source: %d\n",setif_stats.added_source);
  fprintf(f,"Sink:   %d",setif_stats.added_sink);
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Total:  %d",setif_stats.added_pred + setif_stats.added_succ
	  + setif_stats.added_source + setif_stats.added_sink);
  fprintf(f,"\n");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Redundant");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Pred:   %d\n",setif_stats.redundant_pred);
  fprintf(f,"Succ:   %d\n",setif_stats.redundant_succ);
  fprintf(f,"Source: %d\n",setif_stats.redundant_source);
  fprintf(f,"Sink:   %d",setif_stats.redundant_sink);
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Total:  %d\n",
	 setif_stats.redundant_pred+setif_stats.redundant_succ+setif_stats.redundant_source+setif_stats.redundant_sink);

  fprintf(f,"\n");
  
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Forward Cycles");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Collapsed:      %d\n",setif_stats.cycles_collapsed_forward);
  fprintf(f,"Searched:       %d\n",setif_stats.cycles_searched_forward);
  fprintf(f,"Hit rate:       %f\n",
	 ((float)setif_stats.cycles_collapsed_forward)/((float)setif_stats.cycles_searched_forward));
  fprintf(f,"Average Length: %f\n",
	 1+((float)setif_stats.cycles_length_forward) / ((float)setif_stats.cycles_collapsed_forward));
  fprintf(f,"\n");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Reverse Cycles");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Collapsed:      %d\n",setif_stats.cycles_collapsed_backward);
  fprintf(f,"Searched:       %d\n",setif_stats.cycles_searched_backward);
  fprintf(f,"Hit rate:       %f\n",
	 ((float)setif_stats.cycles_collapsed_backward)/((float)setif_stats.cycles_searched_backward));
  fprintf(f,"Average Length: %f\n",
	 1+((float)setif_stats.cycles_length_backward) / ((float)setif_stats.cycles_collapsed_backward));
  fprintf(f,"=====================================\n");
}

/*
  for now, print stamps and types for sources and sinks.
  must eventually rely on specialized code
*/
void setif_print_constraint_graph(FILE *f)
{ 
  setif_var_list_scanner scan;
  gen_e_list_scanner scan_edges;
  gen_e edge;
  setif_var v;
  dot_node n1,n2;
  char temp_str[512];
    
  graph_attr graph_style[3] = {{g_size,"\"8.5,11\""},
			       {g_center,"true"},
			       {g_orientation,"portrait"}};
  edge_attr succ_edge[1] = {{e_style,"solid"}};
  edge_attr pred_edge[1] = {{e_style,"dotted"}};

   dot_start(f,"setif",TRUE,TRUE);
  dot_global_graph_style(graph_style,3);

  setif_var_list_scan(setif_vars,&scan);
  while(setif_var_list_next(&scan,&v))
    {
      snprintf(temp_str,512,"%s:%ld",sv_get_name(v),sv_get_stamp(v));
      n1 = dot_get_node(temp_str);
      gen_e_list_scan(sv_get_lbs(v),&scan_edges);
      while(gen_e_list_next(&scan_edges,&edge))
	{
	  if (setif_is_var(edge))
	    {
	      snprintf(temp_str,512,"%s:%ld",sv_get_name((setif_var)edge),
		       setif_get_stamp(edge));
	      n2 = dot_get_node(temp_str);
	    }
	  else
	    {
	      snprintf(temp_str,512,"source:%ld",setif_get_stamp(edge));
	      n2 = dot_get_node(temp_str);
	    }
	  dot_styled_edge(n2,n1,pred_edge,1);
	}

      gen_e_list_scan(sv_get_ubs(v),&scan_edges);
      while(gen_e_list_next(&scan_edges,&edge))
	{
	  if (setif_is_var(edge))
	    {
	      snprintf(temp_str,512,"%s:%ld",sv_get_name((setif_var)edge),
		       setif_get_stamp(edge));
	      n2 = dot_get_node(temp_str);
	    }
	  else
	    {
	      snprintf(temp_str,512,"sink:%ld",setif_get_stamp(edge));
	      n2 = dot_get_node(temp_str);
	    }
	  dot_styled_edge(n1,n2,succ_edge,1);
	}

      gen_e_list_scan(sv_get_ub_projs(v),&scan_edges);
      while(gen_e_list_next(&scan_edges,&edge))
	{
	  snprintf(temp_str,512,"projpat:%ld",setif_get_stamp(edge));
	  n2 = dot_get_node(temp_str);
	  dot_styled_edge(n1,n2,succ_edge,1);
	}
      
    }
  
  dot_end();
}

