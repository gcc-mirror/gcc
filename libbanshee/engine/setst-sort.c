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
#include <stdio.h>
#include "bounds.h"
#include "setst-sort.h"


struct setst_union_
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
  gen_e_list exprs;
  gen_e_list proj_cache;
};

struct setst_inter_
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
  gen_e_list exprs;
};

struct setst_constant_
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
  char *name;
};

typedef struct setst_inter_ *setst_inter_;
typedef struct setst_union_ *setst_union_;
typedef struct setst_constant_ *setst_constant_;

static region tlb_cache_region;
static jcoll_dict tlb_dict;
static setst_var_list setst_vars;
static bool setst_changed = FALSE;

region setst_region;
term_hash setst_hash;
struct setst_stats setst_stats;

stamp setst_get_stamp(gen_e e)
{
#ifdef NONSPEC
  assert(e->sort == setst_sort);
#endif
  
  if ( ((setst_term)e)->type == VAR_TYPE)
    return st_get_stamp( (setst_var)e );
  
  else
    return ((setst_term)e)->st;
}

static bool eq(gen_e e1, gen_e e2)
{
  return ( setst_get_stamp(e1) == setst_get_stamp(e2) );
}

static gen_e_list get_union(gen_e e)
{
  assert ( ((setst_term)e)->type == UNION_TYPE);

  return ( (setst_union_) e)->exprs;
}

static gen_e_list get_inter(gen_e e)
{
  assert ( ((setst_term)e)->type == INTER_TYPE);

  return ( (setst_inter_) e)->exprs;
}

static void update_lower_bound(setst_var v, gen_e e)
{      
  if (setst_is_var(e))
    {
      if (st_add_lb(v,(setst_var)e))
	{
	  setst_stats.redundant_var++;
	}
      else
	{
	  setst_stats.added_var++;
	  setst_changed = TRUE;
	}
    }
  else
    {
      if (st_add_source(v, e,setst_get_stamp(e)))
	{
	  setst_stats.redundant_source++;
	}
      else
	{
	  setst_stats.added_source++;
	  setst_changed = TRUE;
	}
    }
  
}

static void update_upper_bound(setst_var v, gen_e e)
{
  assert(! setst_is_var(e));

  if (st_add_sink(v,e,setst_get_stamp(e)))
    {
      setst_stats.redundant_sink++;
    }
  else
    {
      setst_stats.added_sink++;
      setst_changed = TRUE;
    }
}


void setst_inclusion(con_match_fn_ptr con_match,gen_e e1, gen_e e2)
{
  if (eq(e1,e2))
    return;

  else if ( setst_is_zero(e1) || setst_is_one(e2) )
    return;

  else if (setst_is_union(e1))
    {
      gen_e_list_scanner scan;
      gen_e temp;

      gen_e_list exprs = get_union(e1);
      
      gen_e_list_scan(exprs,&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  setst_inclusion(con_match,temp,e2);
	}

      return;
    }

  else if (setst_is_inter(e2))
    {
      gen_e_list_scanner scan;
      gen_e temp;
      
      gen_e_list exprs = get_inter(e2);

      gen_e_list_scan(exprs,&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  setst_inclusion(con_match,e1,temp);
	}

      return;
    }

  else if (setst_is_var(e2))
    {
      setst_var v = (setst_var)e2;

      update_lower_bound(v,e1);
    }

  else if (setst_is_var(e1))
    {
      setst_var v = (setst_var)e1;

      update_upper_bound(v,e2);
    }

  else con_match(e1,e2);
}

#ifdef NONSPEC
static struct setst_term zero = {ZERO_TYPE,setst_sort,ZERO_TYPE};
static struct setst_term one  = {ONE_TYPE,setst_sort,ONE_TYPE};
#else
static struct setst_term zero = {ZERO_TYPE,ZERO_TYPE};
static struct setst_term one  = {ONE_TYPE,ONE_TYPE};
#endif /* NONSPEC */

gen_e setst_zero(void)
{
  return (gen_e)&zero;
}

gen_e setst_one(void)
{
  return (gen_e)&one;
}

gen_e setst_fresh(const char *name)
{
  setst_var v = st_fresh(setst_region,name);
  setst_var_list_cons(v,setst_vars);
  return (gen_e)v;
}

gen_e setst_fresh_large(const char *name)
{
  setst_var v = st_fresh_large(setst_region,name);
  setst_var_list_cons(v,setst_vars);
  return (gen_e)v;
}

gen_e setst_fresh_small(const char *name)
{
  setst_var v = st_fresh_small(setst_region,name);
  setst_var_list_cons(v,setst_vars);
  return (gen_e)v;
}

gen_e setst_constant(const char *str) deletes
{
  stamp st[2];
  gen_e result;
  char *name = rstrdup(setst_region,str);

  assert (str != NULL);
  
  st[0] = CONSTANT_TYPE;
  st[1] = stamp_string(name); 

  if ( (result = term_hash_find(setst_hash,st,2)) == NULL)
    {
      setst_constant_ c = ralloc(setst_region, struct setst_constant_);
      c->type = CONSTANT_TYPE;
      c->st = stamp_fresh();
      c->name = name;

      result = (gen_e) c;
      term_hash_insert(setst_hash,result,st,2);
      
      setst_stats.distinct_constants++;
      
      return result;
    }
  
  else
    {
      setst_stats.hashed_constants++;
      return result;
    }
}

static bool filter_zero(const gen_e e)
{
  return (!setst_is_zero(e));
}


static bool filter_one(const gen_e e)
{
  return (!setst_is_one(e));
}

gen_e setst_union(gen_e_list exprs) deletes
{
  gen_e_list filtered = gen_e_list_filter(setst_region,exprs,filter_zero);
  
  if ( gen_e_list_empty(filtered) )
    {
      setst_stats.filtered_unions++;
      return setst_zero();
    }
  else if (gen_e_list_length(filtered) == 1)
    {
      setst_stats.filtered_unions++;
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
	  st[++i] = setst_get_stamp(temp);
	}
      
      if ( (result = 
	   term_hash_find(setst_hash,st,gen_e_list_length(filtered)+1)) 
	   == NULL )
	{
	  struct setst_union_ *u = ralloc(setst_region,struct setst_union_);
	  
	  u->type = UNION_TYPE;
	  u->st = stamp_fresh();
	  u->proj_cache = new_gen_e_list(setst_region);
	  u->exprs = filtered;
	 
	  result = (gen_e)u;
	  term_hash_insert(setst_hash,result,st,gen_e_list_length(filtered)+1);
	
	  setst_stats.distinct_unions++;
	  return result;
	}
      else
	{
	  setst_stats.hashed_unions++;
	  return result;
	}
    }
}

gen_e setst_inter(gen_e_list exprs) deletes
{
  gen_e_list filtered = gen_e_list_filter(setst_region,exprs,filter_one);
  
  if ( gen_e_list_empty(filtered) )
    {
      setst_stats.filtered_intersections++;
      return setst_one();
    }
  else if (gen_e_list_length(filtered) == 1)
    {
      setst_stats.filtered_intersections++;
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
	  st[++i] = setst_get_stamp(temp);
	}
      
      if ( (result = 
	   term_hash_find(setst_hash,st,gen_e_list_length(filtered)+1)) 
	   == NULL )
	{
	  struct setst_inter_ *u = ralloc(setst_region,struct setst_inter_);
	  
	  u->type = UNION_TYPE;
	  u->st = stamp_fresh();
	  u->exprs = filtered;
	 
	  result = (gen_e)u;
	  term_hash_insert(setst_hash,result,st,gen_e_list_length(filtered)+1);
	  
	  setst_stats.distinct_intersections++;
	  
	  return result;
	}
      else 
	{
	  setst_stats.hashed_intersections++;
	  return result;
	}
    }
}


gen_e_list setst_get_union(gen_e e)
{
  assert (((setst_term)e)->type == UNION_TYPE);

  return ((setst_union_)e)->exprs;
}


gen_e_list setst_get_inter(gen_e e)
{
  assert (((setst_term)e)->type == INTER_TYPE);

  return ((setst_inter_)e)->exprs;
}

static void invalidate_tlb_cache(void)
{
  assert(tlb_cache_region);

  jcoll_delete_dict(tlb_dict);
  setst_var_list_app(setst_vars,st_clear_tlb_cache);
  deleteregion_ptr(&tlb_cache_region);
  
  tlb_cache_region = newregion();
  tlb_dict = jcoll_create_dict(tlb_cache_region,setst_get_stamp);
}

static void set_tlb_cache(setst_var v,jcoll j)
{
  st_set_tlb_cache(v,j);
}

static void collect_sinks(bounds b,setst_var v)
{
  gen_e sink;
  gen_e_list_scanner scan;

  gen_e_list_scan(st_get_sinks(v),&scan);

  while (gen_e_list_next(&scan,&sink))
    {
      bounds_add(b,sink,setst_get_stamp(sink));
    }  
}

static void collect_sources(bounds b, setst_var v)
{
  gen_e source;
  gen_e_list_scanner scan;

  gen_e_list_scan(st_get_sources(v),&scan);

  while (gen_e_list_next(&scan,&source))
    {
      bounds_add(b,source,setst_get_stamp(source));
    }  
}

static void collect_lower_bounds(bounds b, setst_var v)
{
  setst_var lb;
  setst_var_list_scanner scan;

  setst_var_list_scan(st_get_lbs(v),&scan);

  while (setst_var_list_next(&scan,&lb))
    {
      bounds_add(b,(gen_e)lb,st_get_stamp(lb));
    }  
}
 
static void apply_sources(setst_var witness, bounds sources)
{
  gen_e source;
  gen_e_list_scanner scan;

  gen_e_list_scan(bounds_exprs(sources),&scan);

  while (gen_e_list_next(&scan,&source))
    {
      if ( st_add_source(witness,source,setst_get_stamp(source)))
	setst_stats.redundant_source++;
	
      else
	setst_stats.added_source++;
    }  
}

static void apply_sinks(setst_var witness, bounds sinks)
{
  gen_e sink;
  gen_e_list_scanner scan;

  gen_e_list_scan(bounds_exprs(sinks),&scan);

  while (gen_e_list_next(&scan,&sink))
    {
      if (st_add_sink(witness,sink,setst_get_stamp(sink)))
	setst_stats.redundant_sink++;
	
      else
	setst_stats.added_sink++;
    }  
}

static void apply_lower_bounds(setst_var witness,bounds lower)
{
  gen_e lb;
  gen_e_list_scanner scan;

  gen_e_list_scan(bounds_exprs(lower),&scan);

  while (gen_e_list_next(&scan,&lb))
    {
      if (st_add_lb(witness,(setst_var)lb))
	setst_stats.redundant_var++;
      else
	setst_stats.added_var++;
    }  
}

static void collapse_cycle(setst_var witness, setst_var_list cycle) deletes
{
  setst_var_list_scanner var_scan;
  setst_var temp;
  region scratch_rgn = newregion();

  bounds sources = bounds_create(scratch_rgn);
  bounds sinks = bounds_create(scratch_rgn);
  bounds lower = bounds_create(scratch_rgn);
 

  setst_stats.cycles_collapsed++;

  /* force at least another iteration */
  setst_changed = TRUE;

  /* collect all bounds */
  setst_var_list_scan(cycle,&var_scan);
  while (setst_var_list_next(&var_scan,&temp))
    {
      collect_sources(sources,temp);
      collect_sinks(sinks,temp);
      collect_lower_bounds(lower,temp);  
    }
 
  /* unify all vars */
  st_unify(witness,cycle);

  /* add all bounds back */
  apply_sources(witness,sources);
  apply_sinks(witness,sinks);
  apply_lower_bounds(witness,lower);
 
  /* cleanup */
  bounds_delete(sources);
  bounds_delete(sinks);
  bounds_delete(lower);
  deleteregion(scratch_rgn);

  /* remove self edges */
  st_repair_bounds(witness);
}
/*
static bool cycle_detect(setst_var goal, setst_var_list path,
			 setst_var_list *result)
{
  int pos = st_get_path_pos(goal);
  setst_stats.cycles_searched++;
  
  if (pos)
    {  
      setst_var_list_scanner scan;
      setst_var temp;
      setst_var_list cycle = new_setst_var_list(tlb_cache_region);
      
      setst_var_list_scan(path,&scan);
      while(setst_var_list_next(&scan,&temp))
	{
	  if (st_get_path_pos(temp) >= pos)
	    setst_var_list_cons(temp,cycle);
	}
      
      *result = cycle;
      return TRUE;
    }
  
  else 
    return FALSE;
}

*/
static bool cycle_detect(setst_var goal, setst_var_list path,
			 setst_var_list *result)
{
  setst_var_list cycle = 
    setst_var_list_reverse(setst_var_list_copy(tlb_cache_region,path));

  setst_stats.cycles_searched++;

  while (!setst_var_list_empty(cycle) && 
	 !eq((gen_e)setst_var_list_head(cycle),(gen_e)goal))
    {
      setst_var_list_tail(cycle);
    }
  
  if (setst_var_list_empty(cycle))
    {
      return FALSE;
    }
  else
    {
      *result = cycle;
      return TRUE;
    }
}

static jcoll tlb_aux(gen_e e,int path_len,setst_var_list path) deletes
{
  if (setst_is_var(e))
    {
      setst_var_list cycle;
      setst_var v = (setst_var)e;
      if ( cycle_detect(v,path,&cycle) )
	{
	  setst_stats.cycles_length += setst_var_list_length(cycle);
	  collapse_cycle(v,cycle);
	  return NULL;
	}
      else
	{
	  if (st_get_tlb_cache(v) != NULL)
	    return st_get_tlb_cache(v);
	  else 
	    {
	      jcoll result;
	      setst_var_list_scanner scan;
	      setst_var lb;
	      jcoll_list jvars = new_jcoll_list(tlb_cache_region);
	      
	      gen_e_list sources = gen_e_list_copy(tlb_cache_region,
						   st_get_sources(v));
	      
	      st_set_path_pos(v,path_len);
	      setst_var_list_scan(st_get_lbs(v),&scan);
	      while (setst_var_list_next(&scan,&lb))
		{
		  setst_var_list_cons(v,path);
		  jcoll_list_cons(tlb_aux((gen_e)lb,++path_len,path),
				  jvars);
		  setst_var_list_tail(path); 
		}
	      
	      if (! gen_e_list_empty(sources))
		jcoll_list_cons(jcoll_create_chain(tlb_dict,sources),
				jvars);
	      result = jcoll_jjoin(tlb_dict,jvars);
	      set_tlb_cache(v,result);
	      st_set_path_pos(v,0);
	      return result;
	    }
	  
	}
    }
  else if (setst_is_union(e))
    {
      gen_e_list_scanner scan;
      gen_e temp;
      jcoll_list jexprs = new_jcoll_list(tlb_cache_region);
      
      gen_e_list_scan(setst_get_union(e),&scan);
      while (gen_e_list_next(&scan,&temp))
	{
	  jcoll_list_cons(tlb_aux(temp,++path_len,path),jexprs);
	}
      
      return jcoll_jjoin(tlb_dict,jexprs);
    }
  else
    {
      fail("Unmatched case in setst tlb computation\n");
      return NULL;
    }
}
static gen_e_list tlb(gen_e e)
{
  return jcoll_flatten(tlb_dict,
		       tlb_aux(e,1,new_setst_var_list(tlb_cache_region)) );
}
static void match_sinks(incl_fn_ptr setst_incl)
{
  gen_e_list_scanner tlb_scanner, sink_scanner;
  setst_var_list_scanner var_scanner;
  setst_var v;
  gen_e lb, sink;
  
  setst_var_list_scan(setst_vars,&var_scanner);
  
  while (setst_var_list_next(&var_scanner,&v))
    {
      gen_e_list tlbs = tlb((gen_e)v);
      gen_e_list snks = st_get_sinks(v);
      
      
      if(gen_e_list_empty(st_get_sinks(v)))
	{
	  setst_stats.no_sinks++;
	  continue;
	}
      else if(st_get_seen(v))
	{
	  setst_stats.incycle_vars++;
	  continue;
	}
      else if (gen_e_list_length(tlbs) == st_get_src_sz(v) 
	       && gen_e_list_length(snks) == st_get_snk_sz(v) )
	{
	  setst_stats.unchanged_vars++;
	  continue;
	}
      st_set_seen(v,TRUE);
      
      st_set_src_sz(v,gen_e_list_length(tlbs));
      st_set_snk_sz(v,gen_e_list_length(snks));
      
      gen_e_list_scan(tlbs,&tlb_scanner);
      
      while (gen_e_list_next(&tlb_scanner,&lb))
	{
	  gen_e_list_scan(snks,&sink_scanner);
	  
	  while (gen_e_list_next(&sink_scanner,&sink))
	    setst_incl(lb,sink);
	}
    }
}
static void iterate(incl_fn_ptr setst_incl)
{
  setst_var_list_scanner var_scanner;
  setst_var v; 
  /* static int iterations = 0; */
  setst_changed = FALSE;
  
  setst_var_list_scan(setst_vars,&var_scanner);
  while (setst_var_list_next(&var_scanner,&v))
    {
      st_set_seen(v,FALSE);
    }
  
  invalidate_tlb_cache();
  match_sinks(setst_incl);
  
  /*  fprintf(stderr,"Iterations : %d\n",++iterations); */
  
  if (setst_changed)
    iterate(setst_incl);
}  
gen_e_list setst_tlb(gen_e e,incl_fn_ptr setst_incl) deletes
{
  if (! setst_changed)
    {
      return tlb(e);
    }
  else
    {
      iterate(setst_incl);
      return tlb(e);
    }
  
}

void setst_set_proj_cache(gen_e e, gen_e elem)
{
  if (setst_is_union(e))
    {
      setst_union_ u = (setst_union_)e;
      gen_e_list_cons(elem,u->proj_cache);
    }
}

gen_e_list setst_get_proj_cache(gen_e e)
{

  if (setst_is_union(e))
    {
      setst_union_ u = (setst_union_)e;
      return u->proj_cache;
    }
  else
    {
      fail("Term does not cache projections\n");
      return NULL;
    }
}

void setst_init(void)
{
  setst_region = newregion();
  tlb_cache_region = newregion();
  setst_hash = make_term_hash(setst_region);
  setst_vars = new_setst_var_list(setst_region);
  tlb_dict = jcoll_create_dict(tlb_cache_region,setst_get_stamp);
}

void setst_reset(void) deletes
{
  term_hash_delete(setst_hash);
  deleteregion_ptr(&setst_region);

  setst_region = newregion();
  setst_hash = make_term_hash(setst_region);
  setst_vars = new_setst_var_list(setst_region);
  invalidate_tlb_cache();
  setst_changed = FALSE;
}

bool setst_is_zero(gen_e e)
{
  return ((setst_term)e)->type == ZERO_TYPE;
}

bool setst_is_one(gen_e e)
{  
  return ((setst_term)e)->type == ONE_TYPE;
}

bool setst_is_var(gen_e e)
{
  return ((setst_term)e)->type == VAR_TYPE;
}

bool setst_is_union(gen_e e)
{
  return ((setst_term)e)->type == UNION_TYPE;
}

bool setst_is_inter(gen_e e)
{
  return ((setst_term)e)->type == INTER_TYPE;
}

char *setst_get_constant_name(gen_e e)
{
  assert( ((setst_term)e)->type == CONSTANT_TYPE );
  
  return ((setst_constant_)e)->name;
}

void setst_print_stats(FILE *f)
{ 
  fprintf(f,"\n========== SetST Var Stats ==========\n");
  fprintf(f,"Fresh : %d\n",setst_stats.fresh); 
  fprintf(f,"Fresh Small : %d\n",setst_stats.fresh_small);
  fprintf(f,"Fresh Large : %d\n",setst_stats.fresh_large);
  fprintf(f,"Total : %d\n",setst_stats.fresh + setst_stats.fresh_small 
	  + setst_stats.fresh_large);
  fprintf(f,"\n========== SetST Sort Stats ==========\n");
  fprintf(f,"\n");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Additions");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Var:   %d\n",setst_stats.added_var);
  fprintf(f,"Source: %d\n",setst_stats.added_source);
  fprintf(f,"Sink:   %d",setst_stats.added_sink);
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Total:  %d",setst_stats.added_var + setst_stats.added_source
	  + setst_stats.added_sink);
  fprintf(f,"\n");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Redundant");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Var:   %d\n",setst_stats.redundant_var);
  fprintf(f,"Source: %d\n",setst_stats.redundant_source);
  fprintf(f,"Sink:   %d",setst_stats.redundant_sink);
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Total:  %d\n",
	 setst_stats.redundant_var + setst_stats.redundant_source
	  + setst_stats.redundant_sink);
 
  fprintf(f,"\n");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Iteration Optimizations");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Skipped vars:   %d\n",setst_stats.incycle_vars);
  fprintf(f,"Unchanged vars: %d\n",setst_stats.unchanged_vars);
  fprintf(f,"Vars w/o sinks: %d\n",setst_stats.no_sinks);
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Cycles");
  fprintf(f,"\n------------------------------\n");
  fprintf(f,"Collapsed:      %d\n",setst_stats.cycles_collapsed);
  fprintf(f,"Searched:       %d\n",setst_stats.cycles_searched);
  fprintf(f,"Hit rate:       %f\n",
	 ((float)setst_stats.cycles_collapsed)/((float)setst_stats.cycles_searched));
  fprintf(f,"Average Length: %f\n",
	 ((float)setst_stats.cycles_length) / ((float)setst_stats.cycles_collapsed));
  fprintf(f,"=====================================\n");
}

