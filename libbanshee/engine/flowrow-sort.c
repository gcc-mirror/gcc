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
#include <string.h>
#include <ansidecl.h>
#include "flowrow-sort.h"
#include "termhash.h"

#include "setif-sort.h"

#define ABS_TYPE 2
#define WILD_TYPE 3
#define ROW_TYPE 4

/* generic flow row */
struct flowrow_gen
{
#ifdef NONSPEC
  sort_kind sort;
#endif
  int type;
  stamp st;
#ifdef NONSPEC
  sort_kind base_sort; 
#endif
};

typedef struct flowrow_gen *flowrow_gen;

struct flowrow
{
#ifdef NONSPEC
  sort_kind sort;
#endif 
  int type;		
  stamp st;	
#ifdef NONSPEC
  sort_kind base_sort; 
#endif
  flowrow_map fields;
  gen_e rest;
};

typedef struct flowrow *flowrow;

struct field_split
{
  gen_e_list matched1;
  gen_e_list matched2;
  flowrow_map nomatch1;
  flowrow_map nomatch2;
};

region flowrow_region;
term_hash flowrow_hash;
struct flowrow_stats flowrow_stats;
static void fields_print(FILE *f,flowrow_map m,field_print_fn_ptr field_print) deletes;

stamp flowrow_get_stamp(gen_e e)
{
  if ( ((flowrow_gen)e)->type == ALIAS_TYPE)
    return ((flowrow_gen)fv_get_alias( (flow_var)e ))->st;
  else
    return ((flowrow_gen)e)->st;
  
}

static flowrow_map flowrow_get_fields(gen_e e)
{
  assert (flowrow_is_row(e));
  
  return ((flowrow)e)->fields;
}

static gen_e flowrow_get_rest(gen_e e)
{
  assert(flowrow_is_row(e));

  return ((flowrow)e)->rest;
}


static int field_compare(const flowrow_field f1,const flowrow_field f2)
			
{
  int compare = strcmp(f1->label,f2->label);
  return compare;
}


static int field_compare_ne(const flowrow_field f1,const flowrow_field f2)
			
{
  int compare = strcmp(f1->label,f2->label);

  if (! compare) /* rows should never have two fields with the same labels */
    {
      failure("Multiple fields in this row share the same label\n");
    }
  return compare;
}

static struct field_split split_fields(region r, flowrow_map fields1,
				       flowrow_map fields2)
{
  struct field_split split;
  flowrow_map_scanner scan1, scan2;
  flowrow_field field1,field2;
  bool consumed1 = TRUE,consumed2 = TRUE, 
    fields1_remain = TRUE, fields2_remain = TRUE;;

  split.matched1 = new_gen_e_list(r);
  split.matched2 = new_gen_e_list(r);
  split.nomatch1 = new_flowrow_map(r);
  split.nomatch2 = new_flowrow_map(r);

  flowrow_map_scan(fields1,&scan1);
  flowrow_map_scan(fields2,&scan2);
 
  while (TRUE)
    {
      if (consumed1)
	fields1_remain = flowrow_map_next(&scan1,&field1);
      if (consumed2)
     	fields2_remain = flowrow_map_next(&scan2,&field2);

      if (fields1_remain && fields2_remain)
	{
	  int compare_fields = field_compare(field1,field2);

	  if (compare_fields < 0)
	    {
	      flowrow_map_cons(field1,split.nomatch1);
	      consumed1 = TRUE;
	      consumed2 = FALSE;
	    }
	  else if (compare_fields > 0)
	    {
	      flowrow_map_cons(field2,split.nomatch2);
	      consumed2 = TRUE;
	      consumed1 = FALSE;
	    }
	  else /* two fields are equal */
	    {
	      gen_e_list_cons(field1->expr,split.matched1);
	      gen_e_list_cons(field2->expr,split.matched2);
	      consumed1 = TRUE;
	      consumed2 = TRUE;
	      continue;
	    }
	}
      else if (fields1_remain)
	{
	  /* flowrow_map_append(split.nomatch1,flowrow_map_copy(r,fields1)); */
	  flowrow_map_cons(field1,split.nomatch1);
	  
	  while (flowrow_map_next(&scan1,&field1))
	    {
	      flowrow_map_cons(field1,split.nomatch1);
	    }

	  break;
	}
      else if (fields2_remain)
	{
	  /* flowrow_map_append(split.nomatch2,flowrow_map_copy(r,fields2)); */
	  flowrow_map_cons(field2,split.nomatch2);
	  while (flowrow_map_next(&scan2,&field2))
	    {
	      flowrow_map_cons(field2,split.nomatch2);
	    }
	  break;
	}
      else /* no remaining fields, so */ break;
    }
  
  return split;
}

static bool flowrow_is_normalized(gen_e r)
{
 if ( flowrow_is_row(r) )
    {
      gen_e rest = flowrow_get_rest(r);
      
      if ( flowrow_is_row(rest) || flowrow_is_alias(rest) )
	return FALSE;
    }
 else if ( flowrow_is_alias(r) )
    return FALSE;

  return TRUE;
}

static gen_e normalize(get_stamp_fn_ptr get_stamp,
			 flowrow_map m,gen_e r) deletes
{
  if (flowrow_is_row(r))
    {
      flowrow_map_append(m,
			 flowrow_map_copy(flowrow_region,
					  flowrow_get_fields(r)));
      return normalize(get_stamp,m,flowrow_get_rest(r));
    }
  else if (flowrow_is_alias(r))
    {
      assert (! flowrow_is_alias(fv_get_alias((flow_var)r)) );
      return normalize(get_stamp, m,fv_get_alias((flow_var)r));
    }
  else
    return flowrow_row(get_stamp,m,r);
}

static gen_e normalize_row(get_stamp_fn_ptr get_stamp, gen_e r) deletes
{
  if (flowrow_is_normalized(r))
    return r;
  else /* normalize the row */
    return normalize(get_stamp,new_flowrow_map(flowrow_region),r);
}

static bool eq(gen_e e1, gen_e e2)
{
  return ( flowrow_get_stamp(e1) == flowrow_get_stamp(e2) ); 
}


/*
  A row constraint row1 <= row2 is l-inductive iff row2 is a var and for all 
  X = tlv(row1), o(row2) > o(X). 
  
  tlv(row) = {X} if row is a var X, {} otherwise 
*/
static bool l_inductive(gen_e e1, gen_e e2)
{
  if (flowrow_is_var(e2))
    {
      if (flowrow_is_var(e1))
	return flowrow_get_stamp(e2) > flowrow_get_stamp(e1);
      else return TRUE;
    }
  return FALSE;
}
      
/* 
   A row constraint row1 <= row2 is r-inductive iff row1 is a var and for all
   X = tlv(row2), o(row1) > o(X)
*/
static bool r_inductive(gen_e e1, gen_e e2)
{
  if (flowrow_is_var(e1))
    {
      if (flowrow_is_var(e2))
	return flowrow_get_stamp(e1) > flowrow_get_stamp(e2);
      else return TRUE;
    }
  return FALSE;
}

static inline bool flowrow_minimal(flowrow r)
{
  return flowrow_is_zero(r->rest);
}

static inline bool flowrow_maximal(flowrow r)
{
  return flowrow_is_one(r->rest);
}

static inline bool flowrow_closed(flowrow r)
{
  return flowrow_is_abs(r->rest);
}

static inline bool flowrow_wildcard(flowrow r)
{
  return flowrow_is_wild(r->rest);
}

static inline bool flowrow_var(flowrow r)
{
  return flowrow_is_var(r->rest);
}

static gen_e contour_instantiate(fresh_fn_ptr fresh,
				 get_stamp_fn_ptr get_stamp, 
				 gen_e e) deletes
{
  if (flowrow_is_row(e))
    {
      gen_e result;
      flowrow_map_scanner scan;
      flowrow_field f;
      gen_e row = normalize_row(get_stamp,e);

      region scratch_rgn = newregion();

      flowrow_map new_fields = new_flowrow_map(scratch_rgn);
            
      flowrow_map_scan(flowrow_get_fields(row),&scan);

      while (flowrow_map_next(&scan,&f))
	{
	  flowrow_field new_field =
	    ralloc(flowrow_region,struct flowrow_field);
	  new_field->label = f->label;
	  new_field->expr = fresh(NULL);
	  
	  flowrow_map_cons(new_field,new_fields);
	}
      
      result = flowrow_row(get_stamp,new_fields,flowrow_fresh(NULL));

      deleteregion(scratch_rgn);

      assert( flowrow_is_row(result) );
      
      return result;
    }
  
  else /* TODO */
    {
      failure("Unmatched contour\n");
      return NULL;
    }
}

static contour get_contour(fresh_fn_ptr fresh,get_stamp_fn_ptr get_stamp, 
			   gen_e zero_elem ATTRIBUTE_UNUSED,gen_e e)
{
  if (flowrow_is_row(e))
    {
      contour result;

      result = ralloc(flowrow_region,struct contour);
      result->shape = e;
      result->fresh = fresh;
      result->get_stamp = get_stamp;
      result->instantiate = contour_instantiate;
	  
      return result;
    }
  else /* TODO */
    {
      failure("Unmatched contour\n");
      return NULL;
    }
}
  

static  void trans_lbs(fresh_fn_ptr fresh,get_stamp_fn_ptr get_stamp, 
		       incl_fn_ptr field_incl, gen_e zero_elem,
		       flow_var v, gen_e e) deletes
{
  gen_e temp;
  gen_e_list_scanner scan;
  
  gen_e_list_scan(fv_get_lbs(v),&scan);
  while (gen_e_list_next(&scan,&temp))
      flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,temp,e);
  
}

static  void trans_ubs(fresh_fn_ptr fresh,get_stamp_fn_ptr get_stamp, 
		       incl_fn_ptr field_incl, gen_e zero_elem,
		       flow_var v, gen_e e) deletes
{
  gen_e temp;
  gen_e_list_scanner scan;
  
  gen_e_list_scan(fv_get_ubs(v),&scan);
  while (gen_e_list_next(&scan,&temp))
    flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e,temp);
}

static  void update_lower_bound(fresh_fn_ptr fresh,get_stamp_fn_ptr get_stamp, 
				incl_fn_ptr field_incl, gen_e zero_elem,
				flow_var v,gen_e e) deletes
{
  if (fv_has_contour(v)) /* _ <= v, and v has a contour */
    {
      gen_e shape = fv_instantiate_contour(v);
      
      fv_set_alias(v,shape);
      trans_ubs(fresh,get_stamp,field_incl,zero_elem,v,shape);
      trans_lbs(fresh,get_stamp,field_incl,zero_elem,v,shape);
      
      flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e,shape);
      
    }
  
  else if (flowrow_is_var(e)) 
    {
      flow_var v_lb = (flow_var)e;
      
      if (fv_has_contour(v_lb)) /* v1 <= v2, v1 has a contour */
	{
	  gen_e shape = fv_instantiate_contour(v_lb);
	  
	  fv_set_alias(v_lb,shape);
	  trans_ubs(fresh,get_stamp,field_incl,zero_elem,v_lb,shape);
	  trans_lbs(fresh,get_stamp,field_incl,zero_elem,v_lb,shape);
	  
	  flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,
			    shape,(gen_e)v);
	  
	}
      
      else /* we have v1 <= v2, no contours */
	{
	  bool redundant;
	  
	  fv_unify_contour(v,(flow_var)e);
	  redundant = fv_add_lb(v,e,flowrow_get_stamp(e));
	  
	  if (! redundant)
	    trans_ubs(fresh,get_stamp,field_incl,zero_elem,v,e);
	      
	}
    }
  else /* we have c(...) <= v, and v has no contour */
    {
      gen_e shape = NULL;
      fv_set_contour(v,get_contour(fresh,get_stamp,zero_elem,e));
      
      shape = fv_instantiate_contour(v);
      fv_set_alias(v,shape);
      trans_ubs(fresh,get_stamp,field_incl,zero_elem,v,shape);
      trans_lbs(fresh,get_stamp,field_incl,zero_elem,v,shape);
      
      flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,e,shape);
      
    }
}

static  void update_upper_bound(fresh_fn_ptr fresh,get_stamp_fn_ptr get_stamp, 
				incl_fn_ptr field_incl, gen_e zero_elem,
				flow_var v,gen_e e) deletes
{
  if (fv_has_contour(v)) /* v isn't aliased, and we discovered a contour*/
    {
      gen_e shape = fv_instantiate_contour(v);
      
      fv_set_alias(v,shape);
      trans_ubs(fresh,get_stamp,field_incl,zero_elem,v,shape);
      trans_lbs(fresh,get_stamp,field_incl,zero_elem,v,shape);
      
      flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,shape,e);
	  
    }
  
  else if (flowrow_is_var(e)) 
    {
      flow_var v2 = (flow_var)e;
	  
      if (fv_has_contour(v2)) // v2 isn't aliased, and we discovered a contour
	{
	  gen_e shape = fv_instantiate_contour(v2);
	     
	  fv_set_alias(v2,shape);
	  trans_ubs(fresh,get_stamp,field_incl,zero_elem,v2,shape);
	  trans_lbs(fresh,get_stamp,field_incl,zero_elem,v2,shape);	      


	  flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,
			    (gen_e)v,shape);

	}

      else /* we have v1 <= v2, no contours */
	{
	  bool redundant;
	    
	  fv_unify_contour(v,(flow_var)e);
	  redundant = fv_add_ub(v,e,flowrow_get_stamp(e));

	  if (! redundant)
	    trans_lbs(fresh,get_stamp,field_incl,zero_elem,v,e);
	      
	}
    }
  else /* we have v <= c(...), and v has no contour */
    {
      gen_e shape = NULL;
      fv_set_contour(v,get_contour(fresh,get_stamp,zero_elem,e));
	  
      shape = fv_instantiate_contour(v);

      if (! flowrow_is_row(shape) )
	{
	  assert(0);
	}
	  
      fv_set_alias(v,shape);
      trans_ubs(fresh,get_stamp,field_incl,zero_elem,v,shape);
      trans_lbs(fresh,get_stamp,field_incl,zero_elem,v,shape);
	  
	
      flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,shape,e);
	  
    }

}

// END


void flowrow_inclusion(fresh_fn_ptr fresh,get_stamp_fn_ptr get_stamp, 
		       incl_fn_ptr field_incl, gen_e zero_elem, gen_e a, 
		       gen_e b) deletes
{
  gen_e e1 = normalize_row(get_stamp, a),
    e2 = normalize_row(get_stamp, b);

  if (eq(e1,e2))
    return;
  else if (flowrow_is_zero(e1) || flowrow_is_wild(e1))
    return;
  else if (flowrow_is_one(e2) || flowrow_is_wild(e2))
    return;

  else if ( l_inductive(e1,e2) )
    {
      flow_var v2 = (flow_var)e2;
      
      flowrow_stats.rows_l_inductive++;
      
      update_lower_bound(fresh,get_stamp,field_incl,zero_elem,v2,e1);
      return;
    }
  
  else if ( r_inductive(e1,e2) )
    {
      flow_var v1 = (flow_var)e1;

      flowrow_stats.rows_r_inductive++;

      update_upper_bound(fresh,get_stamp,field_incl,zero_elem,v1,e2);
      return;
    }

  else if ( flowrow_is_row(e1) && flowrow_is_row(e2))
    {
      region scratch_rgn = newregion();
  
      flowrow r1 = (flowrow)e1,
	r2 = (flowrow)e2;

      struct field_split split = 
	split_fields(scratch_rgn,r1->fields,r2->fields);
      
      if ( gen_e_list_empty(split.matched1) ) 
	{
	  assert ( gen_e_list_empty(split.matched2) );

	  if (flowrow_wildcard(r1) || flowrow_minimal(r1))
	    {
	      gen_e newrow = 
		flowrow_row(get_stamp,split.nomatch1,flowrow_get_rest(e1));
		
	      flowrow_inclusion(fresh,get_stamp,field_incl, zero_elem,newrow, 
				flowrow_get_rest(e2));
	    }
	  else if (flowrow_maximal(r2) || flowrow_closed(r2))
	    {
	      gen_e newrow =
		flowrow_row(get_stamp,split.nomatch2,flowrow_get_rest(e2));

	      flowrow_inclusion(fresh, get_stamp,field_incl,zero_elem,
				flowrow_get_rest(e1),newrow);
	    }
	  else 
	    {
	      gen_e rest1 = flowrow_get_rest(e1),
		rest2 = flowrow_get_rest(e2);

	      //assert( flowrow_is_var(rest1) && flowrow_is_var(rest2));

	      if ( eq(rest1,rest2))
		  failure("Recursive row resolution\n");
	      else
		{
		  gen_e fv = flowrow_fresh(NULL);
		  gen_e newrow1 = flowrow_row(get_stamp,split.nomatch1,fv);
		  gen_e newrow2 = flowrow_row(get_stamp,split.nomatch2,fv);
		    
		  flowrow_inclusion(fresh,get_stamp,field_incl,
				    zero_elem,rest1,newrow2);
		  flowrow_inclusion(fresh,get_stamp,field_incl,
				    zero_elem,newrow1,rest2);
		}

	    } 
	}

      else /* some fields matched */
	{
	  gen_e_list_scanner scan1, scan2;
	  gen_e f1,f2;

	  assert( gen_e_list_length(split.matched1) 
		  == gen_e_list_length(split.matched2) );
	  
	  gen_e_list_scan(split.matched1,&scan1);
	  gen_e_list_scan(split.matched2,&scan2);

	  while (gen_e_list_next(&scan1,&f1) &&
		 gen_e_list_next(&scan2,&f2) )
	    {
	      field_incl(f1,f2);
	    }
	  
	  if ( flowrow_wildcard(r1) && flowrow_wildcard(r2) )
	    {
	      goto END;
	    }
	  else
	    {
	      flowrow_map fields1 = split.nomatch1;
	      flowrow_map fields2 = split.nomatch2;
		     
	      gen_e newrow1 = flowrow_row(get_stamp,fields1,r1->rest);
	      gen_e newrow2 = flowrow_row(get_stamp,fields2,r2->rest);
	      
	      flowrow_inclusion(fresh,get_stamp,field_incl,zero_elem,
				newrow1, newrow2);
	    }
	}
    END:
      deleteregion(scratch_rgn);
    }
  
  else /* potentially a problem normalizing a row? */
    {
      failure("Unmatched case in row inclusion\n");
      return;
    }
}

gen_e flowrow_row(get_stamp_fn_ptr get_stamp,flowrow_map f, gen_e rest) deletes
{
  flowrow_map fields = flowrow_map_copy(flowrow_region,f);

  if (flowrow_map_empty(fields))
    {
      return rest;
    }
  else
    { 
      flowrow_map_scanner scan;
      flowrow_field temp;
      gen_e result;
      int i = 2,
	length = flowrow_map_length(fields);
      stamp st[2+2*length];

      st[0] = ROW_TYPE;
      if (rest)
	st[1] = flowrow_get_stamp(rest);
      else
	assert(0);

      flowrow_map_sort(fields,field_compare_ne);
  
      flowrow_map_scan(fields,&scan);
      while(flowrow_map_next(&scan,&temp))
      {
	st[i++] = stamp_string(temp->label);
	if (temp->expr)
	  st[i++] = get_stamp(temp->expr);
	else
	  assert(0);
      }

      if ( (result = term_hash_find(flowrow_hash,st,2 + 2*length)) == NULL)
	{
	  flowrow r = ralloc(flowrow_region, struct flowrow);
	  r->type = ROW_TYPE;
	  r->st = stamp_fresh();
	  r->fields = fields;
	  r->rest = rest;

#ifdef NONSPEC
	  r->base_sort = row_map_head(fields)->expr->sort;
	  r->sort = flowrow_sort;
#endif
	  result = (gen_e) r;
	  term_hash_insert(flowrow_hash,result,st,2+2*length);
	}
      /*  assert(flowrow_is_normalized(result)); */
      return result;

    }
}

#ifndef NONSPEC
static struct flowrow_gen zero_row = {ZERO_TYPE,ZERO_TYPE};
static struct flowrow_gen one_row = {ONE_TYPE,ONE_TYPE};
static struct flowrow_gen abs_row = {ABS_TYPE, ABS_TYPE};
static struct flowrow_gen wild_row = {WILD_TYPE, WILD_TYPE};

gen_e flowrow_zero(void)
{
  return (gen_e)&zero_row;
}

gen_e flowrow_one(void)
{
  return (gen_e)&one_row;
}

gen_e flowrow_abs(void)
{
  return (gen_e)&abs_row;
}

gen_e flowrow_wild(void)
{
  return (gen_e)&wild_row;
}

gen_e flowrow_fresh(const char *name)
{
  flowrow_stats.fresh++;
  return (gen_e)fv_fresh(flowrow_region,name);
}

gen_e flowrow_fresh_small(const char *name)
{
  flowrow_stats.fresh_small++;
  return (gen_e)fv_fresh_small(flowrow_region,name);
}

gen_e flowrow_fresh_large(const char *name)
{
  flowrow_stats.fresh_large++;
  return (gen_e)fv_fresh_large(flowrow_region,name);
}

#else
static struct flowrow_gen term_zero_row = {flowrow_sort,ZERO_TYPE,ZERO_TYPE,term_sort};
static struct flowrow_gen term_one_row = {flowrow_sort,ONE_TYPE,ONE_TYPE,term_sort};
static struct flowrow_gen term_abs_row = {flowrow_sort,ABS_TYPE, ABS_TYPE,term_sort};
static struct flowrow_gen term_wild_row = {flowrow_sort,WILD_TYPE, WILD_TYPE,term_sort};


static struct flowrow_gen setif_zero_row = {flowrow_sort,ZERO_TYPE,ZERO_TYPE,setif_sort};
static struct flowrow_gen setif_one_row = {flowrow_sort,ONE_TYPE,ONE_TYPE,setif_sort};
static struct flowrow_gen setif_abs_row = {flowrow_sort,ABS_TYPE, ABS_TYPE,setif_sort};
static struct flowrow_gen setif_wild_row = {flowrow_sort,WILD_TYPE, WILD_TYPE,setif_sort};

static struct flowrow_gen setst_zero_row = {flowrow_sort,ZERO_TYPE,ZERO_TYPE,setst_sort};
static struct flowrow_gen setst_one_row = {flowrow_sort,ONE_TYPE,ONE_TYPE,setst_sort};
static struct flowrow_gen setst_abs_row = {flowrow_sort,ABS_TYPE, ABS_TYPE,setst_sort};
static struct flowrow_gen setst_wild_row = {flowrow_sort,WILD_TYPE, WILD_TYPE,setst_sort};


gen_e flowrow_zero(sort_kind base_sort)
{
  switch (base_sort)
    {
    case setif_sort:
      return (gen_e)&setif_zero_row;
    case setst_sort:
      return (gen_e)&setst_zero_row;
    case term_sort:
      return (gen_e)&term_zero_row;
    default:
      {
	failure("No matching base sort: flowrow_zero\n");
	return NULL;
      }
    }
  
  return NULL;
}

gen_e flowrow_one(sort_kind base_sort)
{
  switch (base_sort)
    {
    case setif_sort:
      return (gen_e)&setif_one_row;
    case setst_sort:
      return (gen_e)&setst_one_row;
    case term_sort:
      return (gen_e)&term_one_row;
    default:
      {
	failure("No matching base sort: flowrow_one\n");
	return NULL;
      }
    }
  
  return NULL;
}

gen_e flowrow_abs(sort_kind base_sort)
{
  switch (base_sort)
    {
    case setif_sort:
      return (gen_e)&setif_abs_row;
    case setst_sort:
      return (gen_e)&setst_abs_row;
    case term_sort:
      return (gen_e)&term_abs_row;
    default:
      {
	failure("No matching base sort: flowrow_abs\n");
	return NULL;
      }
    }
  
  return NULL;
}

gen_e flowrow_wild(sort_kind base_sort)
{

  switch (base_sort)
    {
    case setif_sort:
      return (gen_e)&setif_wild_row;
    case setst_sort:
      return (gen_e)&setst_wild_row;
    case term_sort:
      return (gen_e)&term_wild_row;
    default:
      {
	failure("No matching base sort: flowrow_wild\n");
	return NULL;
      }
    }
  
  return NULL;
}

gen_e flowrow_fresh(const char *name,sort_kind base_sort)
{

  switch (base_sort)
    {
    case setif_sort:
      return 
    case setst_sort:
      return (gen_e)&setst_one_row;
    case term_sort:
      return (gen_e)&term_one_row;
    default:
      {
	failure("No matching base sort: flowrow_one\n");
	return NULL;
      }
    }
  
  return NULL;
}

gen_e flowrow_fresh_small(sort_kind base_sort)
{

  switch (base_sort)
    {
    case setif_sort:
      return (gen_e)&setif_one_row;
    case setst_sort:
      return (gen_e)&setst_one_row;
    case term_sort:
      return (gen_e)&term_one_row;
    default:
      {
	failure("No matching base sort: flowrow_one\n");
	return NULL;
      }
    }
  
  return NULL;
}

gen_e flowrow_fresh_large(sort_kind base_sort)
{

}

sort_kind flowrow_base_sort(gen_e e)
{

}
#endif /* NONSPEC */


gen_e flowrow_extract_field(const char *name, gen_e e)
{
  
  static bool field_eq(const flowrow_field f)
    {
      return (! strcmp(f->label,name));
    }
  
  if (flowrow_is_row(e))
    {
      flowrow_map fields = flowrow_get_fields(e);
      flowrow_field f = flowrow_map_find(fields,field_eq);

      if (f)
	return f->expr;
    }
  return NULL;
}

gen_e flowrow_extract_rest(gen_e e)
{
  if (flowrow_is_row(e))
    return flowrow_get_rest(e);
  else
    return NULL;
}

flowrow_map flowrow_extract_fields(gen_e e)
{
  if (flowrow_is_row(e))
    return flowrow_map_copy(flowrow_region,flowrow_get_fields(e));
  else
    return NULL;
}


bool flowrow_is_alias(gen_e e)
{
  return ((flowrow_gen)e)->type == ALIAS_TYPE;
}

bool flowrow_is_zero(gen_e e)
{
  return ((flowrow_gen)e)->type == ZERO_TYPE;
}

bool flowrow_is_one(gen_e e)
{
  return ((flowrow_gen)e)->type == ONE_TYPE;
}

bool flowrow_is_abs(gen_e e)
{
  return ((flowrow_gen)e)->type == ABS_TYPE;
}

bool flowrow_is_wild(gen_e e)
{
  return ((flowrow_gen)e)->type == WILD_TYPE;
}

bool flowrow_is_var(gen_e e)
{
  return ((flowrow_gen)e)->type == VAR_TYPE;
}

bool flowrow_is_row(gen_e e)
{
  return ((flowrow_gen)e)->type == ROW_TYPE;
}

void flowrow_init(void)
{
  flowrow_region = newregion();
  flowrow_hash = make_term_hash(flowrow_region);
}

static void flowrow_reset_stats(void)
{
  flowrow_stats.fresh = 0;
  flowrow_stats.fresh_small = 0;
  flowrow_stats.fresh_large = 0;

  flowrow_stats.rows_disjoint_wild = 0;
  flowrow_stats.rows_equal = 0;
  flowrow_stats.rows_zero_one_wild = 0;
  flowrow_stats.rows_l_inductive = 0;
  flowrow_stats.rows_r_inductive = 0;
  flowrow_stats.rows_disjoint_r1_minimal = 0;
  flowrow_stats.rows_disjoint_r1_var_r2_minimal = 0;
  flowrow_stats.rows_disjoint_r1_var_r2_maximal = 0;
  flowrow_stats.rows_disjoint_r1_var_r2_closed = 0;
  flowrow_stats.rows_disjoint_r1_var_r2_var_lt = 0;
  flowrow_stats.rows_disjoint_r1_var_r2_var_gt = 0;
  flowrow_stats.rows_equal_domains = 0;
  flowrow_stats.rows_nonempty_intersection = 0;
  flowrow_stats.rows_fresh = 0;
  flowrow_stats.rows_fresh_large = 0;
}

void flowrow_reset(void) deletes
{
  term_hash_delete(flowrow_hash);
  deleteregion_ptr(&flowrow_region);

  flowrow_reset_stats();

  flowrow_region = newregion();
  flowrow_hash = make_term_hash(flowrow_region);

}

static void fields_print(FILE *f,flowrow_map m,field_print_fn_ptr field_print) deletes
{
  flowrow_map_scanner scan;
  flowrow_field temp;

  flowrow_map_scan(m,&scan);

  if (flowrow_map_next(&scan,&temp))
    {
      fprintf(f,"%s : ",temp->label);
      
      if (field_print)
	field_print(f,temp->expr);
      else
	fprintf(f,"?");
    }

  while (flowrow_map_next(&scan,&temp))
    {
      fprintf(f,",%s : ",temp->label);
     
      if (field_print)
	field_print(f,temp->expr);
      else
 	fprintf(f,"?");
    }
}

void flowrow_print(FILE *f,get_stamp_fn_ptr get_stamp, 
		   field_print_fn_ptr field_print,gen_e row) deletes
{
  gen_e e = normalize_row(get_stamp,row);

  switch ( ((flowrow_gen)e)->type)
    {
    case ZERO_TYPE:
      fprintf(f, "0");
      break;
    case ONE_TYPE:
      fprintf(f, "1");
      break;
    case ABS_TYPE:
      fprintf(f, "abs");
      break;
    case WILD_TYPE:
      fprintf(f, "wild");
      break;
    case VAR_TYPE:
      fprintf(f, fv_get_name((flow_var)e));
      break;
    case ROW_TYPE:
      fprintf(f, "<");
      fields_print(f, flowrow_get_fields(e), field_print);
      fprintf(f, "|");
      flowrow_print(f, get_stamp, field_print, flowrow_get_rest(e));
      fprintf(f, ">");
      break;
    default:
      assert(0);
      break;
    }
}

void flowrow_print_stats(FILE *f)
{
  fprintf(f,"\n========== Flow Var Stats ==========\n");
  fprintf(f,"Fresh : %d\n",flowrow_stats.fresh); 
  fprintf(f,"Fresh Small : %d\n",flowrow_stats.fresh_small);
  fprintf(f,"Fresh Large : %d\n",flowrow_stats.fresh_large);
  fprintf(f,"=====================================\n");
}

DEFINE_LIST(flowrow_map,flowrow_field)
