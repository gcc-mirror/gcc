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
#include "ufind.h"
#include "assert.h"


enum uf_type {uf_ecr,uf_link};
typedef enum uf_type uf_type;

struct uf_element
{
  uf_type type;
  int rank;
  void *info;
  struct uf_element *link;
};

struct uf_element *new_uf_element(region r, void *info)
{
  struct uf_element *result;

  result = ralloc(r, struct uf_element);

  result->type = uf_ecr;
  result->rank = 0;
  result->info = info;
  result->link = NULL;

  return result;
}

static struct uf_element *find(struct uf_element *e)
{

  if (e->type == uf_ecr)
    return e;
  
  else if (e->link->type == uf_link)
    {
      struct uf_element *temp = e->link;
	
      e->link = e->link->link;

      return find(temp);
    }

  else
    return e->link;
}

bool uf_union(struct uf_element *a, struct uf_element *b)
{
  struct uf_element *e1 = find(a);
  struct uf_element *e2 = find(b);

  if ( e1 == e2 )
    return FALSE;

 else if (e1->rank < e2->rank)
    {
      e1->type = uf_link;
      e1->link = e2;

      return TRUE;
    }

  else if (e1->rank > e2->rank)
    {
      e2->type = uf_link;
      e2->link = e1;

      return TRUE;
    }
  
  else 
    {
      e2->rank++;
      
      e1->type = uf_link;
      e1->link = e2;

      return TRUE;
    }

}

bool uf_unify(combine_fn_ptr combine,
	      struct uf_element *a, struct uf_element *b)
{
  struct uf_element *e1 = find(a);
  struct uf_element *e2 = find(b);

  if ( e1 == e2 )
    return FALSE;

  else if (e1->rank < e2->rank)
    {
      e2->info = combine(e2->info,e1->info);
      e1->type = uf_link;
      e1->link = e2;
     
      return TRUE;
    }

  else if (e1->rank > e2->rank)
    {
      e1->info = combine(e1->info,e2->info);
      e2->type = uf_link;
      e2->link = e1;

      return TRUE;
    }
  
  else 
    {
      e2->info = combine(e2->info, e1->info);

      e2->rank++;
      e1->type = uf_link;
      e1->link = e2;

      return TRUE;
    }
}



void *uf_get_info(struct uf_element *e)
{
  return find(e)->info;
}


bool uf_eq(struct uf_element *e1,struct uf_element *e2)
{
  return (find(e1) == find(e2));
}

void uf_update(struct uf_element *e,uf_info i)
{
  find(e)->info = i;
}






