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

#include <stdlib.h>
#include <assert.h>
#include "bounds.h"

struct bounds
{
  hash_set set;
  gen_e_list elems;
};

bounds bounds_create(region r)
{
  bounds result;
  
  result = ralloc(r, struct bounds);
  result->set = hs_create(r);
  result->elems = new_gen_e_list(r);

  return result;
}

gen_e_list bounds_exprs(bounds b)
{
  return b->elems;
}

bool bounds_add(bounds b, gen_e e, stamp s)
{
  if (hs_member(b->set, s))	
    return TRUE;
  
  else
    {
      gen_e_list_cons(e,b->elems);
      return FALSE;
    }
}

bool bounds_empty(bounds b)
{
  return (gen_e_list_empty(b->elems));
}

bool bounds_query(bounds b, stamp x)
{
  return (hs_query(b->set, x));
}

void bounds_set(bounds b,gen_e_list l)
{
  b->elems = l;
}

void bounds_delete(bounds b)
{
  hs_delete(b->set);
}


