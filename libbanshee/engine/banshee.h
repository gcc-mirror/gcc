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

#ifndef BANSHEE_H
#define BANSHEE_H

#include <stdio.h>
#include "linkage.h"
#include "stamp.h"
#include "list.h"
#include "util.h"
#include "dot.h"

#define ALIAS_TYPE -2
#define VAR_TYPE -1
#define ZERO_TYPE 0
#define ONE_TYPE 1
#define UNION_TYPE 2
#define INTER_TYPE 3
#define CONSTANT_TYPE 4

EXTERN_C_BEGIN


#ifdef NONSPEC

typedef enum sort_kind
{
  flowrow_sort, 
  setif_sort,
  setst_sort,
  flowterm_sort,
  term_sort
} sort_kind;

typedef struct gen_e
{
  sort_kind sort;
} *gen_e;
#else
typedef void *gen_e;
#endif

DECLARE_LIST(gen_e_list,gen_e)

 typedef void (*gen_e_pr_fn_ptr) (FILE *, gen_e);

/* 
  Function pointers that are common to all sorts
*/

/* inclusion */
typedef void (*incl_fn_ptr) (gen_e, gen_e) deletes;   

/* match constructed terms */  
typedef void (*con_match_fn_ptr) (gen_e, gen_e) deletes;

/* make fresh variables */
typedef gen_e (*fresh_fn_ptr) (const char *);
typedef gen_e (*fresh_small_fn_ptr) (const char *);
typedef gen_e (*fresh_large_fn_ptr) (const char *);     

/* get a stamp */
typedef stamp (*get_stamp_fn_ptr) (gen_e);

/* extract a term from a proj pat */
typedef gen_e (*get_proj_fn_ptr) (gen_e_list);

void engine_init(void);
void engine_reset(void) deletes;
void engine_update(void);
void engine_stats(FILE *f);

void print_constraint_graphs(FILE *f);

EXTERN_C_END

#endif /* BANSHEE_H */
