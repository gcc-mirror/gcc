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

#ifndef TERM_SORT_H
#define TERM_SORT_H

#include <stdio.h>
#include "banshee.h"
#include "termhash.h"
#include "term-var.h"

EXTERN_C_BEGIN

extern bool flag_occurs_check;
extern region term_sort_region;
extern term_hash term_sort_hash;

struct gen_term /* extends gen_e */
{
#ifdef NONSPEC
  const sort_kind sort;
#endif
  const int type;
  const stamp st;
};

typedef struct gen_term *gen_term;

/* return TRUE if v occurs in e, fals otherwise */
typedef bool (* occurs_check_fn_ptr) (term_var v, gen_e e);

stamp term_get_stamp(gen_e e);

gen_e term_fresh(const char *name);
gen_e term_fresh_large(const char *name);
gen_e term_fresh_small(const char *name);
gen_e term_zero(void);
gen_e term_one(void);
gen_e term_constant(const char *name);

bool term_is_zero(gen_e e);
bool term_is_one(gen_e e);
bool term_is_var(gen_e e);
bool term_is_constant(gen_e e);

char *term_get_constant_name(gen_e e);
gen_e term_get_ecr(gen_e e);

void term_unify(con_match_fn_ptr con_match, occurs_check_fn_ptr occurs,
		gen_e e1, gen_e e2);
void term_cunify(con_match_fn_ptr con_match,  occurs_check_fn_ptr occurs,
		 gen_e e1, gen_e e2);

void term_print_stats(FILE *f);
void term_print_constraint_graph(FILE *f);

void term_init(void);
void term_reset(void);

extern struct term_stats term_stats;

struct term_stats
{
  int fresh;
  int fresh_small;
  int fresh_large;
};

EXTERN_C_END

#endif /* TERM_SORT_H */



