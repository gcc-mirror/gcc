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
#include "stamp.h"
#include "util.h"
#include "hash.h"
#include "list.h"
#define INITIAL_SIZE 32
#define INITIAL1 0
#define INITIAL2 2000
#define INITIAL3 536870911
#define MIN -1073741824 /* -2^30 */
#define MAX  1073741824 /* 2^30 */

static hash_table str_hash;
static region str_hash_rgn;

static int count1 = INITIAL1, count2 = INITIAL2, count3 = INITIAL3;
static int bounds1 = MIN, bounds2 = 536870911, bounds3 = MAX;

static inline stamp check1(int i)
{
  if (i <= bounds1)
    fail ("Unable to create stamp with small index\n");
  return i;
}

static inline stamp check2(int i)
{
  if (i > bounds2)
    fail ("Unable to create a stamp with regular index\n");
  return i;
}

static inline stamp check3(int i)
{
  if (i >= bounds3)
    fail ("Unable to create a stamp with large index\n");
  return i;
}

stamp stamp_fresh(void)
{
  return (check2(++count2));
}

stamp stamp_fresh_small(void)
{
  return (check1(--count1));
}

stamp stamp_fresh_large(void)
{
  return (check3(++count3));
}

stamp stamp_string(const char *str) deletes
{
  long st;
  assert(str_hash != NULL);

  if (! hash_table_lookup(str_hash,(hash_key)str, (void *)(char *) &st))
    {
      st = stamp_fresh();
      (void)hash_table_insert(str_hash,(hash_key)str,(hash_data) st);
    }
  return st;
}

void stamp_reset(void) deletes
{
  count1 = INITIAL1;
  count2 = INITIAL2;
  count3 = INITIAL3;
  hash_table_reset(str_hash);
  deleteregion_ptr(&str_hash_rgn);
}



void stamp_init(void)
{
  str_hash_rgn = newregion();
  str_hash = make_string_hash_table(str_hash_rgn,INITIAL_SIZE,FALSE);

}
#if 0
const char *stamp_to_str(region r,stamp st)
{
  return inttostr(r,st);
}
#endif
