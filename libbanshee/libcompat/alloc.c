/*
 * Copyright (c) 1999-2001
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
/* TBD: recover unusued portions of pages for use as individual pages */

#include <stddef.h>
#include "regions.h"

static void alloc_block(region r, struct allocator *a, struct ablock *blk,
		 void **p1, int s1, int a1, void **p2, int s2, int a2,
		 size_t blksize, int needsclear)
{
  struct page *newp;
  char *mem1, *mem2;
  
  mem1 = PALIGN(blk->allocfrom, a1);
  mem2 = PALIGN(mem1 + s1, a2);

  /* Can't use last byte of page (pointers to the byte after an object are
     valid) */
  if (mem2 + s2 >= blk->base + blksize)
    {
      if (blksize == RPAGESIZE)
	{
	  newp = alloc_single_page(a->pages);
	  a->pages = newp;
	  blk->allocfrom = (char *)newp + offsetof(struct page, previous);
	  set_region(newp, 1, r);
	}
      else
	{
	  newp = alloc_pages(blksize >> RPAGELOG, a->bigpages);
	  a->bigpages = newp;
	  blk->allocfrom = (char *)newp + offsetof(struct page, previous);
	  set_region(newp, blksize >> RPAGELOG, r);
	}
      blk->base = (char *)newp;

      if (needsclear)
	preclear(blk->allocfrom, blksize - (blk->allocfrom - (char *)newp));
      mem1 = PALIGN(blk->allocfrom, a1);
      mem2 = PALIGN(mem1 + s1, a2);
    }

  ASSERT_INUSE(blk->base, r);
  blk->allocfrom = mem2 + s2;

  *p1 = mem1;
  *p2 = mem2;
}

void qalloc(region r, struct allocator *a, void **p1, int s1, int a1,
	    void **p2, int s2, int a2, int needsclear)
{
  struct page *p;
  char *mem;
  int npages;
  int n = ALIGN(s1, a2) + s2; /* Yes, this is correct (see alloc_block) */

  if (n <= RPAGESIZE / K)
    {
      alloc_block(r, a, &a->page, p1, s1, a1, p2, s2, a2, RPAGESIZE,
		  needsclear);
      return;
    }
  if (n <= RPAGESIZE)
    {
      alloc_block(r, a, &a->superpage, p1, s1, a1, p2, s2, a2,
		  K * RPAGESIZE, needsclear);
      return;
    }
  if (n <= RPAGESIZE * K)
    {
      alloc_block(r, a, &a->hyperpage, p1, s1, a1, p2, s2, a2,
		  K * K * RPAGESIZE, needsclear);
      return;
    }

  npages = (n + ALIGN(offsetof(struct page, previous), a1) + RPAGESIZE - 1)
    >> RPAGELOG;
  p = alloc_pages(npages, a->bigpages);
  a->bigpages = p;
  set_region(p, npages, r);

  mem = (char *)p + offsetof(struct page, previous);
  *p1 = PALIGN(mem, a1);
  *p2 = PALIGN((char *) *p1 + s1, a2);
  if (needsclear)
    preclear(*p2, s2);
}

void free_all_pages(region r, struct allocator *a)
/* Assumes freepages_lock held */
{
  struct page *p, *next;

  for (p = a->pages; p; p = next)
    {
      next = p->next;
      free_single_page(r, p);
    }
  for (p = a->bigpages; p; p = next)
    {
      next = p->next;
      free_pages(r, p);
    }
}
